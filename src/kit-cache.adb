with Ada.Text_IO;

with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;

with Kit.Mutex;

package body Kit.Cache is

   Debug_Locking : constant Boolean := False;
   Max_Cache_Size : Natural := 1_000_000;
   --  Maximum number of objects in the cache
   --  Should, of course, be settable and tunable.

   Max_Table_Index : constant := 256;

   function Database_Index_Hash
     (Index : Marlowe.Database_Index)
      return Ada.Containers.Hash_Type
   is (Ada.Containers.Hash_Type (Index));

   package Cache_Map is
     new Ada.Containers.Hashed_Maps
       (Key_Type        => Marlowe.Database_Index,
        Element_Type    => Cache_Entry,
        Hash            => Database_Index_Hash,
        Equivalent_Keys => Marlowe."=");

   LRU : List_Of_Cache_Entries.List;

--     protected LRU_List is
--        procedure Delete (Position : List_Of_Cache_Entries.Cursor);
--     end LRU_List;

   protected type Local_Cache_Type is
      procedure Insert (New_Entry : Cache_Entry);
      procedure Delete (Item : Cache_Entry);
      function Get (Index  : Marlowe.Database_Index)
                    return Cache_Entry;
      function Full return Boolean;
      pragma Unreferenced (Full);
   private
      Map : Cache_Map.Map;
      Current_Size : Natural := 0;
   end Local_Cache_Type;

   type Local_Cache_Access is access all Local_Cache_Type;

   Local_Cache_Table : array (Marlowe.Table_Index range 1 .. Max_Table_Index)
     of aliased Local_Cache_Type;

   function Get_Local_Cache
     (Table : Marlowe.Table_Index)
      return Local_Cache_Access
   is (Local_Cache_Table (Table)'Access);

   procedure Free is
      new Ada.Unchecked_Deallocation (Cache_Entry_Record'Class,
                                      Cache_Entry);

   procedure Update_LRU (Item : not null access Cache_Entry_Record'Class);
   procedure Reference (Item : not null access Cache_Entry_Record'Class);
   procedure Unreference (Item : not null access Cache_Entry_Record'Class);

   function To_Cache_Index (Rec    : Marlowe.Table_Index;
                            Index  : Marlowe.Database_Index)
                           return Marlowe.Database_Index;

   Cache_Mutex : Mutex.Mutex_Type;
   LRU_Mutex   : Mutex.Mutex_Type;

   Global_Tick : Tick := 0;
   Tick_Mutex  : Mutex.Mutex_Type;

   Cache_Warning_Count : Natural := 0;

   -----------
   -- Close --
   -----------

   procedure Close is
   begin

      Cache_Mutex.Lock;

      --  Entry_Cache.Close (Local_Cache);
      LRU.Clear;

      Cache_Mutex.Unlock;

   end Close;

   -------------------------
   -- Database_Index_Hash --
   -------------------------

--     function Database_Index_Hash
--       (Index : Marlowe.Database_Index)
--        return Ada.Containers.Hash_Type
--     is
--        type Word_64 is mod 2 ** 64;
--        P : constant Word_64 := 16#5555555555555555#;
--        C : constant Word_64 := 17316035218449499591;
--        N : constant Word_64 := Word_64 (Index);
--
--        function Xor_Shift (X : Word_64) return Word_64
--        is (X xor (X / 2 ** 32));
--
--        R : constant Word_64 :=
--          C * Xor_Shift (P * Xor_Shift (N));
--     begin
--        return Ada.Containers.Hash_Type
--          (R mod Ada.Containers.Hash_Type'Modulus);
--     end Database_Index_Hash;

   ---------------------
   -- Get_Cache_Index --
   ---------------------

   function Get_Cache_Index
     (From : Cache_Entry)
     return Marlowe.Database_Index
   is
   begin
      return To_Cache_Index (From.Rec, From.Index);
   end Get_Cache_Index;

   --------------------------
   -- Get_Cache_Statistics --
   --------------------------

   procedure Get_Cache_Statistics (Hits   :    out Natural;
                                   Misses :    out Natural)
   is
   begin
      Hits := 0;
      Misses := 0;
   end Get_Cache_Statistics;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index       (From : Cache_Entry_Record'Class)
                            return Marlowe.Database_Index
   is
   begin
      return From.Index;
   end Get_Index;

   ---------------------
   -- Get_Table_Index --
   ---------------------

   function Get_Table_Index
     (From : Cache_Entry_Record'Class)
     return Marlowe.Table_Index
   is
   begin
      return From.Rec;
   end Get_Table_Index;

   -----------
   -- Image --
   -----------

   function Image (L : access Cache_Entry_Record) return String is
   begin
      return Marlowe.Table_Index'Image (L.Rec) &
        Marlowe.Database_Index'Image (L.Index);
   end Image;

   ----------------
   -- Initialise --
   ----------------

   procedure Initialise (Ent   : in out Cache_Entry_Record'Class;
                         Rec   :        Marlowe.Table_Index;
                         Index :        Marlowe.Database_Index)
   is
   begin
      Ent.Rec   := Rec;
      Ent.Index := Index;
   end Initialise;

   ------------
   -- Insert --
   ------------

   procedure Insert   (New_Entry : Cache_Entry) is
      Deleted_Count : Natural := 0;
      T             : constant Marlowe.Table_Index := New_Entry.Rec;
      Local_Cache   : constant Local_Cache_Access := Get_Local_Cache (T);
   begin

      if Debug_Locking then
         Ada.Text_IO.Put_Line
           ("Insert into cache: table"
            & Marlowe.Table_Index'Image (New_Entry.Rec)
            & " index"
            & Marlowe.Database_Index'Image (New_Entry.Index));
         Ada.Text_IO.Flush;
      end if;

      LRU_Mutex.Lock;

      if False then -- Local_Cache.Full then
         if Cache_Warning_Count mod 100 = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: cache size exceeds configured cache size"
               & (if Cache_Warning_Count > 0
                 then " (suppressed" & Natural'Image (Cache_Warning_Count)
                 & " earlier warnings)"
                 else ""));
         end if;

         declare
            use List_Of_Cache_Entries;
            Position : Cursor := LRU.Last;
         begin
            while Has_Element (Position)
              and then Deleted_Count < Max_Cache_Size / 2
            loop
               declare
                  E    : Cache_Entry := Element (Position);
               begin
                  if E.Is_X_Locked
                    or else E.Is_U_Locked
                    or else E.Is_S_Locked
                    or else E.Dirty
                    or else E.References > 0
                  then
                     --  skip
                     Previous (Position);
                  else

                     if Debug_Locking then
                        Ada.Text_IO.Put_Line
                          ("Dropping from cache: table"
                           & Marlowe.Table_Index'Image (E.Rec)
                           & " index"
                           & Marlowe.Database_Index'Image (E.Index));
                        Ada.Text_IO.Flush;
                     end if;

                     declare
                        Prev : constant Cursor := Previous (Position);
                     begin
                        LRU.Delete (Position);
                        Position := Prev;
                     end;
                     Local_Cache.Delete (E);
                     Free (E);
                     Deleted_Count := Deleted_Count + 1;
                  end if;
               end;
            end loop;
         end;
         if Cache_Warning_Count mod 100 = 0 then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "warning: cache size reduced by"
               & Natural'Image (Deleted_Count));
         end if;
         Cache_Warning_Count := Cache_Warning_Count + 1;
      end if;

      Local_Cache.Insert (New_Entry);

      New_Entry.Cached     := True;
      New_Entry.References := 0;

      LRU.Prepend (New_Entry);
      New_Entry.LRU := LRU.First;
      LRU_Mutex.Unlock;

   end Insert;

   -----------------
   -- Local_Cache --
   -----------------

   protected body Local_Cache_Type is

      ------------
      -- Delete --
      ------------

      procedure Delete (Item : Cache_Entry) is
      begin
         Map.Delete (Item.Index);
         Current_Size := Current_Size - 1;
      end Delete;

      ----------
      -- Full --
      ----------

      function Full return Boolean is
      begin
         return Current_Size >= Max_Cache_Size;
      end Full;

      ---------
      -- Get --
      ---------

      function Get (Index  : Marlowe.Database_Index)
                    return Cache_Entry
      is
         Result  : Cache_Entry := null;
         Position : constant Cache_Map.Cursor := Map.Find (Index);
      begin

         if Cache_Map.Has_Element (Position) then
            Result := Cache_Map.Element (Position);
         end if;

         return Result;

      end Get;

      ------------
      -- Insert --
      ------------

      procedure Insert (New_Entry : Cache_Entry) is
      begin

         Map.Insert (Key      => New_Entry.Index,
                     New_Item => New_Entry);
         Current_Size := Current_Size + 1;

      end Insert;

   end Local_Cache_Type;

   ----------------
   -- Lock_Cache --
   ----------------

   procedure Lock_Cache is
   begin
      Cache_Mutex.Lock;
   end Lock_Cache;

   ---------------
   -- Reference --
   ---------------

   procedure Reference (Item : not null access Cache_Entry_Record'Class) is
   begin
      --  Item.References := Item.References + 1;
      Update_LRU (Item);
   end Reference;

   ----------------------
   -- Reset_Statistics --
   ----------------------

   procedure Reset_Statistics is
   begin
      null;
   end Reset_Statistics;

   --------------
   -- Retrieve --
   --------------

   function  Retrieve (Rec    : Marlowe.Table_Index;
                       Index  : Marlowe.Database_Index)
                      return Cache_Entry
   is
      Local_Cache : constant Local_Cache_Access := Get_Local_Cache (Rec);
      Result      : constant Cache_Entry := Local_Cache.Get (Index);
   begin
      if Result /= null then
         Update_LRU (Result);
      end if;
      return Result;
   end Retrieve;

   ------------
   -- S_Lock --
   ------------

   overriding
   procedure S_Lock (Item : not null access Cache_Entry_Record) is
   begin
      if Debug_Locking then
         Ada.Text_IO.Put_Line ("S_Lock: table"
                               & Marlowe.Table_Index'Image
                                 (Item.Get_Table_Index)
                               & " index"
                               & Marlowe.Database_Index'Image
                                 (Item.Index)
                               & " s = " & Boolean'Image (Item.Is_S_Locked)
                               & " u = " & Boolean'Image (Item.Is_U_Locked)
                               & " x = " & Boolean'Image (Item.Is_X_Locked));
         Ada.Text_IO.Flush;

      end if;

      Locking.Root_Lockable_Type (Item.all).S_Lock;
      Item.Reference;

      Tick_Mutex.Lock;
      Item.Last_Access := Global_Tick;
      Global_Tick   := Global_Tick + 1;
      Tick_Mutex.Unlock;

   end S_Lock;

   ------------------------
   -- Set_Max_Cache_Size --
   ------------------------

   procedure Set_Max_Cache_Size (Size : Natural) is
   begin
      Max_Cache_Size := Size;
   end Set_Max_Cache_Size;

   -----------------
   -- Start_Cache --
   -----------------

   procedure Start_Cache is
   begin
      --  Entry_Cache.Enable_Debug (True);
      --  Local_Cache := Entry_Cache.Create_Cache (Max_Cache_Size);
      null;
   end Start_Cache;

   --------------------
   -- To_Cache_Index --
   --------------------

   function To_Cache_Index (Rec    : Marlowe.Table_Index;
                            Index  : Marlowe.Database_Index)
                           return Marlowe.Database_Index
   is
      use type Marlowe.Database_Index;
   begin
      return Index * Max_Table_Index
        + Marlowe.Database_Index (Rec);
   end To_Cache_Index;

   ------------
   -- U_Lock --
   ------------

   overriding procedure U_Lock
     (Item : not null access Cache_Entry_Record)
   is
   begin
      if Debug_Locking then
         declare
            use type Marlowe.Table_Index;
            use type Marlowe.Database_Index;
         begin
            Ada.Text_IO.Put_Line ("U_Lock: table"
                                  & Marlowe.Table_Index'Image
                                    (Item.Get_Table_Index)
                                  & " index"
                                  & Marlowe.Database_Index'Image
                                    (Item.Index)
                                  & " s = "
                                  & Boolean'Image (Item.Is_S_Locked)
                                  & " u = "
                                  & Boolean'Image (Item.Is_U_Locked)
                                  & " x = "
                                  & Boolean'Image (Item.Is_X_Locked));
            Ada.Text_IO.Flush;
         end;
      end if;
      Locking.Root_Lockable_Type (Item.all).U_Lock;
      Item.Dirty := True;
      Item.Reference;

      Tick_Mutex.Lock;
      Item.Last_Access := Global_Tick;
      Global_Tick   := Global_Tick + 1;
      Tick_Mutex.Unlock;

   end U_Lock;

   ------------
   -- Unlock --
   ------------

   overriding
   procedure Unlock (Item : not null access Cache_Entry_Record) is
   begin
      if Debug_Locking then
         Ada.Text_IO.Put_Line ("Unlock: table"
                               & Marlowe.Table_Index'Image
                                 (Item.Get_Table_Index)
                               & " index"
                               & Marlowe.Database_Index'Image
                                 (Item.Index)
                               & " s = " & Boolean'Image (Item.Is_S_Locked)
                               & " u = " & Boolean'Image (Item.Is_U_Locked)
                               & " x = " & Boolean'Image (Item.Is_X_Locked));
         Ada.Text_IO.Flush;
      end if;
      if Item.Is_X_Locked then
         Item.Dirty := False;
         Cache_Entry_Record'Class (Item.all).Write (Item.Index);
      end if;

      Locking.Root_Lockable_Type (Item.all).Unlock;

      Item.Unreference;

   end Unlock;

   ------------------
   -- Unlock_Cache --
   ------------------

   procedure Unlock_Cache is
   begin
      Cache_Mutex.Unlock;
   end Unlock_Cache;

   -----------------
   -- Unreference --
   -----------------

   procedure Unreference (Item : not null access Cache_Entry_Record'Class) is
   begin
      Update_LRU (Item);
      --  Item.References := Item.References - 1;
   end Unreference;

   ----------------
   -- Update_LRU --
   ----------------

   procedure Update_LRU (Item : not null access Cache_Entry_Record'Class) is
   begin
      if Item.Cached then
         if Debug_Locking then
            Ada.Text_IO.Put_Line
              ("Update_LRU: table"
               & Marlowe.Table_Index'Image (Item.Rec)
               & " index"
               & Marlowe.Database_Index'Image (Item.Index));
            Ada.Text_IO.Flush;
         end if;

         LRU_Mutex.Lock;
         LRU.Delete (Item.LRU);
         LRU.Prepend (Cache_Entry (Item));
         Item.LRU := LRU.First;
         LRU_Mutex.Unlock;

      end if;
   end Update_LRU;

end Kit.Cache;
