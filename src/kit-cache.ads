private with Ada.Containers.Doubly_Linked_Lists;
with Marlowe;
with Kit.Locking;

package Kit.Cache is

   type Cache_Entry_Record is
     abstract new Kit.Locking.Root_Lockable_Type with private;

   procedure Write (Item   : Cache_Entry_Record;
                    Index  : Marlowe.Database_Index)
      is abstract;

   overriding procedure U_Lock
     (Item : not null access Cache_Entry_Record);

   overriding procedure S_Lock
     (Item : not null access Cache_Entry_Record);

   overriding procedure Unlock
     (Item : not null access Cache_Entry_Record);

   procedure Initialise (Ent   : in out Cache_Entry_Record'Class;
                         Rec   :        Marlowe.Table_Index;
                         Index :        Marlowe.Database_Index);

   function Get_Table_Index
     (From : Cache_Entry_Record'Class)
     return Marlowe.Table_Index;

   function Get_Index
     (From : Cache_Entry_Record'Class)
     return Marlowe.Database_Index;

   function Image (L : access Cache_Entry_Record) return String;

   type Cache_Entry is access all Cache_Entry_Record'Class;

   procedure Start_Cache;

   procedure Insert   (New_Entry : Cache_Entry);
   function  Retrieve (Rec    : Marlowe.Table_Index;
                       Index  : Marlowe.Database_Index)
                      return Cache_Entry;

   procedure Close;

   procedure Get_Cache_Statistics (Hits   :    out Natural;
                                   Misses :    out Natural);

   procedure Reset_Statistics;

   procedure Set_Max_Cache_Size (Size : Natural);
   --  This must be called before Start_Cache!

   procedure Lock_Cache;
   procedure Unlock_Cache;

private

   type Tick is mod 2**64;

   package List_Of_Cache_Entries is
      new Ada.Containers.Doubly_Linked_Lists (Cache_Entry);

   function Get_Cache_Index
     (From : Cache_Entry)
     return Marlowe.Database_Index;

   type Cache_Entry_Record is
     abstract new Kit.Locking.Root_Lockable_Type with
      record
         Dirty       : Boolean                           := False;
         Last_Access : Tick;
         Rec         : Marlowe.Table_Index;
         Index       : Marlowe.Database_Index;
         LRU         : List_Of_Cache_Entries.Cursor;
         Cached      : Boolean                           := False;
         References  : Natural                           := 0;
      end record;

   pragma Inline (Get_Index);

end Kit.Cache;
