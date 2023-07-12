with Ada.Text_IO;

with Kit.Exceptions;
with Kit.Mutex;

package body Kit.Generic_Cache is

   Debug_Cache : constant Boolean := False;

   Cache_Mutex : Kit.Mutex.Mutex_Type;

   ---------
   -- Get --
   ---------

   function Get
     (Index       : Marlowe.Database_Index;
      Lock_Result : Boolean := False)
      return Cache_Access
   is
      use type Kit.Cache.Cache_Entry;
      Result : Kit.Cache.Cache_Entry;
   begin

      Result := Kit.Cache.Retrieve (Table, Index);

      if Debug_Cache then
         Ada.Text_IO.Put_Line ("cache-get:" & Table'Img & Index'Img);
      end if;

      if Result = null then

         Cache_Mutex.Lock;

         Result := Kit.Cache.Retrieve (Table, Index);

         if Result /= null then
            Cache_Mutex.Unlock;
         end if;
      end if;

      if Result = null then
         declare
            New_Cached_Record : constant Cache_Access :=
                                  new Cache_Record;
         begin
            Kit.Cache.Initialise (New_Cached_Record.all,
                                  Table,
                                  Index);
            Read (Index, New_Cached_Record.Db);

            if False then
               declare
                  Magic : Integer;
                  for Magic'Address use New_Cached_Record.Db'Address;
               begin
                  if Magic /= Table_Magic then
                     Cache.Unlock_Cache;
                     New_Cached_Record.Unlock;
                     raise Kit.Exceptions.Database_Corruption with
                       "table" & Marlowe.Table_Index'Image (Table)
                       & "; index" & Index'Img
                       & ": bad magic number";
                  end if;
               end;
            end if;

            Result := Kit.Cache.Cache_Entry (New_Cached_Record);
            Kit.Cache.Insert (Result);
            Cache_Mutex.Unlock;
         end;
      end if;

      if Lock_Result then
         Result.S_Lock;
      end if;

      return Cache_Access (Result);

   end Get;

   ------------
   -- S_Lock --
   ------------

   procedure S_Lock (Index : Marlowe.Database_Index) is
   begin
      Get (Index).S_Lock;
   end S_Lock;

   ------------
   -- U_Lock --
   ------------

   procedure U_Lock (Index : Marlowe.Database_Index) is
   begin
      Get (Index).U_Lock;
   end U_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Index : Marlowe.Database_Index) is
   begin
      Get (Index).Unlock;
   end Unlock;

   -----------
   -- Write --
   -----------

   overriding
   procedure Write (Item   : Cache_Record;
                    Index  : Marlowe.Database_Index)
   is
   begin
      Write (Index, Item.Db);
   end Write;

   ------------
   -- X_Lock --
   ------------

   procedure X_Lock (Index : Marlowe.Database_Index) is
   begin
      Get (Index).X_Lock;
   end X_Lock;

end Kit.Generic_Cache;
