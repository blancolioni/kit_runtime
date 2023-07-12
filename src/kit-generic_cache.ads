with Marlowe;
with Kit.Cache;

generic
   Table       : Marlowe.Table_Index;
   Table_Magic : Natural;
   type Database_Record is private;
   with procedure Read (Ref  : Marlowe.Database_Index;
                        Item : out Database_Record);
   with procedure Write (Ref  : Marlowe.Database_Index;
                         Item : Database_Record);
package Kit.Generic_Cache is

   type Cache_Record is new Kit.Cache.Cache_Entry_Record with
      record
         Db : Database_Record;
      end record;

   overriding
   procedure Write (Item   : Cache_Record;
                    Index  : Marlowe.Database_Index);

   type Cache_Access is access all Cache_Record'Class;

   function Get
     (Index       : Marlowe.Database_Index;
      Lock_Result : Boolean := False)
      return Cache_Access;

   procedure S_Lock (Index : Marlowe.Database_Index);
   procedure U_Lock (Index : Marlowe.Database_Index);
   procedure X_Lock (Index : Marlowe.Database_Index);
   procedure Unlock (Index : Marlowe.Database_Index);

end Kit.Generic_Cache;
