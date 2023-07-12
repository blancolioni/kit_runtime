with Ada.Finalization;

package Kit.Locking is

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled
     with private;

   procedure S_Lock (Item : not null access Root_Lockable_Type);
   procedure U_Lock (Item : not null access Root_Lockable_Type);
   procedure X_Lock (Item : not null access Root_Lockable_Type);

   procedure Unlock (Item : not null access Root_Lockable_Type);

   function Is_S_Locked (Item : Root_Lockable_Type) return Boolean;
   function Is_U_Locked (Item : Root_Lockable_Type) return Boolean;
   function Is_X_Locked (Item : Root_Lockable_Type) return Boolean;

private

   protected type Lock_Type is
      entry S_Lock;
      entry U_Lock;
      entry X_Lock;
      procedure Unlock;

      function Is_S_Locked return Boolean;
      function Is_U_Locked return Boolean;
      function Is_X_Locked return Boolean;

   private
      S_Lock_Count : Natural := 0;
      U_Locked     : Boolean := False;
      X_Locked     : Boolean := False;
   end Lock_Type;

   type Root_Lockable_Type is
     abstract limited new Ada.Finalization.Limited_Controlled with
      record
         Lock : Lock_Type;
      end record;

   function Is_S_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.Lock.Is_S_Locked);

   function Is_U_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.Lock.Is_U_Locked);

   function Is_X_Locked (Item : Root_Lockable_Type) return Boolean
   is (Item.Lock.Is_X_Locked);

end Kit.Locking;
