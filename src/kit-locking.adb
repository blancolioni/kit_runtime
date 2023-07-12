package body Kit.Locking is

   protected body Lock_Type is

      function Is_S_Locked return Boolean is (S_Lock_Count > 0);

      function Is_U_Locked return Boolean is (U_Locked);

      function Is_X_Locked return Boolean is (X_Locked);

      entry S_Lock when not X_Locked is
      begin
         S_Lock_Count := S_Lock_Count + 1;
      end S_Lock;

      entry U_Lock when not U_Locked and then not X_Locked is
      begin
         U_Locked := True;
      end U_Lock;

      entry X_Lock when S_Lock_Count = 0 is
      begin
         U_Locked := False;
         X_Locked := True;
      end X_Lock;

      procedure Unlock is
      begin
         if X_Locked then
            X_Locked := False;
         else
            S_Lock_Count := S_Lock_Count - 1;
         end if;
      end Unlock;

   end Lock_Type;

   ------------
   -- S_Lock --
   ------------

   procedure S_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.S_Lock;
   end S_Lock;

   ------------
   -- U_Lock --
   ------------

   procedure U_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.U_Lock;
   end U_Lock;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.Unlock;
   end Unlock;

   ------------
   -- X_Lock --
   ------------

   procedure X_Lock (Item : not null access Root_Lockable_Type) is
   begin
      Item.Lock.X_Lock;
   end X_Lock;

end Kit.Locking;
