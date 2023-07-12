package body Kit.Mutex is

   ----------------
   -- Mutex_Type --
   ----------------

   protected body Mutex_Type is

      --------------------
      -- Exclusive_Lock --
      --------------------

      function Exclusive_Lock return Boolean is
      begin
         return Locked;
      end Exclusive_Lock;

      ----------
      -- Lock --
      ----------

      entry Lock when not Locked and then Shared = 0 is
      begin
         Locked := True;
      end Lock;

      -----------------
      -- Shared_Lock --
      -----------------

      entry Shared_Lock when not Locked is
      begin
         Shared := Shared + 1;
      end Shared_Lock;

      -----------------------
      -- Shared_Lock_Count --
      -----------------------

      function Shared_Lock_Count return Natural is
      begin
         return Shared;
      end Shared_Lock_Count;

      -------------------
      -- Shared_Unlock --
      -------------------

      procedure Shared_Unlock is
      begin
         pragma Assert (Shared > 0);
         Shared := Shared - 1;
      end Shared_Unlock;

      ------------
      -- Unlock --
      ------------

      procedure Unlock is
      begin
         pragma Assert (Locked);
         Locked := False;
      end Unlock;

   end Mutex_Type;

end Kit.Mutex;
