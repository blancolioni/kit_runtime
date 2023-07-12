package Kit.Mutex is

   protected type Mutex_Type is
      entry Lock;
      entry Shared_Lock;
      procedure Unlock;
      procedure Shared_Unlock;
      function Shared_Lock_Count return Natural;
      function Exclusive_Lock return Boolean;

   private
      Locked : Boolean := False;
      Shared : Natural := 0;
   end Mutex_Type;

   Memory_Mutex : Mutex_Type;

end Kit.Mutex;
