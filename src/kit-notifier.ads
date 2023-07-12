with Marlowe;

package Kit.Notifier is

   type Table_Notify_Interface is interface;

   procedure Notify_Table_Change
     (Handle : Table_Notify_Interface)
   is null;

   type Record_Notify_Interface is interface;

   procedure Notify_Record_Change
     (Handle         : Record_Notify_Interface;
      Changed_Record : Marlowe.Database_Index)
   is null;

   procedure Add_Table_Change_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Table_Notify_Interface'Class);

   procedure Add_Record_Delete_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class);

   procedure Add_Record_Create_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class);

   procedure Add_Record_Change_Handler
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index;
      Handler : Record_Notify_Interface'Class);

   procedure Table_Changed
     (Table   : Marlowe.Table_Index);

   procedure Record_Changed
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index);

   procedure Record_Deleted
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index);

   procedure Record_Created
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index);

   procedure Stop;

end Kit.Notifier;
