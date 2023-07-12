with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;

package body Kit.Notifier is

   package Table_Notify_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Table_Notify_Interface'Class);

   package Record_Notify_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Record_Notify_Interface'Class);

   type Record_Notify_Entry is
      record
         On_Record_Changed : Record_Notify_Lists.List;
      end record;

   package Record_Notify_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Marlowe.Database_Index,
        Element_Type => Record_Notify_Entry,
        "<"          => Marlowe."<");

   type Table_Notifier_Entry is
      record
         On_Table_Change  : Table_Notify_Lists.List;
         On_New_Record    : Record_Notify_Lists.List;
         On_Delete_Record : Record_Notify_Lists.List;
         Record_Notifiers : Record_Notify_Maps.Map;
      end record;

   type Table_Notifier_Index is range 1 .. Marlowe.Table_Index'Last;

   package Table_Notifier_Vectors is
     new Ada.Containers.Vectors (Table_Notifier_Index, Table_Notifier_Entry);

   type Notification_Type is (Notifier_Stopped,
                              Table_Changed,
                              Record_Added, Record_Deleted,
                              Record_Changed);

   subtype Table_Notification is Notification_Type range
     Table_Changed .. Table_Changed;

   subtype Record_Notification is Notification_Type range
     Record_Added .. Record_Changed;

   type Notification_Record is
      record
         Notification : Notification_Type;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index;
      end record;

   package Notification_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Notification_Record);

   protected Table_Notifier_Buffer is
      entry Next_Notification
        (Notification : out Notification_Type;
         Table        : out Marlowe.Table_Index;
         Index        : out Marlowe.Database_Index);

      procedure Add_Notification
        (Notification : Notification_Type;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index);

   private
      Buffer : Notification_Lists.List;
      Empty  : Boolean := True;
   end Table_Notifier_Buffer;

   protected Handle_Vector is

      procedure Add_Table_Notification
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index;
         Handler      : Table_Notify_Interface'Class);

      procedure Add_Record_Notification
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index;
         Handler      : Record_Notify_Interface'Class);

      procedure Handle
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index);

      procedure Handle
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index);

   private

      Vector       : Table_Notifier_Vectors.Vector;

   end Handle_Vector;

   task Notifier_Task is

      entry Handle_Table_Notification
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index);

      entry Handle_Record_Notification
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index);

   end Notifier_Task;

   task Dispatch_Notifications_Task;

   -------------------------------
   -- Add_Record_Change_Handler --
   -------------------------------

   procedure Add_Record_Change_Handler
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Handle_Vector.Add_Record_Notification
        (Notification => Record_Changed,
         Table        => Table,
         Index        => Index,
         Handler      => Handler);
   end Add_Record_Change_Handler;

   -------------------------------
   -- Add_Record_Create_Handler --
   -------------------------------

   procedure Add_Record_Create_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Handle_Vector.Add_Record_Notification
        (Notification => Record_Added,
         Table        => Table,
         Index        => 0,
         Handler      => Handler);
   end Add_Record_Create_Handler;

   -------------------------------
   -- Add_Delete_Record_Handler --
   -------------------------------

   procedure Add_Record_Delete_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Record_Notify_Interface'Class)
   is
   begin
      Handle_Vector.Add_Record_Notification
        (Notification => Record_Deleted,
         Table        => Table,
         Index        => 0,
         Handler      => Handler);
   end Add_Record_Delete_Handler;

   ------------------------------
   -- Add_Table_Change_Handler --
   ------------------------------

   procedure Add_Table_Change_Handler
     (Table   : Marlowe.Table_Index;
      Handler : Table_Notify_Interface'Class)
   is
   begin
      Handle_Vector.Add_Table_Notification
        (Notification => Table_Changed,
         Table        => Table,
         Handler      => Handler);
   end Add_Table_Change_Handler;

   ---------------------------------
   -- Dispatch_Notifications_Task --
   ---------------------------------

   task body Dispatch_Notifications_Task is
      Notification : Notification_Type;
      Table        : Marlowe.Table_Index;
      Index        : Marlowe.Database_Index;
   begin
      loop
         Table_Notifier_Buffer.Next_Notification
           (Notification, Table, Index);
         case Notification is
            when Notifier_Stopped =>
               exit;
               when Table_Notification =>
               Handle_Vector.Handle
                 (Notification => Notification,
                  Table        => Table);
            when Record_Notification =>
               Handle_Vector.Handle
                 (Notification => Notification,
                  Table        => Table,
                  Index        => Index);
         end case;
      end loop;
   end Dispatch_Notifications_Task;

   -------------------
   -- Handle_Vector --
   -------------------

   protected body Handle_Vector is

      -----------------------------
      -- Add_Record_Notification --
      -----------------------------

      procedure Add_Record_Notification
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index;
         Handler      : Record_Notify_Interface'Class)
      is
         Notifier_Index : constant Table_Notifier_Index :=
                            Table_Notifier_Index (Table);
      begin
         while Vector.Last_Index < Notifier_Index loop
            Vector.Append (Table_Notifier_Entry'(others => <>));
         end loop;

         case Notification is
         when Record_Added =>
            Vector (Notifier_Index).On_New_Record.Append (Handler);
         when Record_Deleted =>
            Vector (Notifier_Index).On_Delete_Record.Append (Handler);
         when Record_Changed =>
            declare
               Map      : Record_Notify_Maps.Map renames
                            Vector (Notifier_Index).Record_Notifiers;
               Position : constant Record_Notify_Maps.Cursor :=
                            Map.Find (Index);
            begin
               if not Record_Notify_Maps.Has_Element (Position) then
                  declare
                     List : Record_Notify_Lists.List;
                  begin
                     List.Append (Handler);
                     Map.Insert (Index, (On_Record_Changed => List));
                  end;
               else
                  Map (Position).On_Record_Changed.Append (Handler);
               end if;
            end;
         end case;
      end Add_Record_Notification;

      ----------------------------
      -- Add_Table_Notification --
      ----------------------------

      procedure Add_Table_Notification
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index;
         Handler      : Table_Notify_Interface'Class)
      is
         Notifier_Index : constant Table_Notifier_Index :=
                            Table_Notifier_Index (Table);
      begin
         while Vector.Last_Index < Notifier_Index loop
            Vector.Append (Table_Notifier_Entry'(others => <>));
         end loop;

         case Notification is
            when Table_Changed =>
               Vector (Notifier_Index).On_Table_Change.Append (Handler);
         end case;
      end Add_Table_Notification;

      ------------
      -- Handle --
      ------------

      procedure Handle
        (Notification : Table_Notification;
         Table        : Marlowe.Table_Index)
      is
         Notifier_Index : constant Table_Notifier_Index :=
                            Table_Notifier_Index (Table);
      begin
         if Vector.Last_Index >= Notifier_Index then
            case Notification is
            when Table_Changed =>
               for Handler of
                 Vector (Notifier_Index).On_Table_Change
               loop
                  Handler.Notify_Table_Change;
               end loop;
            end case;
         end if;
      end Handle;

      ------------
      -- Handle --
      ------------

      procedure Handle
        (Notification : Record_Notification;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index)
      is
         Notifier_Index : constant Table_Notifier_Index :=
                            Table_Notifier_Index (Table);
      begin
         if Vector.Last_Index >= Notifier_Index then
            case Notification is
            when Record_Added =>
               for Handler of
                 Vector (Notifier_Index).On_New_Record
               loop
                  Handler.Notify_Record_Change (Index);
               end loop;
            when Record_Deleted =>
               for Handler of
                 Vector (Notifier_Index).On_Delete_Record
               loop
                  Handler.Notify_Record_Change (Index);
               end loop;
            when Record_Changed =>
               declare
                  Map      : Record_Notify_Maps.Map renames
                               Vector (Notifier_Index).Record_Notifiers;
                  Position : constant Record_Notify_Maps.Cursor :=
                               Map.Find (Index);
               begin
                  if Record_Notify_Maps.Has_Element (Position) then
                     for Handler of
                       Map (Position).On_Record_Changed
                     loop
                        Handler.Notify_Record_Change (Index);
                     end loop;
                  end if;
               end;
            end case;
         end if;
      end Handle;

   end Handle_Vector;

   -------------------
   -- Notifier_Task --
   -------------------

   task body Notifier_Task is
      Local_Table_Notification  : Table_Notification;
      Local_Record_Notification : Record_Notification;
      Local_Table               : Marlowe.Table_Index;
      Local_Record_Index        : Marlowe.Database_Index;
   begin
      loop
         select
            accept Handle_Table_Notification
              (Notification : Table_Notification;
               Table        : Marlowe.Table_Index)
            do
               Local_Table_Notification := Notification;
               Local_Table := Table;
            end Handle_Table_Notification;

            Table_Notifier_Buffer.Add_Notification
              (Local_Table_Notification, Local_Table, 0);
         or
            accept Handle_Record_Notification
              (Notification : Record_Notification;
               Table        : Marlowe.Table_Index;
               Index        : Marlowe.Database_Index)
            do
               Local_Record_Notification := Notification;
               Local_Table := Table;
               Local_Record_Index := Index;
            end Handle_Record_Notification;

            Table_Notifier_Buffer.Add_Notification
              (Local_Record_Notification, Local_Table, Local_Record_Index);
         or
            terminate;
         end select;
      end loop;
   end Notifier_Task;

   --------------------
   -- Record_Changed --
   --------------------

   procedure Record_Changed
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Changed, Table, Index);
   end Record_Changed;

   --------------------
   -- Record_Created --
   --------------------

   procedure Record_Created
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Added, Table, Index);
   end Record_Created;

   --------------------
   -- Record_Deleted --
   --------------------

   procedure Record_Deleted
     (Table   : Marlowe.Table_Index;
      Index   : Marlowe.Database_Index)
   is
   begin
      Notifier_Task.Handle_Record_Notification
        (Record_Deleted, Table, Index);
   end Record_Deleted;

   ----------
   -- Stop --
   ----------

   procedure Stop is
   begin
      Table_Notifier_Buffer.Add_Notification
        (Notifier_Stopped, 0, 0);
   end Stop;

   -------------------
   -- Table_Changed --
   -------------------

   procedure Table_Changed
     (Table   : Marlowe.Table_Index)
   is
   begin
      Notifier_Task.Handle_Table_Notification
        (Table_Changed, Table);
   end Table_Changed;

   ---------------------------
   -- Table_Notifier_Buffer --
   ---------------------------

   protected body Table_Notifier_Buffer is

      -----------------------
      -- Next_Notification --
      -----------------------

      entry Next_Notification
        (Notification : out Notification_Type;
         Table        : out Marlowe.Table_Index;
         Index        : out Marlowe.Database_Index)
        when not Empty
      is
         Item : constant Notification_Record :=
                  Buffer.First_Element;
      begin
         Buffer.Delete_First;
         Empty := Buffer.Is_Empty;
         Notification := Item.Notification;
         Table := Item.Table;
         Index := Item.Index;
      end Next_Notification;

      ----------------------
      -- Add_Notification --
      ----------------------

      procedure Add_Notification
        (Notification : Notification_Type;
         Table        : Marlowe.Table_Index;
         Index        : Marlowe.Database_Index)
      is
      begin
         Buffer.Append ((Notification, Table, Index));
         Empty := False;
      end Add_Notification;

   end Table_Notifier_Buffer;

end Kit.Notifier;
