with Marlowe.Key_Storage;

package body Kit.Text is

   ------------------
   -- From_Storage --
   ------------------

   procedure From_Storage
     (Value   :    out Text_Type;
      Storage : in     System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
      Buffer : constant Storage_Array (0 .. Storage'Length - 1) := Storage;
   begin
      Marlowe.Key_Storage.From_Storage
        (Value.Overflow, Buffer (0 .. 7));
      Value.Local_Length := Short_Text_Length (Buffer (8));
      for I in Value.Local_Text'Range loop
         Value.Local_Text (I) :=
           Character'Val
             (Buffer (Storage_Offset (I - Value.Local_Text'First + 9)));
      end loop;
   end From_Storage;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Handle : Marlowe.Data_Stores.Data_Store;
      Value  : String;
      Field  : in out Text_Type)
   is
   begin
      if Value'Length <= Field.Local_Text'Length then
         Field.Local_Length := Short_Text_Length (Value'Length);
         Field.Local_Text (1 .. Value'Length) := Value;
      else
         declare
            use System.Storage_Elements;
            use type Marlowe.File_And_Page;
            Overflow : Storage_Array (1 .. Value'Length - Max_Short_Text);
            Extra_Text : constant String :=
                           Value (Value'First + Max_Short_Text
                                  .. Value'Last);
         begin
            for I in Extra_Text'Range loop
               Overflow (Storage_Offset (I - Extra_Text'First + 1)) :=
                 Character'Pos (Extra_Text (I));
            end loop;

            if Field.Overflow = Marlowe.Null_File_And_Page then
               Field.Overflow :=
                 Marlowe.Data_Stores.Create_Field_Extension
                   (Handle.all);
            end if;

            Marlowe.Data_Stores.Write_Field_Extension
              (Store     => Handle.all,
               Reference => Field.Overflow,
               Data      => Overflow);

            Field.Local_Length :=
              Short_Text_Length (Field.Local_Text'Last + 1);
            Field.Local_Text   :=
              Value (Value'First .. Value'First + Field.Local_Text'Length - 1);
         end;
      end if;
   end Set_Text;

   ----------------
   -- To_Storage --
   ----------------

   procedure To_Storage
     (Value   : in     Text_Type;
      Storage : in out System.Storage_Elements.Storage_Array)
   is
      use System.Storage_Elements;
   begin
      Marlowe.Key_Storage.To_Storage
        (Value.Overflow, Storage (Storage'First .. Storage'First + 7));
      Storage (Storage'First + 8) := Storage_Element (Value.Local_Length);
      for I in Value.Local_Text'Range loop
         declare
            Index : constant Storage_Offset :=
                      Storage_Offset (I - Value.Local_Text'First)
                      + Storage'First + 9;
         begin
            Storage (Index) := Character'Pos (Value.Local_Text (I));
         end;
      end loop;
   end To_Storage;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Handle : Marlowe.Data_Stores.Data_Store;
      Field  : Text_Type)
      return String
   is
      use System.Storage_Elements;
   begin

      if Natural (Field.Local_Length) <= Field.Local_Text'Last then
         return Field.Local_Text (1 .. Natural (Field.Local_Length));
      else
         declare
            Overflow : constant Storage_Array :=
                         Marlowe.Data_Stores.Read_Field_Extension
                           (Handle.all, Field.Overflow);
            Back     : String (1 .. Natural (Overflow'Length));
         begin
            for I in Back'Range loop
               Back (I) :=
                 Character'Val
                   (Overflow (Storage_Offset (I) + Overflow'First - 1));
            end loop;
            return Field.Local_Text & Back;
         end;
      end if;
   end To_String;

end Kit.Text;
