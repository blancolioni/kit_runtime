with System.Storage_Elements;

with Marlowe.Data_Stores;

package Kit.Text is

   type Text_Type is private;

   function To_String
     (Handle : Marlowe.Data_Stores.Data_Store;
      Field  : Text_Type)
      return String;

   procedure Set_Text
     (Handle : Marlowe.Data_Stores.Data_Store;
      Value  : String;
      Field  : in out Text_Type);

   procedure From_Storage
     (Value   :    out Text_Type;
      Storage : in     System.Storage_Elements.Storage_Array);

   procedure To_Storage
     (Value   : in     Text_Type;
      Storage : in out System.Storage_Elements.Storage_Array);

private

   Max_Short_Text : constant := 23;
   type Short_Text_Length is mod 256;

   subtype Short_Text_Type is String (1 .. Max_Short_Text);

   type Text_Type is
      record
         Overflow     : Marlowe.File_And_Page := Marlowe.Null_File_And_Page;
         Local_Length : Short_Text_Length     := 0;
         Local_Text   : Short_Text_Type       :=
                          (others => Character'Val (0));
      end record;

   for Text_Type'Size use 32 * 8;

end Kit.Text;
