package Kit.Strings is

   type String_Type (Max_Length : Natural) is
      record
         Text     : String (1 .. Max_Length) := (others => Character'Val (0));
         Length   : Natural := 0;
      end record;

end Kit.Strings;
