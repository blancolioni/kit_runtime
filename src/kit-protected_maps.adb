package body Kit.Protected_Maps is

   ------------------------
   -- Constant_Reference --
   ------------------------

   function Constant_Reference
     (Container : in out Map;
      Key       : Key_Type)
      return Constant_Reference_Type
   is
      Element : Element_Access;
   begin
      Container.Internal.Get_Cached_Reference (Key, Element);
      return Constant_Reference_Type'
        (Element => Element);
   end Constant_Reference;

   --------------------
   -- Get_Statistics --
   --------------------

   procedure Get_Statistics
     (Container : Map;
      Size      : out Natural;
      Hits      : out Natural;
      Misses    : out Natural)
   is
      Result : constant Cache_Statistics :=
        Container.Internal.Get_Statistics;
   begin
      Size := Result.Size;
      Hits := Result.Hits;
      Misses := Result.Misses;
   end Get_Statistics;

   ----------------
   -- Invalidate --
   ----------------

   procedure Invalidate
     (Container : in out Map;
      Key       : Key_Type) is
   begin
      Container.Internal.Invalidate (Key);
   end Invalidate;

   -------------------
   -- Protected_Map --
   -------------------

   protected body Protected_Map is

      --------------------------
      -- Get_Cached_Reference --
      --------------------------

      procedure Get_Cached_Reference
        (Key    : Key_Type;
         Result : out Element_Access)
      is
         Position : constant Maps.Cursor := Map.Find (Key);
      begin
         if Maps.Has_Element (Position) then
            Result := Maps.Element (Position);
--              declare
--                 Item : Cached_Element renames Map (Position);
--              begin
--                 if Item.Valid then
--                    Hit_Count := Hit_Count + 1;
--                 else
--                    Invalid_Count := Invalid_Count + 1;
--                    Load (Key, Item.Element.all);
--                    Item.Valid := True;
--                 end if;
--                 Result := Item.Element;
--              end;
         else
            Miss_Count := Miss_Count + 1;
            if Free.Is_Empty then
               Result := new Element_Type;
            else
               Result := Free.First_Element;
               Free.Delete_First;
            end if;
            Load (Key, Result.all);
            Map.Insert (Key, Result); --  (True, Result));
         end if;
      end Get_Cached_Reference;

      --------------------
      -- Get_Statistics --
      --------------------

      function Get_Statistics return Cache_Statistics is
      begin
         return Cache_Statistics'
           (Size   => Natural (Map.Length),
            Hits   => Hit_Count,
            Misses => Miss_Count,
            Invalid => Invalid_Count);
      end Get_Statistics;

      ----------------
      -- Invalidate --
      ----------------

      procedure Invalidate (Key : Key_Type) is
         Position : Maps.Cursor := Map.Find (Key);
      begin
         if Maps.Has_Element (Position) then
            Map.Delete (Position); --  Map (Position).Valid := False;
         end if;
      end Invalidate;

   end Protected_Map;

end Kit.Protected_Maps;
