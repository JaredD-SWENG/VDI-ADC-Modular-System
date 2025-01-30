with QOI;
with Ada.Text_IO;                       use Ada.Text_IO;
with System.Storage_Elements;           use System.Storage_Elements;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package Morphological_Operations is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

   
   --------------------------------------------------------------------------------
   -- Morphological_Operation
   --
   -- Performs morphological operations (erosion, dilation, opening, closing) on
   -- binary images using specified structuring elements.
   --
   -- Parameters:
   --    Data      - Binary image data array (modified in-place)
   --    Desc      - QOI image descriptor containing format information
   --    Operation - String specifying operation ("Erosion", "Dilation",
   --                "Opening", "Closing")
   --    SE_Type   - Structuring element type ("Square" or "Circle")
   --    SE_Size   - Size of structuring element (1 to 30)
   --
   -- Effects:
   --    Modifies the input Data array in-place according to the specified
   --    morphological operation.
   --------------------------------------------------------------------------------
   procedure Morphological_Operation
     (Data    : in out Storage_Array; Desc : QOI.QOI_Desc; Operation : String;
      SE_Type :        String; SE_Size : Integer)
   is
      -- Define an enumeration type for operations
      type Morph_Operation is (Erosion, Dilation, Opening, Closing);

      -- Map input string to enumeration
      function To_Morph_Operation (Op : String) return Morph_Operation is
      begin
         if Op = "Erosion" then
            return Erosion;
         elsif Op = "Dilation" then
            return Dilation;
         elsif Op = "Opening" then
            return Opening;
         elsif Op = "Closing" then
            return Closing;
         else
            raise Program_Error with "Invalid operation";
         end if;
      end To_Morph_Operation;

      -- Size of each pixel in bytes (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      Width      : constant Integer       := Integer (Desc.Width);
      Height     : constant Integer       := Integer (Desc.Height);

      -- Temporary array for processing
      Temp : Storage_Array := Data;

      -- Function to check if a pixel is white (255)
      function Is_White (X, Y : Integer) return Boolean is
         Idx : constant Storage_Count :=
           Storage_Count ((Y * Width + X) * Integer (Pixel_Size) + 1);
      begin
         return Data (Idx) = 255;
      end Is_White;

      -- Function to create structuring element
      function In_Structuring_Element (X, Y : Integer) return Boolean is
         Center : constant Integer := SE_Size / 2;
         Dist_X : constant Integer := abs (X - Center);
         Dist_Y : constant Integer := abs (Y - Center);
      begin
         if SE_Type = "Square" then
            return Dist_X <= Center and Dist_Y <= Center;
         else -- Circle
            return
              Float'Floor (Sqrt (Float (Dist_X * Dist_X + Dist_Y * Dist_Y))) <=
              Float (Center);
         end if;
      end In_Structuring_Element;

      -- Procedures for basic operations
      procedure Erode is
      begin
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               declare
                  Result : Boolean                := True;
                  Idx    : constant Storage_Count :=
                    Storage_Count ((Y * Width + X) * Integer (Pixel_Size) + 1);
               begin
                  -- Check neighborhood
                  for SY in 0 .. SE_Size - 1 loop
                     for SX in 0 .. SE_Size - 1 loop
                        if In_Structuring_Element (SX, SY) then
                           declare
                              NX : constant Integer := X + SX - (SE_Size / 2);
                              NY : constant Integer := Y + SY - (SE_Size / 2);
                           begin
                              if NX >= 0 and NX < Width and NY >= 0 and
                                NY < Height
                              then
                                 Result := Result and Is_White (NX, NY);
                              end if;
                           end;
                        end if;
                     end loop;
                  end loop;

                  -- Set pixel value based on erosion result
                  if Result then
                     Temp (Idx)     := 255;
                     Temp (Idx + 1) := 255;
                     Temp (Idx + 2) := 255;
                  else
                     Temp (Idx)     := 0;
                     Temp (Idx + 1) := 0;
                     Temp (Idx + 2) := 0;
                  end if;
               end;
            end loop;
         end loop;
      end Erode;

      procedure Dilate is
      begin
         for Y in 0 .. Height - 1 loop
            for X in 0 .. Width - 1 loop
               declare
                  Result : Boolean                := False;
                  Idx    : constant Storage_Count :=
                    Storage_Count ((Y * Width + X) * Integer (Pixel_Size) + 1);
               begin
                  -- Check neighborhood for dilation condition
                  for SY in 0 .. SE_Size - 1 loop
                     for SX in 0 .. SE_Size - 1 loop
                        if In_Structuring_Element (SX, SY) then
                           declare
                              NX : constant Integer := X + SX - (SE_Size / 2);
                              NY : constant Integer := Y + SY - (SE_Size / 2);
                           begin
                              if NX >= 0 and NX < Width and NY >= 0 and
                                NY < Height
                              then
                                 Result := Result or Is_White (NX, NY);
                              end if;
                           end;
                        end if;
                     end loop;
                  end loop;

                  -- Set pixel value based on dilation result
                  if Result then
                     Temp (Idx)     := 255;
                     Temp (Idx + 1) := 255;
                     Temp (Idx + 2) := 255;
                  else
                     Temp (Idx)     := 0;
                     Temp (Idx + 1) := 0;
                     Temp (Idx + 2) := 0;
                  end if;
               end;
            end loop;
         end loop;
      end Dilate;

   begin
      -- Input validation for structuring element size and type checks.
      if SE_Size < 1 or SE_Size > 30 then
         raise Constraint_Error
           with "Structuring element size must be between 1 and 30";
      elsif not (SE_Type = "Square" or SE_Type = "Circle") then
         raise Program_Error with "Invalid structuring element type";
      end if;

      -- Perform requested operation using enumeration-based case statement.
      case To_Morph_Operation (Operation) is
         when Erosion =>
            Erode;

         when Dilation =>
            Dilate;

         when Opening =>
            Erode;
            Data := Temp;
            Dilate;

         when Closing =>
            Dilate;
            Data := Temp;
            Erode;

         when others =>
            raise Program_Error with "Unsupported operation";
      end case;

      -- Copy result back to input array.
      Data := Temp;

   end Morphological_Operation;

end Morphological_Operations;