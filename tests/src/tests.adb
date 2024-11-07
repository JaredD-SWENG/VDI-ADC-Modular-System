with QOI;
with System.Storage_Elements; use System.Storage_Elements;
with Load_QOI;                use Load_QOI;
with OpenCV_Ada.Pixel;        use OpenCV_Ada.Pixel;
with Ada.Strings;             use Ada.Strings;
with Ada.Text_IO;             use Ada.Text_IO;

procedure Tests is
   -- Input_File_Name   : String := "src\" & "5x5BlackSquare.qoi";
    Input_File_Name   : String := "src\" & "hi144p.qoi";
   Output_File_Name  : String := "output.qoi";
   Input : Input_Data;
begin
   Input := Get_QOI (Input_File_Name);
   declare
      Output      : Storage_Array (1 .. QOI.Encode_Worst_Case(Input.Desc));
      Output_Temp : Storage_Array (1 .. Input.Desc.Width * Input.Desc.Height * Input.Desc.Channels) := (others => 150);
      Output_Size : Storage_Count;

      ----------------- BEGIN BLUR -----------------
      procedure Apply_Gaussian_Blur
        (S      : Storage_Array;
         D      : out Storage_Array;
         Width  : Integer;
         Height : Integer)
      is
         function Create_Pixel(S : in Storage_Array; C : Integer; R : Integer) return Pixel
         is
            BO    : Integer := R * Width * 3 + 3 * C + 1;
            Red   : Float;
            Green : Float;
            Blue  : Float;
         begin
            -- Put_Line(BO'Image);
            Red   := Float (S (Storage_Offset (BO)));
            Green := Float (S (Storage_Offset (BO + 1)));
            Blue  := Float (S (Storage_Offset (BO + 2)));
            
            return (Red, Green, Blue);
         end;

         function Compute_Gaussian_Blur(S : Storage_Array; C : Integer; R : Integer) return Pixel
         is
            -- Gaussian weights for a 3x3 kernel
            W : constant array (0 .. 2, 0 .. 2) of Float :=
              ((1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0),
               (2.0 / 16.0, 4.0 / 16.0, 2.0 / 16.0),
               (1.0 / 16.0, 2.0 / 16.0, 1.0 / 16.0));

            -- Initialize pixel values
            Pixel_Sum  : Pixel := (0.0, 0.0, 0.0);
            Weight_Sum : Float := 0.0;

         begin
            for i in -1 .. 1 loop
               for j in -1 .. 1 loop
                  declare
                     P      : Pixel := Create_Pixel (S, C + j, R + i);
                     Weight : Float := W (i + 1, j + 1);
                  begin
                     Pixel_Sum := Pixel_Sum + (P * Weight);
                     Weight_Sum := Weight_Sum + Weight;
                  end;
               end loop;
            end loop;

            -- Normalize by the sum of the weights
            return Pixel_Sum / Weight_Sum;
         end;

      begin
         for Row in 1 .. Height - 2 loop
            for Col in 1 .. Width - 2 loop
               declare
                  P           : Pixel := Compute_Gaussian_Blur (S, Col, Row);
                  Base_Offset : Integer := Row * Width * 3 + 3 * Col + 1;
               begin
                  D (Storage_Offset (Base_Offset))       := Storage_Element (P.R);
                  D (Storage_Offset (Base_Offset + 1))   := Storage_Element (P.G);
                  D (Storage_Offset (Base_Offset + 2))   := Storage_Element (P.B);
               end;
            end loop;
         end loop;
      end Apply_Gaussian_Blur;
      ----------------- END BLUR -----------------
   begin
      Apply_Gaussian_Blur(Input.Data.all, Output_Temp, Integer(Input.Desc.Width), Integer(Input.Desc.Height));
      QOI.Encode (Output_Temp, Input.Desc, Output, Output_Size);
      Write_To_File (Output_File_Name, Output, Output_Size);
   end;
end Tests;