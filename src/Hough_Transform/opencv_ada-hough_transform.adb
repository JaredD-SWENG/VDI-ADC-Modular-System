with Opencv_Ada.Graphics;               use Opencv_Ada.Graphics;
with Opencv_Ada.Graphics.Pixel;         use Opencv_Ada.Graphics.Pixel;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body OpenCV_Ada.Hough_Transform is
   procedure Hough_Line_Transform
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Theta_Resolution        : Positive := 180;
      Rho_Resolution          : Positive := 180)
   is
      -- Calculate maximum possible rho value (diagonal length)
      Max_Rho : constant Float :=
        Sqrt (Float (Width * Width + Height * Height));

      -- Create accumulator array
      Acc : constant Accumulator_Access :=
        new Accumulator_Array (1 .. Rho_Resolution, 1 .. Theta_Resolution);

      -- Constants for conversion
      Deg_To_Rad : constant Float := Ada.Numerics.Pi / 180.0;
      Rho_Scale  : constant Float := Float (Rho_Resolution) / (2.0 * Max_Rho);

      -- Parameters for line filtering
      Max_Lines       : constant Positive :=
        10; -- Maximum number of lines to detect
      Min_Line_Length : constant Float :=
        Float (Width + Height) / 8.0; -- Minimum line length
      Max_Line_Gap    : constant Float :=
        Float (Width + Height) / 16.0; -- Maximum gap between line segments
      Angle_Threshold : constant Float :=
        10.0 * Deg_To_Rad; -- Angle difference threshold for merging
   begin
      -- Initialize accumulator
      for R in Acc'Range(1) loop
         for T in Acc'Range(2) loop
            Acc (R, T) := 0;
         end loop;
      end loop;

      -- Voting process (unchanged)
      for Y in 1 .. Height loop
         for X in 1 .. Width loop
            if Graphics.Pixel.Get_Pixel (Data, X, Y, Width, Height, Channels)
              > 0
            then
               for T in 0 .. Theta_Resolution - 1 loop
                  declare
                     Theta   : constant Float := Float (T) * Deg_To_Rad;
                     Rho     : constant Float :=
                       Float (X) * Cos (Theta) + Float (Y) * Sin (Theta);
                     Rho_Idx : constant Integer :=
                       Integer ((Rho + Max_Rho) * Rho_Scale);
                  begin
                     if Rho_Idx > 0 and Rho_Idx <= Rho_Resolution then
                        Acc (Rho_Idx, T + 1) := Acc (Rho_Idx, T + 1) + 1;
                     end if;
                  end;
               end loop;
            end if;
         end loop;
      end loop;

      -- Find peaks with non-maximal suppression
      declare
         -- Increase threshold based on image size[7]
         Threshold : constant Natural :=
           Natural (0.85 * Float (Width + Height) / 2.0); -- Higher threshold

         type Line_Info is record
            Rho, Theta : Float;
            Votes      : Natural;
         end record;

         -- Array to store detected lines
         type Line_Array is array (1 .. Max_Lines) of Line_Info;
         Lines      : Line_Array;
         Line_Count : Natural := 0;

         -- Non-maximal suppression window size
         Window_Size : constant Positive := 5;
      begin
         -- Find local maxima
         for R in Window_Size + 1 .. Rho_Resolution - Window_Size loop
            for T in Window_Size + 1 .. Theta_Resolution - Window_Size loop
               if Acc (R, T) > Threshold then
                  -- Check if it's a local maximum
                  declare
                     Is_Maximum : Boolean := True;
                  begin
                     -- Check neighborhood
                     for DR in -Window_Size .. Window_Size loop
                        for DT in -Window_Size .. Window_Size loop
                           if DR /= 0 or DT /= 0 then
                              if Acc (R + DR, T + DT) >= Acc (R, T) then
                                 Is_Maximum := False;
                                 exit;
                              end if;
                           end if;
                        end loop;
                        exit when not Is_Maximum;
                     end loop;

                     -- If it's a local maximum, add to lines
                     if Is_Maximum and Line_Count < Max_Lines then
                        Line_Count := Line_Count + 1;
                        Lines (Line_Count) :=
                          (Rho   => (Float (R) / Rho_Scale) - Max_Rho,
                           Theta => Float (T - 1) * Deg_To_Rad,
                           Votes => Acc (R, T));
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         -- Draw only the strongest lines
         for I in 1 .. Line_Count loop
            declare
               Theta          : constant Float := Lines (I).Theta;
               Rho            : constant Float := Lines (I).Rho;
               X1, Y1, X2, Y2 : Integer;
            begin
               -- Line endpoint calculation (unchanged)
               if abs (Sin (Theta)) < 0.001 then
                  X1 := Integer (Rho);
                  X2 := X1;
                  Y1 := 1;
                  Y2 := Integer (Height);
               elsif abs (Cos (Theta)) < 0.001 then
                  Y1 := Integer (Rho);
                  Y2 := Y1;
                  X1 := 1;
                  X2 := Integer (Width);
               else
                  X1 := 1;
                  Y1 :=
                    Integer ((Rho - Float (X1) * Cos (Theta)) / Sin (Theta));
                  X2 := Integer (Width);
                  Y2 :=
                    Integer ((Rho - Float (X2) * Cos (Theta)) / Sin (Theta));
               end if;

               Draw_Line (Data, X1, Y1, X2, Y2, Width, Height, Channels);
            end;
         end loop;
      end;

      -- Free accumulator memory
      declare
         procedure Free is new
           Ada.Unchecked_Deallocation
             (Object => Accumulator_Array,
              Name   => Accumulator_Access);
         Temp : Accumulator_Access := Acc;
      begin
         Free (Temp);
      end;
   end Hough_Line_Transform;
end OpenCV_Ada.Hough_Transform;
