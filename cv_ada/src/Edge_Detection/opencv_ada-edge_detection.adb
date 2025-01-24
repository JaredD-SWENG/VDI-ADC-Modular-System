with Opencv_Ada.Graphics.Pixel;
with Opencv_Ada.Blur;                   use Opencv_Ada.Blur;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body OpenCV_Ada.Edge_Detection is
   -- Sobel edge detection implementation
   procedure Sobel_Edge_Detection
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      Temp : Storage_Array := Data;

      -- Sobel kernels
      Gx : constant array (1 .. 3, 1 .. 3) of Integer :=
        [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]];

      Gy : constant array (1 .. 3, 1 .. 3) of Integer :=
        [[-1, -2, -1], [0, 0, 0], [1, 2, 1]];

      Max_Gradient : Natural := 0;
   begin
      -- First pass to compute gradients and find maximum
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Gradient_X         : Integer := 0;
               Gradient_Y         : Integer := 0;
               Gradient_Magnitude : Natural;
            begin
               -- Apply Sobel kernels
               for I in 1 .. 3 loop
                  for J in 1 .. 3 loop
                     declare
                        Pixel_Value : constant Integer :=
                          Integer
                            (Graphics.Get_Pixel
                               (Temp,
                                X + Storage_Count (J) - 2,
                                Y + Storage_Count (I) - 2,
                                Width,
                                Height,
                                Channels));
                     begin
                        Gradient_X := Gradient_X + (Pixel_Value * Gx (I, J));
                        Gradient_Y := Gradient_Y + (Pixel_Value * Gy (I, J));
                     end;
                  end loop;
               end loop;

               -- Calculate gradient magnitude using Sqrt from Elementary_Functions
               declare
                  Gx_Squared : constant Float :=
                    Float (Gradient_X * Gradient_X);
                  Gy_Squared : constant Float :=
                    Float (Gradient_Y * Gradient_Y);
                  Magnitude  : constant Float :=
                    Sqrt (Gx_Squared + Gy_Squared);
               begin
                  -- Convert back to Natural with bounds checking
                  if Magnitude > Float (Natural'Last) then
                     Gradient_Magnitude := Natural'Last;
                  else
                     Gradient_Magnitude := Natural (Magnitude);
                  end if;
               end;

               -- Update maximum gradient
               if Gradient_Magnitude > Max_Gradient then
                  Max_Gradient := Gradient_Magnitude;
               end if;

               -- Store gradient in all channels
               for C in 0 .. Channels - 1 loop
                  declare
                     Pixel_Index : constant Storage_Count :=
                       ((Y - 1) * Width + (X - 1)) * Channels + C + 1;
                  begin
                     Data (Pixel_Index) :=
                       Storage_Element (Natural'Min (Gradient_Magnitude, 255));
                  end;
               end loop;
            end;
         end loop;
      end loop;

      -- Normalize the gradients
      if Max_Gradient > 0 then
         for I in Data'Range loop
            declare
               Normalized_Value : constant Natural :=
                 (Natural (Data (I)) * 255) / Max_Gradient;
            begin
               Data (I) :=
                 Storage_Element (Natural'Min (Normalized_Value, 255));
            end;
         end loop;
      end if;
   end Sobel_Edge_Detection;

   procedure Canny_Edge_Detection
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Low_Threshold           : Float := 0.1;
      High_Threshold          : Float := 0.3)
   is

      Temp               : Storage_Array := Data;
      Gradient_Magnitude : Storage_Array (Data'Range);
      Gradient_Direction : array (1 .. Width * Height) of Float;
   begin
      -- Step 1: Apply Gaussian blur
      Gaussian_Blur (Data, Width, Height, Channels);

      -- Step 2: Compute gradients using Sobel
      Sobel_Edge_Detection (Data, Width, Height, Channels);

      -- Step 3: Non-maximum suppression
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Idx       : constant Storage_Count :=
                 ((Y - 1) * Width + (X - 1)) * Channels + 1;
               -- Fix the array index calculation
               Angle     : constant Float :=
                 Gradient_Direction
                   (Storage_Count'Pos
                      ((Storage_Count (Y) - 1) * Width
                       + (Storage_Count (X) - 1))
                    + 1);
               Magnitude : constant Storage_Element := Data (Idx);
            begin
               -- Quantize angle to 0, 45, 90, or 135 degrees
               if ((Angle >= -22.5 and Angle <= 22.5)
                   or (Angle <= -157.5)
                   or (Angle >= 157.5))
               then
                  -- Horizontal edge
                  if Magnitude <= Data (Idx - Channels)
                    or Magnitude <= Data (Idx + Channels)
                  then
                     Gradient_Magnitude (Idx) := 0;
                  else
                     Gradient_Magnitude (Idx) := Magnitude;
                  end if;
               elsif ((Angle >= 22.5 and Angle <= 67.5)
                      or (Angle <= -112.5 and Angle >= -157.5))
               then
                  -- 45 degree edge
                  if Magnitude <= Data (Idx - Channels - Width * Channels)
                    or Magnitude <= Data (Idx + Channels + Width * Channels)
                  then
                     Gradient_Magnitude (Idx) := 0;
                  else
                     Gradient_Magnitude (Idx) := Magnitude;
                  end if;
                  -- Add similar checks for 90 and 135 degrees
               end if;
            end;
         end loop;
      end loop;

      -- Step 4: Double threshold and hysteresis
      declare
         High : constant Storage_Element :=
           Storage_Element (255.0 * High_Threshold);
         Low  : constant Storage_Element :=
           Storage_Element (255.0 * Low_Threshold);
      begin
         for I in Data'Range loop
            if Gradient_Magnitude (I) >= High then
               Data (I) := 255;  -- Strong edge
            elsif Gradient_Magnitude (I) >= Low then
               Data (I) := 128;  -- Weak edge
            else
               Data (I) := 0;    -- Non-edge
            end if;
         end loop;
      end;

      -- Step 5: Edge tracking by hysteresis
      -- Inside Canny_Edge_Detection procedure
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Idx : constant Storage_Count :=
                 ((Y - 1) * Width + (X - 1)) * Channels + 1;
            begin
               if Data (Idx) = 128 then
                  -- Weak edge
                  -- Check 8-connected neighbors for strong edges
                  declare
                     Has_Strong_Neighbor : Boolean := False;
                     Neighbor_Idx        : Storage_Count;
                  begin
                     for DY in -1 .. 1 loop
                        for DX in -1 .. 1 loop
                           -- Calculate neighbor index with bounds checking
                           if (Storage_Count (Integer (X) + DX)) >= 1
                             and (Storage_Count (Integer (X) + DX)) <= Width
                             and (Storage_Count (Integer (Y) + DY)) >= 1
                             and (Storage_Count (Integer (Y) + DY)) <= Height
                           then

                              Neighbor_Idx :=
                                ((Storage_Count (Integer (Y) + DY) - 1) * Width
                                 + (Storage_Count (Integer (X) + DX) - 1))
                                * Channels
                                + 1;

                              if Data (Neighbor_Idx) = 255 then
                                 Has_Strong_Neighbor := True;
                                 exit;
                              end if;
                           end if;
                        end loop;
                        exit when Has_Strong_Neighbor;
                     end loop;

                     if Has_Strong_Neighbor then
                        Data (Idx) := 255; -- Convert to strong edge

                     else
                        Data (Idx) := 0;   -- Remove weak edge
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;
   end Canny_Edge_Detection;
end OpenCV_Ada.Edge_Detection;
