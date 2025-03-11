with CV_Ada.Graphics;                   use CV_Ada.Graphics;
with CV_Ada.Graphics.Pixel;             use CV_Ada.Graphics.Pixel;
with Ada.Unchecked_Deallocation;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO;                       use Ada.Text_IO;

package body CV_Ada.Hough_Transform is
   ------------------------------------------------------------------------------
   -- Hough_Line_Transform
   --
   -- Applies the Hough Transform to detect straight lines in a binary image.
   --
   -- Parameters:
   --   Data             - Image data array (modified in-place to draw lines)
   --   Width, Height    - Dimensions of the image
   --   Channels         - Number of channels per pixel (e.g., 3 for RGB)
   --   Theta_Resolution - Angular resolution for line detection (default 180)
   --   Rho_Resolution   - Distance resolution for line detection (default 180)
   --
   -- Description:
   --   This procedure uses an accumulator to identify lines by voting on
   --   rho (distance) and theta (angle) values. Lines are detected as peaks
   --   in the accumulator, with non-maximal suppression to refine results.
   ------------------------------------------------------------------------------
   procedure Hough_Line_Transform
     (Input          : in out Input_Data; Theta_Resolution : Positive := 180;
      Rho_Resolution :        Positive := 180; Left_Line : out Line_Parameters;
      Right_Line     :    out Line_Parameters)
   is
      -- Extract necessary parameters from Input.Desc
      Width    : Storage_Count := Input.Desc.Width;
      Height   : Storage_Count := Input.Desc.Height;
      Channels : Storage_Count := Input.Desc.Channels;

      -- Get direct reference to the data array
      Data : Storage_Array_Access := Input.Data;

      -- Calculate maximum possible rho value (image diagonal length)
      Max_Rho : constant Float :=
        Sqrt (Float (Width * Width + Height * Height));

      -- Initialize accumulator array for line voting
      Acc : constant Accumulator_Access :=
        new Accumulator_Array (1 .. Rho_Resolution, 1 .. Theta_Resolution);

      -- Conversion constants
      Deg_To_Rad : constant Float := Ada.Numerics.Pi / 180.0;
      Rho_Scale  : constant Float := Float (Rho_Resolution) / (2.0 * Max_Rho);

      -- Parameters for line filtering
      Max_Lines       : constant Positive := 10; -- Max lines to detect
      Min_Line_Length : constant Float    := Float (Width + Height) / 8.0;
      Max_Line_Gap    : constant Float    := Float (Width + Height) / 16.0;
      Angle_Threshold : constant Float    := 10.0 * Deg_To_Rad;

      -- Initialize left and right line parameters
      Left_Line_Found  : Boolean := False;
      Right_Line_Found : Boolean := False;
   begin
      -- Initialize accumulator to zero
      for R in Acc'Range (1) loop
         for T in Acc'Range (2) loop
            Acc (R, T) := 0;
         end loop;
      end loop;

      -- Voting process: for each pixel, vote in the accumulator if it's part of a line
      for Y in 1 .. Height loop
         for X in 1 .. Width loop
            if Get_Pixel (Data.all, X, Y, Width, Height, Channels) > 0 then
               -- Loop through theta values and compute corresponding rho
               for T in 0 .. Theta_Resolution - 1 loop
                  declare
                     Theta   : constant Float   := Float (T) * Deg_To_Rad;
                     Rho     : constant Float   :=
                       Float (X) * Cos (Theta) + Float (Y) * Sin (Theta);
                     Rho_Idx : constant Integer :=
                       Integer ((Rho + Max_Rho) * Rho_Scale);
                  begin
                     -- Vote if rho index is within valid range
                     if Rho_Idx > 0 and Rho_Idx <= Rho_Resolution then
                        Acc (Rho_Idx, T + 1) := Acc (Rho_Idx, T + 1) + 1;
                     end if;
                  end;
               end loop;
            end if;
         end loop;
      end loop;

      -- Detect peaks in accumulator using non-maximal suppression
      declare
         Threshold : constant Natural :=
           Natural
             (0.85 * Float (Width + Height) /
              2.0); -- Dynamic threshold

         type Line_Info is record
            Rho, Theta     : Float;
            Votes          : Natural;
            X1, Y1, X2, Y2 : Integer; -- Store endpoints
            Slope : Float;           -- Store slope for lane classification
         end record;

         -- Array to store detected lines
         type Line_Array is array (1 .. Max_Lines) of Line_Info;
         Lines      : Line_Array;
         Line_Count : Natural := 0;

         -- Arrays to store left and right lane candidates
         Left_Lane_Candidates  : Line_Array;
         Right_Lane_Candidates : Line_Array;
         Left_Count            : Natural := 0;
         Right_Count           : Natural := 0;

         -- Non-maximal suppression window size
         Window_Size : constant Positive := 5;

         -- Lane angle thresholds (in radians)
         Left_Lane_Min_Angle  : constant Float := 0.5;  -- ~30 degrees
         Left_Lane_Max_Angle  : constant Float := 1.3;  -- ~75 degrees
         Right_Lane_Min_Angle : constant Float := 1.8; -- ~105 degrees
         Right_Lane_Max_Angle : constant Float := 2.6; -- ~150 degrees
      begin
         -- Identify local maxima in accumulator
         for R in Window_Size + 1 .. Rho_Resolution - Window_Size loop
            for T in Window_Size + 1 .. Theta_Resolution - Window_Size loop
               if Acc (R, T) > Threshold then
                  declare
                     Is_Maximum : Boolean := True;
                  begin
                     -- Check surrounding cells to ensure local maximum
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

                     -- Store line if it's a local maximum and within max line limit
                     if Is_Maximum and Line_Count < Max_Lines then
                        declare
                           Theta          : constant Float :=
                             Float (T - 1) * Deg_To_Rad;
                           Rho            : constant Float :=
                             (Float (R) / Rho_Scale) - Max_Rho;
                           X1, Y1, X2, Y2 : Integer;
                           Line_Slope     : Float;
                        begin
                           -- Calculate endpoints of each line based on rho and theta
                           if abs (Sin (Theta)) < 0.001 then
                              X1         := Integer (Rho);
                              X2         := X1;
                              Y1         := 1;
                              Y2         := Integer (Height);
                              Line_Slope := Float'Last; -- Vertical line
                           elsif abs (Cos (Theta)) < 0.001 then
                              Y1         := Integer (Rho);
                              Y2         := Y1;
                              X1         := 1;
                              X2         := Integer (Width);
                              Line_Slope := 0.0; -- Horizontal line
                           else
                              X1 := 1;
                              Y1 :=
                                Integer
                                  ((Rho - Float (X1) * Cos (Theta)) /
                                   Sin (Theta));
                              X2 := Integer (Width);
                              Y2 :=
                                Integer
                                  ((Rho - Float (X2) * Cos (Theta)) /
                                   Sin (Theta));

                              -- Calculate slope
                              if X2 /= X1 then
                                 Line_Slope :=
                                   Float (Y2 - Y1) / Float (X2 - X1);
                              else
                                 Line_Slope := Float'Last;
                              end if;
                           end if;

                           -- Only consider lines with reasonable slopes for lanes
                           -- Classify as left or right lane based on angle
                           if Theta >= Left_Lane_Min_Angle and
                             Theta <= Left_Lane_Max_Angle and Line_Slope < 0.0
                           then -- Left lane (negative slope)
                              Left_Count := Left_Count + 1;
                              Left_Lane_Candidates (Left_Count) :=
                                (Rho   => Rho, Theta => Theta,
                                 Votes => Acc (R, T), X1 => X1, Y1 => Y1,
                                 X2    => X2, Y2 => Y2, Slope => Line_Slope);
                           elsif Theta >= Right_Lane_Min_Angle and
                             Theta <= Right_Lane_Max_Angle and Line_Slope > 0.0
                           then -- Right lane (positive slope)
                              Right_Count := Right_Count + 1;
                              Right_Lane_Candidates (Right_Count) :=
                                (Rho   => Rho, Theta => Theta,
                                 Votes => Acc (R, T), X1 => X1, Y1 => Y1,
                                 X2    => X2, Y2 => Y2, Slope => Line_Slope);
                           end if;

                           -- Also store in general lines array for drawing
                           Line_Count         := Line_Count + 1;
                           Lines (Line_Count) :=
                             (Rho => Rho, Theta => Theta, Votes => Acc (R, T),
                              X1    => X1, Y1 => Y1, X2 => X2, Y2 => Y2,
                              Slope => Line_Slope);
                        end;
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         -- Find best left and right lane lines (highest vote count)
         declare
            Best_Left_Votes  : Natural := 0;
            Best_Right_Votes : Natural := 0;
         begin
            -- Find best left lane
            for I in 1 .. Left_Count loop
               if Left_Lane_Candidates (I).Votes > Best_Left_Votes then
                  Best_Left_Votes := Left_Lane_Candidates (I).Votes;
                  Left_Line       :=
                    (Rho   => Left_Lane_Candidates (I).Rho,
                     Theta => Left_Lane_Candidates (I).Theta,
                     X1    => Left_Lane_Candidates (I).X1,
                     Y1    => Left_Lane_Candidates (I).Y1,
                     X2    => Left_Lane_Candidates (I).X2,
                     Y2    => Left_Lane_Candidates (I).Y2);
                  Left_Line_Found := True;
               end if;
            end loop;

            -- Find best right lane
            for I in 1 .. Right_Count loop
               if Right_Lane_Candidates (I).Votes > Best_Right_Votes then
                  Best_Right_Votes := Right_Lane_Candidates (I).Votes;
                  Right_Line       :=
                    (Rho   => Right_Lane_Candidates (I).Rho,
                     Theta => Right_Lane_Candidates (I).Theta,
                     X1    => Right_Lane_Candidates (I).X1,
                     Y1    => Right_Lane_Candidates (I).Y1,
                     X2    => Right_Lane_Candidates (I).X2,
                     Y2    => Right_Lane_Candidates (I).Y2);
                  Right_Line_Found := True;
               end if;
            end loop;
         end;

         Draw_Line
           (Data.all, Left_Line.X1, Left_Line.Y1, Left_Line.X2, Left_Line.Y2,
            Width, Height, Channels);
         Draw_Line
           (Data.all, Right_Line.X1, Right_Line.Y1, Right_Line.X2,
            Right_Line.Y2, Width, Height, Channels);

         --  -- Draw detected lines on the image
         --  for I in 1 .. Line_Count loop
         --     -- Draw line on the image
         --     Draw_Line(Data.all,
         --               Lines(I).X1, Lines(I).Y1,
         --               Lines(I).X2, Lines(I).Y2,
         --               Width, Height, Channels);
         --  end loop;
      end;

      -- Free accumulator memory to prevent leaks
      declare
         procedure Free_Acc is new Ada.Unchecked_Deallocation
           (Object => Accumulator_Array, Name => Accumulator_Access);
         Temp : Accumulator_Access := Acc;
      begin
         Free_Acc (Temp);
      end;
   end Hough_Line_Transform;

   ------------------------------------------------------------------------------
   -- Hough_Circle_Transform
   --
   -- Applies the Hough Transform to detect circles in a binary image.
   --
   -- Parameters:
   --   Data        - Image data array (modified in-place to draw circles)
   --   Width, Height - Dimensions of the image
   --   Channels    - Number of channels per pixel (e.g., 3 for RGB)
   --   Min_Radius  - Minimum radius of circles to detect
   --   Max_Radius  - Maximum radius of circles to detect
   --   Threshold   - Minimum votes required to consider a valid circle
   --   Max_Circles - Maximum number of circles to detect (default 10)
   --
   -- Description:
   --   This procedure uses a 3D accumulator to identify circles by voting on
   --   potential center coordinates (x, y) and radius (r). Circles are detected
   --   as peaks in the accumulator, with non-maximal suppression to refine results.
   --   The detected circles are drawn onto the image.
   ------------------------------------------------------------------------------
   procedure Hough_Circle_Transform
     (Input       : in out Input_Data; Min_Radius : Storage_Count;
      Max_Radius  :        Storage_Count; Threshold : Natural;
      Max_Circles :        Positive := 10)
   is
      -- Extract necessary parameters from Input.Desc
      Width    : constant Storage_Count := Storage_Count (Input.Desc.Width);
      Height   : constant Storage_Count := Storage_Count (Input.Desc.Height);
      Channels : constant Storage_Count := Storage_Count (Input.Desc.Channels);

      -- Get direct reference to the data array
      Data : Storage_Array_Access := Input.Data;

      -- 3D accumulator array (x, y, r)
      type Accumulator_Array is
        array
          (Storage_Count range <>, Storage_Count range <>,
           Storage_Count range <>) of Natural;
      type Accumulator_Access is access Accumulator_Array;

      Acc :
        Accumulator_Array
          (1 .. Width, 1 .. Height, Min_Radius .. Max_Radius) :=
        (others => (others => (others => 0)));

      Circles : Circle_Array_Access := new Circle_Array (1 .. Max_Circles);
      Circle_Count : Natural             := 0;

      Angle_Steps : constant := 360;
      Deg_To_Rad  : constant := Ada.Numerics.Pi / 180.0;

   begin
      Put_Line ("Vo");
      -- Voting process
      for Y in 1 .. Height loop
         for X in 1 .. Width loop
            if Data (((Y - 1) * Width * Channels + (X - 1)) * Channels + 1) > 0
            then
               for R in Min_Radius .. Max_Radius loop
                  for Theta in 0 .. Angle_Steps - 1 loop
                     declare
                        Angle : constant Float   := Float (Theta) * Deg_To_Rad;
                        A     : constant Integer :=
                          Integer (Float (X) - Float (R) * Cos (Angle));
                        B     : constant Integer :=
                          Integer (Float (Y) - Float (R) * Sin (Angle));
                     begin
                        if A > 0 and A <= Integer (Width) and B > 0 and
                          B <= Integer (Height)
                        then
                           Acc (Storage_Count (A), Storage_Count (B), R) :=
                             Acc (Storage_Count (A), Storage_Count (B), R) + 1;
                        end if;
                     end;
                  end loop;
               end loop;
            end if;
         end loop;
      end loop;

      -- Find circles (local maxima in accumulator)
      for R in Min_Radius .. Max_Radius loop
         for Y in 2 .. Height - 1 loop
            for X in 2 .. Width - 1 loop
               declare
                  Center_Value : constant Natural := Acc (X, Y, R);
               begin
                  if Center_Value > Threshold then
                     declare
                        Is_Maximum : Boolean := True;
                     begin
                        -- Check 3x3x3 neighborhood
                        for DZ in -1 .. 1 loop
                           declare
                              Current_R : constant Integer := Integer (R) + DZ;
                           begin
                              if Current_R >= Integer (Min_Radius) and
                                Current_R <= Integer (Max_Radius)
                              then
                                 for DY in -1 .. 1 loop
                                    for DX in -1 .. 1 loop
                                       declare
                                          Current_X : constant Integer :=
                                            Integer (X) + DX;
                                          Current_Y : constant Integer :=
                                            Integer (Y) + DY;
                                       begin
                                          if Current_X > 0 and
                                            Current_X <= Integer (Width) and
                                            Current_Y > 0 and
                                            Current_Y <= Integer (Height)
                                          then
                                             if Acc
                                                 (Storage_Count (Current_X),
                                                  Storage_Count (Current_Y),
                                                  Storage_Count (Current_R)) >
                                               Center_Value
                                             then
                                                Is_Maximum := False;
                                                exit;
                                             end if;
                                          end if;
                                       end;
                                    end loop;
                                    exit when not Is_Maximum;
                                 end loop;
                              end if;
                              exit when not Is_Maximum;
                           end;
                        end loop;

                        if Is_Maximum and Circle_Count < Max_Circles then
                           Circle_Count           := Circle_Count + 1;
                           Circles (Circle_Count) := (X, Y, R, Center_Value);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end loop;
      end loop;

      -- Draw detected circles
      for I in 1 .. Circle_Count loop
         declare
            Circle : Circle_Parameters renames Circles (I);

            -- Function to safely convert coordinates
            function Safe_Convert (X, Y : Integer) return Storage_Count is
            begin
               if X > 0 and X <= Integer (Width) and Y > 0 and
                 Y <= Integer (Height)
               then
                  return
                    ((Storage_Count (Y) - 1) * Width +
                     (Storage_Count (X) - 1)) *
                    Channels +
                    1;
               else
                  return 1; -- Return safe default if out of bounds
               end if;
            end Safe_Convert;

         begin
            -- Draw circle using Bresenham's circle algorithm
            for Theta in 0 .. Angle_Steps - 1 loop
               declare
                  Angle    : constant Float   := Float (Theta) * Deg_To_Rad;
                  -- Use Integer for intermediate calculations
                  Center_X : constant Integer := Integer (Circle.X);
                  Center_Y : constant Integer := Integer (Circle.Y);
                  Radius   : constant Integer := Integer (Circle.R);
                  -- Calculate point coordinates
                  X        : constant Integer :=
                    Center_X + Integer (Float (Radius) * Cos (Angle));
                  Y        : constant Integer :=
                    Center_Y + Integer (Float (Radius) * Sin (Angle));
               begin
                  -- Only draw if point is within image bounds
                  if X > 0 and X <= Integer (Width) and Y > 0 and
                    Y <= Integer (Height)
                  then
                     -- Calculate array index safely
                     declare
                        Base_Index : constant Storage_Count :=
                          ((Storage_Count (Y) - 1) * Width +
                           (Storage_Count (X) - 1)) *
                          Channels;
                     begin
                        Data (Base_Index + 1) := 0;      -- Red channel
                        Data (Base_Index + 2) := 100;    -- Green channel
                        Data (Base_Index + 3) := 255;    -- Blue channel
                        -- Preserve alpha channel if it exists
                        if Channels = 4 then
                           Data (Base_Index + 4) := Data (Base_Index + 4);
                        end if;
                     end;
                  end if;
               end;
            end loop;
         end;
      end loop;

      -- Free memory
      declare
         procedure Free_Circles is new Ada.Unchecked_Deallocation
           (Object => Circle_Array, Name => Circle_Array_Access);
         Temp_Circles : Circle_Array_Access := Circles;
      begin
         Free_Circles (Temp_Circles);
      end;
   end Hough_Circle_Transform;

end CV_Ada.Hough_Transform;

--  with CV_Ada.Graphics;               use CV_Ada.Graphics;
--  with CV_Ada.Graphics.Pixel;         use CV_Ada.Graphics.Pixel;
--  with Ada.Unchecked_Deallocation;
--  with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

--  package body CV_Ada.Hough_Transform is
--     ------------------------------------------------------------------------------
--     -- Hough_Line_Transform
--     --
--     -- Applies the Hough Transform to detect straight lines in a binary image.
--     --
--     -- Parameters:
--     --   Data             - Image data array (modified in-place to draw lines)
--     --   Width, Height    - Dimensions of the image
--     --   Channels         - Number of channels per pixel (e.g., 3 for RGB)
--     --   Theta_Resolution - Angular resolution for line detection (default 180)
--     --   Rho_Resolution   - Distance resolution for line detection (default 180)
--     --
--     -- Description:
--     --   This procedure uses an accumulator to identify lines by voting on
--     --   rho (distance) and theta (angle) values. Lines are detected as peaks
--     --   in the accumulator, with non-maximal suppression to refine results.
--     ------------------------------------------------------------------------------
--     procedure Hough_Line_Transform
--       (Data : in out Storage_Array; Width, Height, Channels : Storage_Count;
--        Theta_Resolution :    Positive := 180; Rho_Resolution : Positive := 180)
--     is
--        -- Calculate maximum possible rho value (image diagonal length)
--        Max_Rho : constant Float :=
--          Sqrt (Float (Width * Width + Height * Height));

--        -- Initialize accumulator array for line voting
--        Acc : constant Accumulator_Access :=
--          new Accumulator_Array (1 .. Rho_Resolution, 1 .. Theta_Resolution);

--        -- Conversion constants
--        Deg_To_Rad : constant Float := Ada.Numerics.Pi / 180.0;
--        Rho_Scale  : constant Float := Float (Rho_Resolution) / (2.0 * Max_Rho);

--        -- Parameters for line filtering
--        Max_Lines       : constant Positive := 10; -- Max lines to detect
--        Min_Line_Length : constant Float    := Float (Width + Height) / 8.0;
--        Max_Line_Gap    : constant Float    := Float (Width + Height) / 16.0;
--        Angle_Threshold : constant Float    := 10.0 * Deg_To_Rad;
--     begin
--        -- Initialize accumulator to zero
--        for R in Acc'Range (1) loop
--           for T in Acc'Range (2) loop
--              Acc (R, T) := 0;
--           end loop;
--        end loop;

--        -- Voting process: for each pixel, vote in the accumulator if it's part of a line
--        for Y in 1 .. Height loop
--           for X in 1 .. Width loop
--              if Get_Pixel (Data, X, Y, Width, Height, Channels) > 0 then
--                 -- Loop through theta values and compute corresponding rho
--                 for T in 0 .. Theta_Resolution - 1 loop
--                    declare
--                       Theta   : constant Float   := Float (T) * Deg_To_Rad;
--                       Rho     : constant Float   :=
--                         Float (X) * Cos (Theta) + Float (Y) * Sin (Theta);
--                       Rho_Idx : constant Integer :=
--                         Integer ((Rho + Max_Rho) * Rho_Scale);
--                    begin
--                       -- Vote if rho index is within valid range
--                       if Rho_Idx > 0 and Rho_Idx <= Rho_Resolution then
--                          Acc (Rho_Idx, T + 1) := Acc (Rho_Idx, T + 1) + 1;
--                       end if;
--                    end;
--                 end loop;
--              end if;
--           end loop;
--        end loop;

--        -- Detect peaks in accumulator using non-maximal suppression
--        declare
--           Threshold : constant Natural :=
--             Natural
--               (0.85 * Float (Width + Height) /
--                2.0); -- Dynamic threshold

--           type Line_Info is record
--              Rho, Theta : Float;
--              Votes      : Natural;
--           end record;

--           -- Array to store detected lines
--           type Line_Array is array (1 .. Max_Lines) of Line_Info;
--           Lines      : Line_Array;
--           Line_Count : Natural := 0;

--           -- Non-maximal suppression window size
--           Window_Size : constant Positive := 5;
--        begin
--           -- Identify local maxima in accumulator
--           for R in Window_Size + 1 .. Rho_Resolution - Window_Size loop
--              for T in Window_Size + 1 .. Theta_Resolution - Window_Size loop
--                 if Acc (R, T) > Threshold then
--                    declare
--                       Is_Maximum : Boolean := True;
--                    begin
--                       -- Check surrounding cells to ensure local maximum
--                       for DR in -Window_Size .. Window_Size loop
--                          for DT in -Window_Size .. Window_Size loop
--                             if DR /= 0 or DT /= 0 then
--                                if Acc (R + DR, T + DT) >= Acc (R, T) then
--                                   Is_Maximum := False;
--                                   exit;
--                                end if;
--                             end if;
--                          end loop;
--                          exit when not Is_Maximum;
--                       end loop;

--                       -- Store line if it's a local maximum and within max line limit
--                       if Is_Maximum and Line_Count < Max_Lines then
--                          Line_Count         := Line_Count + 1;
--                          Lines (Line_Count) :=
--                            (Rho   => (Float (R) / Rho_Scale) - Max_Rho,
--                             Theta => Float (T - 1) * Deg_To_Rad,
--                             Votes => Acc (R, T));
--                       end if;
--                    end;
--                 end if;
--              end loop;
--           end loop;

--           -- Draw detected lines on the image
--           for I in 1 .. Line_Count loop
--              declare
--                 Theta          : constant Float := Lines (I).Theta;
--                 Rho            : constant Float := Lines (I).Rho;
--                 X1, Y1, X2, Y2 : Integer;
--              begin
--                 -- Calculate endpoints of each line based on rho and theta
--                 if abs (Sin (Theta)) < 0.001 then
--                    X1 := Integer (Rho);
--                    X2 := X1;
--                    Y1 := 1;
--                    Y2 := Integer (Height);
--                 elsif abs (Cos (Theta)) < 0.001 then
--                    Y1 := Integer (Rho);
--                    Y2 := Y1;
--                    X1 := 1;
--                    X2 := Integer (Width);
--                 else
--                    X1 := 1;
--                    Y1 :=
--                      Integer ((Rho - Float (X1) * Cos (Theta)) / Sin (Theta));
--                    X2 := Integer (Width);
--                    Y2 :=
--                      Integer ((Rho - Float (X2) * Cos (Theta)) / Sin (Theta));
--                 end if;

--                 -- Draw line on the image
--                 Draw_Line (Data, X1, Y1, X2, Y2, Width, Height, Channels);
--              end;
--           end loop;
--        end;

--        -- Free accumulator memory to prevent leaks
--        declare
--           procedure Free is new Ada.Unchecked_Deallocation
--             (Object => Accumulator_Array, Name => Accumulator_Access);
--           Temp : Accumulator_Access := Acc;
--        begin
--           Free (Temp);
--        end;
--     end Hough_Line_Transform;

--     ------------------------------------------------------------------------------
--     -- Hough_Circle_Transform
--     --
--     -- Applies the Hough Transform to detect circles in a binary image.
--     --
--     -- Parameters:
--     --   Data        - Image data array (modified in-place to draw circles)
--     --   Width, Height - Dimensions of the image
--     --   Channels    - Number of channels per pixel (e.g., 3 for RGB)
--     --   Min_Radius  - Minimum radius of circles to detect
--     --   Max_Radius  - Maximum radius of circles to detect
--     --   Threshold   - Minimum votes required to consider a valid circle
--     --   Max_Circles - Maximum number of circles to detect (default 10)
--     --
--     -- Description:
--     --   This procedure uses a 3D accumulator to identify circles by voting on
--     --   potential center coordinates (x, y) and radius (r). Circles are detected
--     --   as peaks in the accumulator, with non-maximal suppression to refine results.
--     --   The detected circles are drawn onto the image.
--     ------------------------------------------------------------------------------
--     procedure Hough_Circle_Transform
--       (Data       : in out Storage_Array; Width : Storage_Count;
--        Height     :        Storage_Count; Channels : Storage_Count;
--        Min_Radius :        Storage_Count; Max_Radius : Storage_Count;
--        Threshold  :        Natural; Max_Circles : Positive := 10)
--     is

--        -- 3D accumulator array (x, y, r)
--        type Accumulator_Array is
--          array
--            (Storage_Count range <>, Storage_Count range <>,
--             Storage_Count range <>) of Natural;
--        type Accumulator_Access is access Accumulator_Array;

--        Acc :
--          Accumulator_Array
--            (1 .. Width, 1 .. Height, Min_Radius .. Max_Radius) :=
--          (others => (others => (others => 0)));

--        Circles : Circle_Array_Access := new Circle_Array (1 .. Max_Circles);
--        Circle_Count : Natural             := 0;

--        Angle_Steps : constant := 360;
--        Deg_To_Rad  : constant := Ada.Numerics.Pi / 180.0;

--     begin

--        -- Voting process
--        for Y in 1 .. Height loop
--           for X in 1 .. Width loop
--              if Data (((Y - 1) * Width * Channels + (X - 1)) * Channels + 1) > 0
--              then
--                 for R in Min_Radius .. Max_Radius loop
--                    for Theta in 0 .. Angle_Steps - 1 loop
--                       declare
--                          Angle : constant Float   := Float (Theta) * Deg_To_Rad;
--                          A     : constant Integer :=
--                            Integer (Float (X) - Float (R) * Cos (Angle));
--                          B     : constant Integer :=
--                            Integer (Float (Y) - Float (R) * Sin (Angle));
--                       begin
--                          if A > 0 and A <= Integer (Width) and B > 0 and
--                            B <= Integer (Height)
--                          then
--                             Acc (Storage_Count (A), Storage_Count (B), R) :=
--                               Acc (Storage_Count (A), Storage_Count (B), R) + 1;
--                          end if;
--                       end;
--                    end loop;
--                 end loop;
--              end if;
--           end loop;
--        end loop;

--        -- Find circles (local maxima in accumulator)
--        for R in Min_Radius .. Max_Radius loop
--           for Y in 2 .. Height - 1 loop
--              for X in 2 .. Width - 1 loop
--                 declare
--                    Center_Value : constant Natural := Acc (X, Y, R);
--                 begin
--                    if Center_Value > Threshold then
--                       declare
--                          Is_Maximum : Boolean := True;
--                       begin
--                          -- Check 3x3x3 neighborhood
--                          for DZ in -1 .. 1 loop
--                             declare
--                                Current_R : constant Integer := Integer (R) + DZ;
--                             begin
--                                if Current_R >= Integer (Min_Radius) and
--                                  Current_R <= Integer (Max_Radius)
--                                then
--                                   for DY in -1 .. 1 loop
--                                      for DX in -1 .. 1 loop
--                                         declare
--                                            Current_X : constant Integer :=
--                                              Integer (X) + DX;
--                                            Current_Y : constant Integer :=
--                                              Integer (Y) + DY;
--                                         begin
--                                            if Current_X > 0 and
--                                              Current_X <= Integer (Width) and
--                                              Current_Y > 0 and
--                                              Current_Y <= Integer (Height)
--                                            then
--                                               if Acc
--                                                   (Storage_Count (Current_X),
--                                                    Storage_Count (Current_Y),
--                                                    Storage_Count (Current_R)) >
--                                                 Center_Value
--                                               then
--                                                  Is_Maximum := False;
--                                                  exit;
--                                               end if;
--                                            end if;
--                                         end;
--                                      end loop;
--                                      exit when not Is_Maximum;
--                                   end loop;
--                                end if;
--                                exit when not Is_Maximum;
--                             end;
--                          end loop;

--                          if Is_Maximum and Circle_Count < Max_Circles then
--                             Circle_Count           := Circle_Count + 1;
--                             Circles (Circle_Count) := (X, Y, R, Center_Value);
--                          end if;
--                       end;
--                    end if;
--                 end;
--              end loop;
--           end loop;
--        end loop;

--        -- Draw detected circles
--        for I in 1 .. Circle_Count loop
--           declare
--              Circle : Circle_Parameters renames Circles (I);

--              -- Function to safely convert coordinates
--              function Safe_Convert (X, Y : Integer) return Storage_Count is
--              begin
--                 if X > 0 and X <= Integer (Width) and Y > 0 and
--                   Y <= Integer (Height)
--                 then
--                    return
--                      ((Storage_Count (Y) - 1) * Width +
--                       (Storage_Count (X) - 1)) *
--                      Channels +
--                      1;
--                 else
--                    return 1; -- Return safe default if out of bounds
--                 end if;
--              end Safe_Convert;

--           begin
--              -- Draw circle using Bresenham's circle algorithm
--              for Theta in 0 .. Angle_Steps - 1 loop
--                 declare
--                    Angle    : constant Float   := Float (Theta) * Deg_To_Rad;
--                    -- Use Integer for intermediate calculations
--                    Center_X : constant Integer := Integer (Circle.X);
--                    Center_Y : constant Integer := Integer (Circle.Y);
--                    Radius   : constant Integer := Integer (Circle.R);
--                    -- Calculate point coordinates
--                    X        : constant Integer :=
--                      Center_X + Integer (Float (Radius) * Cos (Angle));
--                    Y        : constant Integer :=
--                      Center_Y + Integer (Float (Radius) * Sin (Angle));
--                 begin
--                    -- Only draw if point is within image bounds
--                    if X > 0 and X <= Integer (Width) and Y > 0 and
--                      Y <= Integer (Height)
--                    then
--                       -- Calculate array index safely
--                       declare
--                          Base_Index : constant Storage_Count :=
--                            ((Storage_Count (Y) - 1) * Width +
--                             (Storage_Count (X) - 1)) *
--                            Channels;
--                       begin
--                          Data (Base_Index + 1) := 0;    -- Red channel
--                          Data (Base_Index + 2) := 100;    -- Green channel
--                          Data (Base_Index + 3) := 255;  -- Blue channel
--                          -- Preserve alpha channel if it exists
--                          if Channels = 4 then
--                             Data (Base_Index + 4) := Data (Base_Index + 4);
--                          end if;
--                       end;
--                    end if;
--                 end;
--              end loop;
--           end;
--        end loop;

--        -- Free memory
--        declare
--           procedure Free is new Ada.Unchecked_Deallocation
--             (Object => Accumulator_Array, Name => Accumulator_Access);
--           procedure Free is new Ada.Unchecked_Deallocation
--             (Object => Circle_Array, Name => Circle_Array_Access);
--           --Temp_Acc     : Accumulator_Access := Acc;
--           Temp_Circles : Circle_Array_Access := Circles;
--        begin
--           --Free (Temp_Acc);
--           Free (Temp_Circles);
--        end;
--     end Hough_Circle_Transform;
--  end CV_Ada.Hough_Transform;
