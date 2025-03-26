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
   --   Input          - Image data array (modified in-place to draw circles)
   --   Min_Radius     - Minimum radius to detect (default 10)
   --   Max_Radius     - Maximum radius to detect (default 100)
   --   Radius_Step    - Step size for radius iteration (default 1)
   --   Vote_Threshold - Minimum votes needed to consider a circle (default 100)
   --   Max_Circles    - Maximum number of circles to detect (default 5)
   --   Circles        - Output array of detected circles
   --
   -- Description:
   --   This procedure uses a 3D accumulator (x, y, radius) to identify circles
   --   by voting on potential circle centers and radii. Circles are detected as
   --   peaks in the accumulator, with non-maximal suppression to refine results.
   ------------------------------------------------------------------------------
   procedure Hough_Circle_Transform
     (Input          : in out Input_Data; Min_Radius : Positive := 10;
      Max_Radius     :        Positive := 100; Radius_Step : Positive := 1;
      Vote_Threshold :        Positive := 100; Max_Circles : Positive := 5;
      Circles        :    out Circle_Array_Access)
   is
      -- Extract necessary parameters from Input.Desc
      Width    : constant Storage_Count := Input.Desc.Width;
      Height   : constant Storage_Count := Input.Desc.Height;
      Channels : constant Storage_Count := Input.Desc.Channels;

      -- Get direct reference to the data array
      Data : Storage_Array_Access := Input.Data;

      -- Calculate number of radius steps
      Radius_Count : constant Positive :=
        (Max_Radius - Min_Radius) / Radius_Step + 1;

      -- Initialize 3D accumulator array for circle voting - Use a more compact accumulator
      -- Allocate dynamically to avoid stack overflow and improve memory usage
      Acc : constant Circle_Accumulator_Access :=
        new Circle_Accumulator_Array
          (1 .. Integer (Width), 1 .. Integer (Height), 1 .. Radius_Count);

      -- Pre-compute sine and cosine values to avoid redundant calculations
      type Angle_Lookup_Table is array (0 .. 359) of Float;
      Sin_Table, Cos_Table : Angle_Lookup_Table;
      
      -- Adaptive angle step based on radius for optimization
      function Get_Angle_Step(Radius: Integer) return Float is
      begin
         -- Smaller radii need more precise angles, larger can use fewer steps
         if Radius < 20 then
            return 0.05;
         elsif Radius < 50 then
            return 0.1;
         else
            return 0.2;
         end if;
      end Get_Angle_Step;

      -- Helper function to get radius index in accumulator
      function Radius_To_Index (R : Integer) return Integer is
      begin
         return (R - Min_Radius) / Radius_Step + 1;
      end Radius_To_Index;

      -- Helper function to get radius from index
      function Index_To_Radius (I : Integer) return Integer is
      begin
         return (I - 1) * Radius_Step + Min_Radius;
      end Index_To_Radius;

      -- Array to store detected circles temporarily
      type Circle_Vote_Array is array (1 .. Max_Circles) of Circle_Parameters;
      Circle_Votes : Circle_Vote_Array;
      Circle_Count : Natural := 0;

      -- Window size for non-maximal suppression
      Window_Size : constant Positive := 5;
      
      -- Dynamically allocated arrays for optimization
      type Edge_Point_Record is record
         X, Y : Integer;
      end record;
      type Edge_Point_Array is array (Positive range <>) of Edge_Point_Record;
      type Edge_Point_Access is access Edge_Point_Array;
      Edge_Points : Edge_Point_Access := new Edge_Point_Array(1 .. Integer(Width * Height));
      Edge_Point_Count : Natural := 0;
   begin
      -- Pre-compute sine and cosine values
      for I in Sin_Table'Range loop
         declare
            Angle : constant Float := Float(I) * Ada.Numerics.Pi / 180.0;
         begin
            Sin_Table(I) := Sin(Angle);
            Cos_Table(I) := Cos(Angle);
         end;
      end loop;

      -- Initialize accumulator to zero
      -- Using block structure for better cache locality
      for X in Acc'Range (1) loop
         for Y in Acc'Range (2) loop
            for R in Acc'Range (3) loop
               Acc (X, Y, R) := 0;
            end loop;
         end loop;
      end loop;
      
      -- First pass: Collect edge points to avoid redundant checks
      for Y in 1 .. Integer (Height) loop
         for X in 1 .. Integer (Width) loop
            if Get_Pixel(Data.all, Storage_Count (X), Storage_Count (Y), 
                        Width, Height, Channels) > 0 then
               Edge_Point_Count := Edge_Point_Count + 1;
               Edge_Points(Edge_Point_Count) := (X => X, Y => Y);
            end if;
         end loop;
      end loop;

      -- Voting process: use collected edge points for more efficient processing
      for Point_Idx in 1 .. Edge_Point_Count loop
         declare
            X : constant Integer := Edge_Points(Point_Idx).X;
            Y : constant Integer := Edge_Points(Point_Idx).Y;
         begin
            -- For each possible radius, vote for all points that could be centers
            for R_idx in 1 .. Radius_Count loop
               declare
                  R : constant Integer := Index_To_Radius (R_idx);
                  Angle_Step : constant Float := Get_Angle_Step(R); -- Adaptive step
                  Theta_Steps : constant Integer := 
                    Integer(2.0 * Ada.Numerics.Pi / Angle_Step);
                  
                  -- Optimized voting: vote for fewer points with larger radii
                  Vote_Step : constant Positive := 
                    (if R < 30 then 1 else (if R < 60 then 2 else 3));
               begin
                  -- Optimize: For large radiuses, sample fewer points on the circle
                  for I in 0 .. Theta_Steps - 1 loop
                     if I mod Vote_Step = 0 then -- Skip points based on radius size
                        declare
                           Angle_Deg : constant Integer := Integer(Float(I) * 180.0 / 
                                                         Float(Theta_Steps)) mod 360;
                           -- Use pre-computed sine/cosine values
                           A : constant Integer := X - Integer(Float(R) * Cos_Table(Angle_Deg));
                           B : constant Integer := Y - Integer(Float(R) * Sin_Table(Angle_Deg));
                        begin
                           -- Check if the potential center is within image bounds
                           if A >= 1 and A <= Integer (Width) and 
                              B >= 1 and B <= Integer (Height) then
                              -- Vote for this center and radius
                              Acc (A, B, R_idx) := Acc (A, B, R_idx) + 1;
                           end if;
                        end;
                     end if;
                  end loop;
               end;
            end loop;
         end;
      end loop;

      -- Detect peaks in accumulator using optimized non-maximal suppression
      -- Adjust window size based on radius for better accuracy/speed tradeoff
      for X in Window_Size + 1 .. Integer (Width) - Window_Size loop
         for Y in Window_Size + 1 .. Integer (Height) - Window_Size loop
            for R_idx in 1 .. Radius_Count loop
               -- Early threshold check to skip unnecessary processing
               if Acc (X, Y, R_idx) > Vote_Threshold then
                  declare
                     Is_Maximum : Boolean := True;
                     R : constant Integer := Index_To_Radius(R_idx);
                     -- Adjust window size based on radius
                     Local_Window : constant Integer := 
                       Integer'Min(Window_Size, Integer'Max(2, R / 10));
                  begin
                     -- Check surrounding cells to ensure local maximum
                     -- Early exit optimization
                     Outer_Loop:
                     for DX in -Local_Window .. Local_Window loop
                        for DY in -Local_Window .. Local_Window loop
                           for DR in -1 .. 1 loop
                              if DX /= 0 or DY /= 0 or DR /= 0 then
                                 declare
                                    NX : constant Integer := X + DX;
                                    NY : constant Integer := Y + DY;
                                    NR : constant Integer := R_idx + DR;
                                 begin
                                    if NX >= 1 and NX <= Integer (Width) and
                                       NY >= 1 and NY <= Integer (Height) and
                                       NR >= 1 and NR <= Radius_Count then
                                       if Acc (NX, NY, NR) >= Acc (X, Y, R_idx) then
                                          Is_Maximum := False;
                                          exit Outer_Loop;  -- Early exit
                                       end if;
                                    end if;
                                 end;
                              end if;
                           end loop;
                        end loop;
                     end loop Outer_Loop;

                     -- If this is a local maximum and there's room, add to detected circles
                     if Is_Maximum then
                        declare
                           Votes : constant Natural := Acc (X, Y, R_idx);
                           R     : constant Integer := Index_To_Radius (R_idx);
                        begin
                           -- Add circle or replace weakest one
                           if Circle_Count < Max_Circles then
                              Circle_Count := Circle_Count + 1;
                              Circle_Votes (Circle_Count) := 
                                (X => X, Y => Y, Radius => R, Votes => Votes);
                           else
                              -- Find and replace the weakest circle if this one is stronger
                              declare
                                 Min_Index : Integer := 1;
                                 Min_Votes : Natural := Circle_Votes(1).Votes;
                              begin
                                 for I in 2 .. Max_Circles loop
                                    if Circle_Votes (I).Votes < Min_Votes then
                                       Min_Votes := Circle_Votes (I).Votes;
                                       Min_Index := I;
                                    end if;
                                 end loop;

                                 if Votes > Min_Votes then
                                    Circle_Votes (Min_Index) := 
                                      (X => X, Y => Y, Radius => R, Votes => Votes);
                                 end if;
                              end;
                           end if;
                        end;
                     end if;
                  end;
               end if;
            end loop;
         end loop;
      end loop;

      -- Simplified sorting since we're maintaining a small list
      -- Use insertion sort for small arrays (fast for Max_Circles typically being small)
      for I in 2 .. Circle_Count loop
         declare
            J    : Integer := I;
            Temp : Circle_Parameters := Circle_Votes (I);
         begin
            while J > 1 and then Circle_Votes (J - 1).Votes < Temp.Votes loop
               Circle_Votes (J) := Circle_Votes (J - 1);
               J := J - 1;
            end loop;
            Circle_Votes (J) := Temp;
         end;
      end loop;

      -- Allocate and populate output array with detected circles
      Circles := new Circle_Array (1 .. Circle_Count);
      for I in 1 .. Circle_Count loop
         Circles (I) := Circle_Votes (I);

         -- Draw circles on the image for visualization
         Draw_Circle(Data.all, Storage_Count(Circle_Votes (I).X),
                    Storage_Count(Circle_Votes (I).Y),
                    Storage_Count(Circle_Votes (I).Radius), 
                    Width, Height, Channels);
      end loop;

      -- Free accumulator memory to prevent leaks
      declare
         procedure Free_Acc is new Ada.Unchecked_Deallocation
           (Object => Circle_Accumulator_Array,
            Name   => Circle_Accumulator_Access);
         Temp : Circle_Accumulator_Access := Acc;
      begin
         Free_Acc (Temp);
      end;
   end Hough_Circle_Transform;

end CV_Ada.Hough_Transform;
