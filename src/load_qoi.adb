with QOI;
with Ada.Text_IO;                       use Ada.Text_IO;
with System.Storage_Elements;           use System.Storage_Elements;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;

procedure Load_Qoi is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

   procedure Write_To_File
     (Filename : String; D : Storage_Array; Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;
      Ret : Integer;
   begin
      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Ret := Write (FD, D'Address, Integer (Size));

      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);
   end Write_To_File;

   function Load_QOI (Filename : String) return Input_Data is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;
      Ret : Integer;

      Result : Input_Data;
   begin
      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         Len     : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : constant Storage_Array_Access :=
           new Storage_Array (1 .. Len);
      begin
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         QOI.Get_Desc (In_Data.all, Result.Desc);

         declare
            Out_Len     : constant Storage_Count :=
              Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
            Out_Data    : constant Storage_Array_Access :=
              new Storage_Array (1 .. Out_Len);
            Output_Size : Storage_Count;
         begin
            QOI.Decode
              (Data        => In_Data.all,
               Desc        => Result.Desc,
               Output      => Out_Data.all,
               Output_Size => Output_Size);

            Result.Data := Out_Data;

            if Reference_QOI.Check_Decode
                 (In_Data.all,
                  Result.Desc,
                  Out_Data.all
                    (Out_Data'First .. Out_Data'First + Output_Size - 1))
            then
               Put_Line ("Compare with reference decoder: OK");
            else
               Put_Line ("Compare with reference decoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            return Result;
         end;
      end;
   end Load_QOI;

   -- Convert RGB(A) image to grayscale
   procedure Convert_To_Grayscale
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   is
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      Gray_Value : Storage_Element;
   begin
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if I mod Pixel_Size = 1 then
            -- Convert to grayscale using luminosity method
            -- Y = 0.299R + 0.587G + 0.114B
            Gray_Value :=
              Storage_Element
                ((Integer (Data (I)) * 299 + Integer (Data (I + 1)) * 587
                  + Integer (Data (I + 2)) * 114)
                 / 1000);

            -- Set RGB channels to the same gray value
            Data (I) := Gray_Value;     -- R
            Data (I + 1) := Gray_Value; -- G
            Data (I + 2) := Gray_Value; -- B

            -- Preserve alpha channel if it exists
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Grayscale;

   -- Add this procedure after Convert_To_Grayscale
   procedure Convert_To_Black_And_White
     (Data      : in out Storage_Array;
      Desc      : QOI.QOI_Desc;
      Threshold : Storage_Element := 128)
   is
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      BW_Value   : Storage_Element;
   begin
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if I mod Pixel_Size = 1 then
            -- Check if the pixel is above or below threshold
            -- Since image is already grayscale, we can just check one channel
            if Data (I) >= Threshold then
               BW_Value := 255; -- White

            else
               BW_Value := 0;   -- Black
            end if;

            -- Set RGB channels to either black or white
            Data (I) := BW_Value;     -- R
            Data (I + 1) := BW_Value; -- G
            Data (I + 2) := BW_Value; -- B

            -- Preserve alpha channel if it exists
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Black_And_White;

   -- Add blur functionality
   procedure Blur_Image
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      Temp        : Storage_Array := Data;
      Kernel_Size : constant Storage_Count := 6;
      Half_Kernel : constant Storage_Count := Kernel_Size / 2;
   begin
      for Y in Half_Kernel .. Height - Half_Kernel - 1 loop
         for X in Half_Kernel .. Width - Half_Kernel - 1 loop
            for C in 0 .. Channels - 1 loop
               declare
                  Sum   : Integer := 0;
                  Count : Integer := 0;
               begin
                  -- Apply box blur kernel
                  for KY in -Half_Kernel .. Half_Kernel loop
                     for KX in -Half_Kernel .. Half_Kernel loop
                        declare
                           Pos : constant Storage_Count :=
                             ((Y + KY) * Width + (X + KX)) * Channels + C + 1;
                        begin
                           Sum := Sum + Integer (Temp (Pos));
                           Count := Count + 1;
                        end;
                     end loop;
                  end loop;

                  -- Store averaged result
                  Data (((Y * Width) + X) * Channels + C + 1) :=
                    Storage_Element (Sum / Count);
               end;
            end loop;
         end loop;
      end loop;
   end Blur_Image;

   -- Add these procedures after your existing ones

   -- Helper function to get pixel value safely with bounds checking
   function Get_Pixel
     (Data                    : Storage_Array;
      X, Y                    : Storage_Count;
      Width, Height, Channels : Storage_Count) return Storage_Element
   is
      Index : Storage_Count;
   begin
      if X < 1 or X > Width or Y < 1 or Y > Height then
         return 0;
      end if;

      Index := ((Y - 1) * Width + (X - 1)) * Channels + 1;
      return Data (Index);
   end Get_Pixel;

   -- Sobel edge detection implementation
   procedure Sobel_Edge_Detection
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      Temp : Storage_Array := Data;

      -- Sobel kernels
      Gx : array (1 .. 3, 1 .. 3) of Integer :=
        ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1));

      Gy : array (1 .. 3, 1 .. 3) of Integer :=
        ((-1, -2, -1), (0, 0, 0), (1, 2, 1));

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
                            (Get_Pixel
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

   -- Add these type declarations after the existing ones
   type Accumulator_Cell is record
      Rho   : Integer;
      Theta : Integer;
      Votes : Natural;
   end record;

   type Accumulator_Array is
     array (Positive range <>, Positive range <>) of Natural;
   type Accumulator_Access is access Accumulator_Array;

   -- Add this procedure before Hough_Transform
   procedure Draw_Line
     (Data                    : in out Storage_Array;
      X1, Y1, X2, Y2          : Integer;
      Width, Height, Channels : Storage_Count;
      Color                   : Storage_Element := 255)
   is
      DX     : constant Integer := abs (X2 - X1);
      DY     : constant Integer := abs (Y2 - Y1);
      Step_X : constant Integer := (if X1 < X2 then 1 else -1);
      Step_Y : constant Integer := (if Y1 < Y2 then 1 else -1);
      Error  : Integer := (if DX > DY then DX else -DY) / 2;
      X      : Integer := X1;
      Y      : Integer := Y1;
   begin
      loop
         -- Draw pixel if within bounds
         if X > 0 and X <= Integer (Width) and Y > 0 and Y <= Integer (Height)
         then
            -- Set all channels to the line color
            for C in 0 .. Integer (Channels) - 1 loop
               declare
                  Index : constant Storage_Offset :=
                    Storage_Offset
                      (((Y - 1) * Integer (Width) + (X - 1))
                       * Integer (Channels)
                       + C
                       + 1);
               begin
                  Data (Index) := Color;
               end;
            end loop;
         end if;

         exit when X = X2 and Y = Y2;

         declare
            Error2 : constant Integer := Error;
         begin
            if Error2 > -DX then
               Error := Error - DY;
               X := X + Step_X;
            end if;
            if Error2 < DY then
               Error := Error + DX;
               Y := Y + Step_Y;
            end if;
         end;
      end loop;
   end Draw_Line;



   -- Add this procedure after Sobel_Edge_Detection
   procedure Hough_Transform
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
            if Get_Pixel (Data, X, Y, Width, Height, Channels) > 0 then
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
   end Hough_Transform;

   -- Add these procedures after your existing ones
   procedure Gaussian_Blur
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Sigma                   : Float := 1.4)
   is

      -- Calculate kernel size based on sigma (usually 6*sigma)
      Kernel_Size : constant Positive :=
        Positive (Float'Ceiling (6.0 * Sigma));
      Half_Size   : constant Integer := Kernel_Size / 2;

      -- Create temporary array for processing
      Temp : Storage_Array := Data;
   begin
      -- Create Gaussian kernel
      declare
         type Kernel_Type is
           array (Integer range <>, Integer range <>) of Float;
         Kernel :
           Kernel_Type (-Half_Size .. Half_Size, -Half_Size .. Half_Size);
         Sum    : Float := 0.0;
      begin
         -- Generate Gaussian kernel
         for Y in Kernel'Range(1) loop
            for X in Kernel'Range(2) loop
               Kernel (Y, X) :=
                 (1.0 / (2.0 * Ada.Numerics.Pi * Sigma * Sigma))
                 * Float
                     (Exp (-(Float (X * X + Y * Y) / (2.0 * Sigma * Sigma))));
               Sum := Sum + Kernel (Y, X);
            end loop;
         end loop;

         -- Normalize kernel
         for Y in Kernel'Range(1) loop
            for X in Kernel'Range(2) loop
               Kernel (Y, X) := Kernel (Y, X) / Sum;
            end loop;
         end loop;

         -- Apply convolution
         for Y in Half_Size + 1 .. Integer (Height) - Half_Size loop
            for X in Half_Size + 1 .. Integer (Width) - Half_Size loop
               for C in 0 .. Integer (Channels) - 1 loop
                  declare
                     Sum : Float := 0.0;
                  begin
                     for KY in Kernel'Range(1) loop
                        for KX in Kernel'Range(2) loop
                           declare
                              Idx : constant Storage_Count :=
                                ((Storage_Count (Y + KY - 1) * Width
                                  + Storage_Count (X + KX - 1))
                                 * Channels
                                 + Storage_Count (C)
                                 + 1);
                           begin
                              Sum :=
                                Sum + Float (Temp (Idx)) * Kernel (KY, KX);
                           end;
                        end loop;
                     end loop;
                     Data
                       (((Storage_Count (Y - 1) * Width
                          + Storage_Count (X - 1))
                         * Channels
                         + Storage_Count (C)
                         + 1)) :=
                       Storage_Element (Float'Rounding (Sum));
                  end;
               end loop;
            end loop;
         end loop;
      end;
   end Gaussian_Blur;

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

   Input : Input_Data;
begin
   Input := Load_QOI ("lane1.qoi");

   -- Convert to grayscale first
   Convert_To_Grayscale (Input.Data.all, Input.Desc);

   -- Blur the image
   --  Blur_Image
   --    (Input.Data.all,
   --     Input.Desc.Width,
   --     Input.Desc.Height,
   --     Input.Desc.Channels);

   -- Apply Sobel edge detection
   Sobel_Edge_Detection
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels);

   -- Apply Canny edge detection
   Canny_Edge_Detection
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels,
      Low_Threshold  => 0.1,
      High_Threshold => 0.3);

   -- Apply Hough Transform to detect lines
   Hough_Transform
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels);

   declare
      Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
      Output_Size : Storage_Count;
   begin
      QOI.Encode (Input.Data.all, Input.Desc, Output, Output_Size);
      Write_To_File ("output.qoi", Output, Output_Size);
   end;
end Load_Qoi;
