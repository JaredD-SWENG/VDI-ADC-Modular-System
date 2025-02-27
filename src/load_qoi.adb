with QOI;
with Ada.Text_IO;                       use Ada.Text_IO;
with System.Storage_Elements;           use System.Storage_Elements;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Simulated_Camera;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

procedure Load_Qoi is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

   -------------------------------------------------------------------------------
   -- Write_To_File
   --
   -- Writes binary data from a Storage_Array to a file
   --
   -- Parameters:
   --    Filename - Name of the file to write to
   --    D       - Storage_Array containing data to write
   --    Size    - Number of bytes to write from D
   --
   -- Returns:
   --    None. Exits program with code 1 if any errors occur during write
   -------------------------------------------------------------------------------
   procedure Write_To_File
     (Filename : String; D : Storage_Array; Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;  -- File descriptor for the output file
      Ret : Integer;         -- Return value from Write operation
   begin
      -- Create a new binary file
      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      -- Check if file creation failed
      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      -- Write Size bytes from array D to file
      Ret := Write (FD, D'Address, Integer (Size));

      -- Verify all bytes were written
      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      -- Close the file
      Close (FD);
   end Write_To_File;

   -------------------------------------------------------------------------------
   -- Load_QOI
   --
   -- Loads and decodes a QOI (Quite OK Image) format file
   --
   -- Parameters:
   --    Filename - Path to the QOI file to load
   --
   -- Returns:
   --    Input_Data record containing the decoded image data and description
   --
   -- Raises:
   --    OS_Exit(1) if file cannot be opened/read or decoding fails
   -------------------------------------------------------------------------------
   function Load_QOI (Filename : String) return Input_Data is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;  -- File descriptor for input file
      Ret : Integer;         -- Return value from read operation

      Result : Input_Data;   -- Holds decoded image data and description
   begin
      -- Open file in binary mode
      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      -- Exit if file cannot be opened
      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         -- Get file size and allocate buffer
         Len     : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : constant Storage_Array_Access :=
           new Storage_Array (1 .. Len);
      begin
         -- Read entire file into buffer
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         -- Verify complete file was read
         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         -- Extract QOI header info into description
         QOI.Get_Desc (In_Data.all, Result.Desc);

         declare
            -- Calculate output buffer size based on image dimensions
            Out_Len     : constant Storage_Count        :=
              Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
            Out_Data    : constant Storage_Array_Access :=
              new Storage_Array (1 .. Out_Len);
            Output_Size : Storage_Count;  -- Actual size of decoded data
         begin
            -- Decode QOI data into raw pixel data
            QOI.Decode
              (Data   => In_Data.all, Desc => Result.Desc,
               Output => Out_Data.all, Output_Size => Output_Size);

            Result.Data := Out_Data;

            -- Verify decoded data matches reference implementation
            if Reference_QOI.Check_Decode
                (In_Data.all, Result.Desc,
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

   ------------------------------------------------------------------------------
   -- Rotate_Image
   --
   -- Rotates an image by a specified angle (in degrees).
   --
   -- Parameters:
   --   Data      - Image data array to be rotated (modified in-place)
   --   Desc      - QOI descriptor containing image metadata
   --   Angle     - Rotation angle in degrees (positive for clockwise rotation)
   --
   -- Effects:
   --   Produces a rotated version of the input image. The original image is
   --   overwritten with the rotated result. Pixels outside the bounds of the
   --   original image are set to black (0).
   ------------------------------------------------------------------------------
   procedure Rotate_Image
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Angle : Float)
   is
      -- Import Pi from Ada.Numerics
      use Ada.Numerics;

      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Integer := Integer (Desc.Channels);

      -- Image dimensions
      Width  : constant Integer := Integer (Desc.Width);
      Height : constant Integer := Integer (Desc.Height);

      -- Angle in radians for trigonometric calculations
      Radians : constant Float := Angle * Pi / 180.0;

      -- Trigonometric values for rotation matrix
      Cos_Theta : constant Float := Cos (Radians);
      Sin_Theta : constant Float := Sin (Radians);

      -- Temporary array to store rotated image data
      Temp : Storage_Array := Data;

      -- Center of the image
      Center_X : constant Float := Float (Width) / 2.0;
      Center_Y : constant Float := Float (Height) / 2.0;

   begin
      -- Iterate over each pixel in the destination image
      for Y in 0 .. Height - 1 loop
         for X in 0 .. Width - 1 loop
            -- Translate pixel position to origin, apply rotation, and translate back
            declare
               Src_X_Float : constant Float :=
                 Cos_Theta * (Float (X) - Center_X) +
                 Sin_Theta * (Float (Y) - Center_Y) + Center_X;
               Src_Y_Float : constant Float :=
                 -Sin_Theta * (Float (X) - Center_X) +
                 Cos_Theta * (Float (Y) - Center_Y) + Center_Y;

               -- Convert source coordinates to integers for indexing
               Src_X : Integer := Integer (Src_X_Float);
               Src_Y : Integer := Integer (Src_Y_Float);

               -- Destination pixel position in 1D array
               Dest_Pos : constant Storage_Count :=
                 Storage_Count ((Y * Width + X) * Pixel_Size + 1);

               Src_Pos : Storage_Count;
            begin
               -- Check if source coordinates are within bounds of the original image
               if Src_X >= 0 and Src_X < Width and Src_Y >= 0 and
                 Src_Y < Height
               then
                  -- Source pixel position in 1D array
                  Src_Pos :=
                    Storage_Count (((Src_Y * Width) + Src_X) * Pixel_Size + 1);

                  -- Copy pixel data from source to destination
                  for C in 0 .. Pixel_Size - 1 loop
                     Data (Dest_Pos + Storage_Count (C)) :=
                       Temp (Src_Pos + Storage_Count (C));
                  end loop;
               else
                  -- Set pixel to black if source is out of bounds
                  for C in 0 .. Pixel_Size - 1 loop
                     Data (Dest_Pos + Storage_Count (C)) := 0;
                  end loop;
               end if;
            end;
         end loop;
      end loop;
   end Rotate_Image;

   ------------------------------------------------------------------------------
   -- Flip_Image
   --
   -- Flips an image horizontally or vertically.
   --
   -- Parameters:
   --   Data       - Image data array to be flipped (modified in-place)
   --   Desc       - QOI descriptor containing image metadata
   --   Direction  - Direction of the flip: "Horizontal" or "Vertical"
   --
   -- Effects:
   --   Modifies the input Data array in-place by rearranging pixel values.
   ------------------------------------------------------------------------------
   procedure Flip_Image
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Direction : String)
   is
      -- Size of each pixel in bytes (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      -- Total number of pixels in one row of the image
      Row_Size : constant Storage_Count := Desc.Width * Pixel_Size;

      -- Temporary buffer to hold a single row during processing
      Temp_Row : Storage_Array (1 .. Row_Size);
   begin
      if Direction = "Horizontal" then
         -- Flip the image horizontally
         for Y in 0 .. Desc.Height - 1 loop
            -- Process each row of the image
            declare
               Row_Start : constant Storage_Count := Y * Row_Size + 1;
            begin
               -- Copy the row into a temporary buffer
               Temp_Row := Data (Row_Start .. Row_Start + Row_Size - 1);

               -- Reverse the order of pixels within the row
               for X in 0 .. Desc.Width - 1 loop
                  for C in 0 .. Pixel_Size - 1 loop
                     Data (Row_Start + X * Pixel_Size + C) :=
                       Temp_Row
                         (Row_Size - X * Pixel_Size - Pixel_Size + C + 1);
                  end loop;
               end loop;
            end;
         end loop;

      elsif Direction = "Vertical" then
         -- Flip the image vertically
         for Y in 0 .. (Desc.Height / 2) - 1 loop
            declare
               Top_Row_Start    : constant Storage_Count := Y * Row_Size + 1;
               Bottom_Row_Start : constant Storage_Count :=
                 (Desc.Height - Y - 1) * Row_Size + 1;
            begin
               -- Swap top and bottom rows
               Temp_Row                                                   :=
                 Data (Top_Row_Start .. Top_Row_Start + Row_Size - 1);
               Data (Top_Row_Start .. Top_Row_Start + Row_Size - 1)       :=
                 Data (Bottom_Row_Start .. Bottom_Row_Start + Row_Size - 1);
               Data (Bottom_Row_Start .. Bottom_Row_Start + Row_Size - 1) :=
                 Temp_Row;
            end;
         end loop;

      else
         -- Invalid direction, raise an error
         raise Program_Error
           with "Invalid flip direction. Use 'Horizontal' or 'Vertical'.";
      end if;
   end Flip_Image;

   --------------------------------------------------------------------------------
   -- Adjust_Brightness
   --
   -- Adjusts the brightness of an image by either adding a specified value to each
   -- color channel of every pixel or flipping the brightness (dimming bright pixels
   -- and brightening dim ones, but leaving very dark pixels unchanged).
   --
   -- Parameters:
   --    Data           - Image data array to be adjusted (modified in-place)
   --    Desc           - QOI descriptor containing image metadata
   --    Brightness     - Integer value to adjust brightness (-255 to 255)
   --    Flip_Brightness - Boolean flag to enable special flip brightness mode
   --
   -- Effects:
   --    Modifies the input Data array in-place by increasing/decreasing brightness
   --    or flipping brightness. Values are clamped between 0 and 255.
   --------------------------------------------------------------------------------
   procedure Adjust_Brightness
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Brightness : Integer;
      Flip_Brightness :        Boolean)
   is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      -- Temporary variable for adjusted channel value
      Adjusted_Value : Integer;

      -- Midpoint for flip brightness (127 for 8-bit images)
      Midpoint : constant Integer := 127;

      -- Threshold below which very dark pixels remain unchanged
      Dark_Threshold : constant Integer := 5;

      type Color_Channel is (Red, Green, Blue);

   begin
      -- Iterate through each pixel in the image data
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         -- Process only color channels (skip alpha channel if present)
         if (I - Data'First) mod Pixel_Size < Pixel_Size - 1 then
            if Flip_Brightness then
               -- Flip brightness: dim bright pixels, brighten dim ones,
               -- but leave very dark pixels unchanged
               if Integer (Data (I)) > Midpoint then
                  -- Dim bright pixels above the midpoint
                  Adjusted_Value :=
                    Integer (Data (I)) - (Integer (Data (I)) - Midpoint) / 2;
               elsif Integer (Data (I)) > Dark_Threshold then
                  -- Brighten dim pixels above the dark threshold but below midpoint
                  Adjusted_Value :=
                    Integer (Data (I)) + (Midpoint - Integer (Data (I))) / 2;
               else
                  -- Leave very dark pixels unchanged
                  Adjusted_Value := Integer (Data (I));
               end if;
            else
               -- Standard brightness adjustment: add/subtract Brightness value
               Adjusted_Value := Integer (Data (I)) + Brightness;
            end if;

            -- Clamp adjusted value between 0 and 255
            if Adjusted_Value < 0 then
               Data (I) := 0;
            elsif Adjusted_Value > 255 then
               Data (I) := 255;
            else
               Data (I) := Storage_Element (Adjusted_Value);
            end if;
         end if;
      end loop;
   end Adjust_Brightness;

   ------------------------------------------------------------------------------
   -- Adjust_Contrast
   --
   -- Adjusts the contrast of an image by modifying pixel intensity values.
   --
   -- Parameters:
   --   Data   - Image data array to be modified (modified in-place)
   --   Desc   - QOI descriptor containing image metadata
   --   Factor - Contrast adjustment factor (e.g., 1.0 = no change,
   --            >1.0 increases contrast, <1.0 decreases contrast)
   --
   -- Effects:
   --   Modifies the input Data array in-place, scaling pixel intensity values
   --   based on the specified contrast factor.
   ------------------------------------------------------------------------------
   procedure Adjust_Contrast
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Factor : Float)
   is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      -- Midpoint value for pixel intensity (128 for 8-bit color depth)
      Midpoint : constant Integer := 128;

      -- Temporary variable for adjusted pixel value
      Adjusted_Value : Integer;

   begin
      -- Iterate through each pixel in the image data
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         -- Process only the color channels (skip alpha if present)
         if (I - Data'First) mod Pixel_Size < 3 then
            -- Adjust pixel intensity based on contrast formula:
            -- New_Value = Midpoint + Factor * (Old_Value - Midpoint)
            Adjusted_Value :=
              Integer (Midpoint) +
              Integer (Factor * Float (Integer (Data (I)) - Midpoint));

            -- Clamp adjusted value to valid range [0, 255]
            if Adjusted_Value < 0 then
               Data (I) := 0;
            elsif Adjusted_Value > 255 then
               Data (I) := 255;
            else
               Data (I) := Storage_Element (Adjusted_Value);
            end if;
         end if;
      end loop;
   end Adjust_Contrast;

   ------------------------------------------------------------------------------
   -- Convert_To_Grayscale
   --
   -- Converts an RGB or RGBA image to grayscale using the luminosity method
   --
   -- Parameters:
   --   Data   - Image data array to be converted (modified in-place)
   --   Desc   - QOI descriptor containing image metadata
   --
   -- The conversion uses the luminosity formula:
   -- Y = 0.299R + 0.587G + 0.114B
   --
   -- For RGBA images, the alpha channel is preserved unchanged
   ------------------------------------------------------------------------------
   procedure Convert_To_Grayscale
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      -- Calculated grayscale value for current pixel
      Gray_Value : Storage_Element;

   begin
      -- Process each pixel (skipping to start of each pixel using mod)
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if (I - Data'First) mod Pixel_Size = 0 then
            -- Convert RGB to grayscale using luminosity method
            -- Add 500 to numerator to ensure proper rounding (integer math)
            Gray_Value :=
              Storage_Element
                ((Integer (Data (I)) * 299 + Integer (Data (I + 1)) * 587 +
                  Integer (Data (I + 2)) * 114 + 500) /
                 1_000);

            -- Apply gray value to RGB channels
            Data (I)     := Gray_Value; -- Red channel
            Data (I + 1) := Gray_Value; -- Green channel
            Data (I + 2) := Gray_Value; -- Blue channel

            -- Keep alpha channel unchanged if present (RGBA)
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Grayscale;

   --------------------------------------------------------------------------------
   -- Convert_To_Black_And_White
   --
   -- Converts an image from grayscale to pure black and white using a threshold.
   --
   -- Parameters:
   --    Data      - Image data array to be converted (modified in-place)
   --    Desc      - QOI image descriptor containing format information
   --    Threshold - Brightness value (0-255) that determines black/white cutoff,
   --                defaults to 128
   --
   -- Effects:
   --    Modifies the input Data array in-place, converting each pixel to either
   --    pure black (0) or pure white (255) based on the threshold value.
   --    Alpha channel values are preserved if present.
   --------------------------------------------------------------------------------
   procedure Convert_To_Black_And_White
     (Data      : in out Storage_Array; Desc : QOI.QOI_Desc;
      Threshold :        Storage_Element := 128)
   is
      -- Size of each pixel in bytes (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      -- Value to set for all color channels (0 for black, 255 for white)
      BW_Value   : Storage_Element;
   begin
      -- Iterate through each pixel in the image data
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         -- Process only the first channel of each pixel
         if I mod Pixel_Size = 1 then
            -- Determine if pixel should be black or white
            if Data (I) > Threshold then
               BW_Value := 255;  -- Convert to white

            else
               BW_Value := 0;    -- Convert to black
            end if;

            -- Apply the black/white value to all color channels
            Data (I)     := BW_Value;  -- Red channel
            Data (I + 1) := BW_Value;  -- Green channel
            Data (I + 2) := BW_Value;  -- Blue channel

            -- If image has alpha channel, preserve its original value
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Black_And_White;

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

   ------------------------------------------------------------------------------
   -- Invert_Colors
   --
   -- Inverts the colors of an image by subtracting each color channel value
   -- from 255. For RGBA images, the alpha channel remains unchanged.
   --
   -- Parameters:
   --   Data - Image data array to be inverted (modified in-place)
   --   Desc - QOI descriptor containing image metadata
   ------------------------------------------------------------------------------
   procedure Invert_Colors (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
   begin
      -- Iterate through each pixel in the image data
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         -- Process only the first three channels (R, G, B)
         if (I - Data'First) mod Pixel_Size = 0 then
            Data (I)     := 255 - Data (I);     -- Red channel
            Data (I + 1) := 255 - Data (I + 1); -- Green channel
            Data (I + 2) := 255 - Data (I + 2); -- Blue channel

            -- Preserve alpha channel if present (RGBA)
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Invert_Colors;

   ------------------------------------------------------------------------------
   -- Sharpen_Image
   --
   -- Sharpens an image by applying a Laplacian filter.
   --
   -- Parameters:
   --   Data - Image data array to be sharpened (modified in-place)
   --   Desc - QOI descriptor containing image metadata
   --
   -- Effects:
   --   Enhances edges in the image by applying a convolution with a Laplacian
   --   kernel. The resulting image is written back to the input Data array.
   ------------------------------------------------------------------------------
   procedure Sharpen_Image (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Integer := Integer (Desc.Channels);

      -- Image dimensions
      Width  : constant Integer := Integer (Desc.Width);
      Height : constant Integer := Integer (Desc.Height);

      -- Temporary array to store the processed image data
      Temp : Storage_Array := Data;

      -- Laplacian kernel for edge detection
      Kernel : constant array (-1 .. 1, -1 .. 1) of Integer :=
        ((0, -1, 0), (-1, 4, -1), (0, -1, 0));

   begin
      -- Iterate over each pixel in the image (excluding borders)
      for Y in 1 .. Height - 2 loop
         for X in 1 .. Width - 2 loop
            -- Process each color channel separately
            for C in 0 .. Pixel_Size - 2 loop
               declare
                  -- Accumulator for convolution result
                  Sum : Integer := 0;

                  -- Destination pixel position in the output array
                  Dest_Pos : constant Storage_Count :=
                    Storage_Count (((Y * Width) + X) * Pixel_Size + C + 1);
               begin
                  -- Apply the Laplacian kernel to the current pixel neighborhood
                  for Ky in -1 .. 1 loop
                     for Kx in -1 .. 1 loop
                        declare
                           -- Source pixel position in the input array
                           Src_X : constant Integer := X + Kx;
                           Src_Y : constant Integer := Y + Ky;

                           Src_Pos : constant Storage_Count :=
                             Storage_Count
                               (((Src_Y * Width) + Src_X) * Pixel_Size + C +
                                1);
                        begin
                           Sum :=
                             Sum + Integer (Data (Src_Pos)) * Kernel (Ky, Kx);
                        end;
                     end loop;
                  end loop;

                  -- Clamp the resulting value to [0, 255]
                  if Sum < 0 then
                     Temp (Dest_Pos) := 0;
                  elsif Sum > 255 then
                     Temp (Dest_Pos) := 255;
                  else
                     Temp (Dest_Pos) := Storage_Element (Sum);
                  end if;
               end;
            end loop;
         end loop;
      end loop;

      -- Copy the processed data back to the original Data array
      Data := Temp;
   end Sharpen_Image;

   ------------------------------------------------------------------------------
   -- Procedure: Blur_Image
   -- Purpose: Applies a box blur filter to an image using a sliding kernel
   --
   -- Parameters:
   --   Data      - Image data array to be blurred (modified in-place)
   --   Width     - Width of the image in pixels
   --   Height    - Height of the image in pixels
   --   Channels  - Number of color channels per pixel (e.g. 3 for RGB)
   --
   -- The procedure uses a box blur kernel of size Kernel_Size x Kernel_Size
   -- to average pixel values with their neighbors. Edge pixels within Half_Kernel
   -- distance from borders are not processed to avoid array bounds issues.
   ------------------------------------------------------------------------------
   procedure Blur_Image
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      -- Temporary copy of input data to read from while writing blurred result
      Temp : Storage_Array := Data;

      -- Size of the blur kernel (must be even)
      Kernel_Size : constant Storage_Count := 4;

      -- Half kernel size used for iteration bounds
      Half_Kernel : constant Storage_Count := Kernel_Size / 2;
   begin
      -- Iterate over each pixel, excluding border pixels
      for Y in Half_Kernel .. Height - Half_Kernel - 1 loop
         for X in Half_Kernel .. Width - Half_Kernel - 1 loop
            -- Process each color channel
            for C in 0 .. Channels - 1 loop
               declare
                  Sum   : Integer := 0;  -- Sum of kernel values
                  Count : Integer := 0;  -- Number of values summed
               begin
                  -- Apply box blur kernel by summing neighboring pixels
                  for KY in -Half_Kernel .. Half_Kernel loop
                     for KX in -Half_Kernel .. Half_Kernel loop
                        declare
                           -- Calculate 1D array position from 2D coordinates
                           Pos : constant Storage_Count :=
                             ((Y + KY) * Width + (X + KX)) * Channels + C + 1;
                        begin
                           Sum   := Sum + Integer (Temp (Pos));
                           Count := Count + 1;
                        end;
                     end loop;
                  end loop;

                  -- Store averaged value in output image
                  Data (((Y * Width) + X) * Channels + C + 1) :=
                    Storage_Element (Sum / Count);
               end;
            end loop;
         end loop;
      end loop;
   end Blur_Image;

   ------------------------------------------------------------------------------
   -- Get_Pixel
   --
   -- Retrieves the pixel data from an image at specified (X, Y) coordinates.
   -- Returns zero if coordinates are out of bounds.
   --
   -- Parameters:
   --   Data     - Image data array
   --   X, Y     - Pixel coordinates (1-based indexing)
   --   Width    - Width of the image in pixels
   --   Height   - Height of the image in pixels
   --   Channels - Number of channels per pixel (e.g., 3 for RGB, 4 for RGBA)
   --
   -- Returns:
   --   Storage_Element - Pixel value at (X, Y) for the first channel, or zero
   --                     if out of bounds.
   ------------------------------------------------------------------------------
   function Get_Pixel
     (Data                    : Storage_Array; X, Y : Storage_Count;
      Width, Height, Channels : Storage_Count) return Storage_Element
   is
      Index : Storage_Count;
   begin
      -- Return zero if coordinates are out of bounds
      if X < 1 or X > Width or Y < 1 or Y > Height then
         return 0;
      end if;

      -- Calculate index of the pixel in the Data array
      Index := ((Y - 1) * Width + (X - 1)) * Channels + 1;

      -- Return pixel data at calculated index
      return Data (Index);
   end Get_Pixel;

   ------------------------------------------------------------------------------
   -- Sobel_Edge_Detection
   --
   -- Applies Sobel edge detection to an image by computing the gradients in the
   -- x and y directions and calculating the gradient magnitude for each pixel.
   -- The result is stored in the Data array with values normalized to 0-255.
   --
   -- Parameters:
   --   Data     - Image data array, modified in place to store edge-detected data
   --   Width    - Width of the image in pixels
   --   Height   - Height of the image in pixels
   --   Channels - Number of color channels per pixel (typically 1 for grayscale)
   ------------------------------------------------------------------------------
   procedure Sobel_Edge_Detection
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      -- Temporary copy of image data for gradient calculations
      Temp : Storage_Array := Data;

      -- Sobel convolution kernels for x and y gradients
      Gx : array (1 .. 3, 1 .. 3) of Integer :=
        ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1));
      Gy : array (1 .. 3, 1 .. 3) of Integer :=
        ((-1, -2, -1), (0, 0, 0), (1, 2, 1));

      -- Track maximum gradient for normalization
      Max_Gradient : Natural := 0;
   begin
      -- Calculate gradients and find the maximum gradient
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Gradient_X         : Integer := 0;
               Gradient_Y         : Integer := 0;
               Gradient_Magnitude : Natural;
            begin
               -- Apply Sobel kernels to compute x and y gradients
               for I in 1 .. 3 loop
                  for J in 1 .. 3 loop
                     declare
                        Pixel_Value : constant Integer :=
                          Integer
                            (Get_Pixel
                               (Temp, X + Storage_Count (J) - 2,
                                Y + Storage_Count (I) - 2, Width, Height,
                                Channels));
                     begin
                        Gradient_X := Gradient_X + (Pixel_Value * Gx (I, J));
                        Gradient_Y := Gradient_Y + (Pixel_Value * Gy (I, J));
                     end;
                  end loop;
               end loop;

               -- Compute gradient magnitude (Euclidean norm)
               declare
                  Gx_Squared : constant Float :=
                    Float (Gradient_X * Gradient_X);
                  Gy_Squared : constant Float :=
                    Float (Gradient_Y * Gradient_Y);
                  Magnitude : constant Float := Sqrt (Gx_Squared + Gy_Squared);
               begin
                  -- Convert magnitude to Natural with bounds checking
                  if Magnitude > Float (Natural'Last) then
                     Gradient_Magnitude := Natural'Last;
                  else
                     Gradient_Magnitude := Natural (Magnitude);
                  end if;
               end;

               -- Update maximum gradient value for later normalization
               if Gradient_Magnitude > Max_Gradient then
                  Max_Gradient := Gradient_Magnitude;
               end if;

               -- Store gradient magnitude in each channel of the pixel
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

      -- Normalize gradients to the range 0-255
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

   -- Accumulator_Cell represents a single cell in the Hough Transform accumulator,
   -- with parameters for the line (Rho and Theta) and its corresponding vote count.
   type Accumulator_Cell is record
      Rho   : Integer;  -- Distance from the origin (rho)
      Theta : Integer;  -- Angle in degrees (theta)
      Votes : Natural;  -- Vote count for this (Rho, Theta) pair
   end record;

   -- Accumulator_Array defines a 2D array for storing votes, indexed by rho and theta.
   type Accumulator_Array is
     array (Positive range <>, Positive range <>) of Natural;

   -- Accumulator_Access is an access type for dynamically allocated Accumulator_Array.
   type Accumulator_Access is access Accumulator_Array;

   ------------------------------------------------------------------------------
   -- Draw_Line
   --
   -- Draws a line between two specified points (X1, Y1) and (X2, Y2) in an image,
   -- using Bresenham's line algorithm. Sets each pixel along the line to a given
   -- color for all channels.
   --
   -- Parameters:
   --   Data     - Image data array (modified in-place)
   --   X1, Y1   - Starting coordinates of the line
   --   X2, Y2   - Ending coordinates of the line
   --   Width    - Width of the image in pixels
   --   Height   - Height of the image in pixels
   --   Channels - Number of channels per pixel (e.g., 3 for RGB, 4 for RGBA)
   --   Color    - Line color for all channels (default is 255)
   ------------------------------------------------------------------------------
   procedure Draw_Line
     (Data                    : in out Storage_Array; X1, Y1, X2, Y2 : Integer;
      Width, Height, Channels : Storage_Count; Color : Storage_Element := 255)
   is
      -- Calculate differences and steps for line drawing
      DX     : constant Integer := abs (X2 - X1);
      DY     : constant Integer := abs (Y2 - Y1);
      Step_X : constant Integer := (if X1 < X2 then 1 else -1);
      Step_Y : constant Integer := (if Y1 < Y2 then 1 else -1);

      -- Error term used for line steepness
      Error : Integer := (if DX > DY then DX else -DY) / 2;

      -- Current position
      X : Integer := X1;
      Y : Integer := Y1;
   begin
      -- Draw line by iterating from start point to end point
      loop
         -- Draw pixel only if within bounds
         if X > 0 and X <= Integer (Width) and Y > 0 and Y <= Integer (Height)
         then
            -- Set the pixel color for each channel
            for C in 0 .. Integer (Channels) - 1 loop
               declare
                  -- Calculate index for each channel in the Data array
                  Index : constant Storage_Offset :=
                    Storage_Offset
                      (((Y - 1) * Integer (Width) + (X - 1)) *
                       Integer (Channels) +
                       C + 1);
               begin
                  Data (Index) := Color;
               end;
            end loop;
         end if;

         -- Exit loop once endpoint is reached
         exit when X = X2 and Y = Y2;

         -- Update error and position
         declare
            Error2 : constant Integer := Error;
         begin
            if Error2 > -DX then
               Error := Error - DY;
               X     := X + Step_X;
            end if;
            if Error2 < DY then
               Error := Error + DX;
               Y     := Y + Step_Y;
            end if;
         end;
      end loop;
   end Draw_Line;

   ------------------------------------------------------------------------------
   -- Hough_Transform
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
   procedure Hough_Transform
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count;
      Theta_Resolution :    Positive := 180; Rho_Resolution : Positive := 180)
   is
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
            if Get_Pixel (Data, X, Y, Width, Height, Channels) > 0 then
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
                        Line_Count         := Line_Count + 1;
                        Lines (Line_Count) :=
                          (Rho   => (Float (R) / Rho_Scale) - Max_Rho,
                           Theta => Float (T - 1) * Deg_To_Rad,
                           Votes => Acc (R, T));
                     end if;
                  end;
               end if;
            end loop;
         end loop;

         -- Draw detected lines on the image
         for I in 1 .. Line_Count loop
            declare
               Theta          : constant Float := Lines (I).Theta;
               Rho            : constant Float := Lines (I).Rho;
               X1, Y1, X2, Y2 : Integer;
            begin
               -- Calculate endpoints of each line based on rho and theta
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

               -- Draw line on the image
               Draw_Line (Data, X1, Y1, X2, Y2, Width, Height, Channels);
            end;
         end loop;
      end;

      -- Free accumulator memory to prevent leaks
      declare
         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Accumulator_Array, Name => Accumulator_Access);
         Temp : Accumulator_Access := Acc;
      begin
         Free (Temp);
      end;
   end Hough_Transform;

   ------------------------------------------------------------------------------
   -- Gaussian_Blur
   --
   -- Applies a Gaussian blur to an image using a kernel based on the specified
   -- sigma value. The image data is modified in place.
   --
   -- Parameters:
   --   Data     - Image data array to be blurred (modified in-place)
   --   Width    - Width of the image in pixels
   --   Height   - Height of the image in pixels
   --   Channels - Number of channels per pixel (e.g., 3 for RGB, 4 for RGBA)
   --   Sigma    - Standard deviation for the Gaussian kernel, controls blur radius
   ------------------------------------------------------------------------------
   procedure Gaussian_Blur
     (Data  : in out Storage_Array; Width, Height, Channels : Storage_Count;
      Sigma :        Float := 1.4)
   is

      -- Calculate kernel size based on sigma (usually 6 * sigma)
      Kernel_Size : constant Positive :=
        Positive (Float'Ceiling (6.0 * Sigma));
      Half_Size   : constant Integer  := Kernel_Size / 2;

      -- Create a temporary copy of the image data for processing
      Temp : Storage_Array := Data;
   begin
      -- Declare Gaussian kernel and normalization sum
      declare
         type Kernel_Type is
           array (Integer range <>, Integer range <>) of Float;
         Kernel :
           Kernel_Type (-Half_Size .. Half_Size, -Half_Size .. Half_Size);
         Sum    : Float := 0.0;
      begin
         -- Generate Gaussian kernel values
         for Y in Kernel'Range (1) loop
            for X in Kernel'Range (2) loop
               Kernel (Y, X) :=
                 (1.0 / (2.0 * Ada.Numerics.Pi * Sigma * Sigma)) *
                 Float
                   (Exp (-(Float (X * X + Y * Y) / (2.0 * Sigma * Sigma))));
               Sum           := Sum + Kernel (Y, X);
            end loop;
         end loop;

         -- Normalize kernel values so that their sum equals 1
         for Y in Kernel'Range (1) loop
            for X in Kernel'Range (2) loop
               Kernel (Y, X) := Kernel (Y, X) / Sum;
            end loop;
         end loop;

         -- Apply convolution for each pixel within the image bounds
         for Y in Half_Size + 1 .. Integer (Height) - Half_Size loop
            for X in Half_Size + 1 .. Integer (Width) - Half_Size loop
               for C in 0 .. Integer (Channels) - 1 loop
                  declare
                     Sum : Float := 0.0;
                  begin
                     -- Convolve kernel with pixel neighborhood
                     for KY in Kernel'Range (1) loop
                        for KX in Kernel'Range (2) loop
                           declare
                              Idx : constant Storage_Count :=
                                ((Storage_Count (Y + KY - 1) * Width +
                                  Storage_Count (X + KX - 1)) *
                                 Channels +
                                 Storage_Count (C) + 1);
                           begin
                              -- Accumulate weighted sum for the current channel
                              Sum :=
                                Sum + Float (Temp (Idx)) * Kernel (KY, KX);
                           end;
                        end loop;
                     end loop;
                     -- Update the pixel data in the original array
                     Data
                       ((
                         (Storage_Count (Y - 1) * Width +
                          Storage_Count (X - 1)) *
                         Channels +
                         Storage_Count (C) + 1)) :=
                       Storage_Element (Float'Rounding (Sum));
                  end;
               end loop;
            end loop;
         end loop;
      end;
   end Gaussian_Blur;

   ------------------------------------------------------------------------------
   -- Canny_Edge_Detection
   --
   -- Applies Canny edge detection to an image. This includes Gaussian blurring,
   -- gradient computation, non-maximum suppression, double thresholding, and
   -- edge tracking by hysteresis.
   --
   -- Parameters:
   --   Data          - Image data array to be processed (modified in-place)
   --   Width         - Width of the image in pixels
   --   Height        - Height of the image in pixels
   --   Channels      - Number of color channels per pixel (3 for RGB, 4 for RGBA)
   --   Low_Threshold - Lower bound for edge detection (default is 0.1)
   --   High_Threshold - Upper bound for edge detection (default is 0.3)
   ------------------------------------------------------------------------------
   procedure Canny_Edge_Detection
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count;
      Low_Threshold :        Float := 0.1; High_Threshold : Float := 0.3)
   is

      -- Temporary storage for intermediate image states
      Temp               : Storage_Array := Data;
      Gradient_Magnitude : Storage_Array (Data'Range);
      Gradient_Direction : array (1 .. Width * Height) of Float;
   begin
      -- Step 1: Apply Gaussian blur to reduce noise in the image
      Gaussian_Blur (Data, Width, Height, Channels);

      -- Step 2: Compute gradients using the Sobel operator
      Sobel_Edge_Detection (Data, Width, Height, Channels);

      -- Step 3: Non-maximum suppression to thin edges
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Idx       : constant Storage_Count   :=
                 ((Y - 1) * Width + (X - 1)) * Channels + 1;
               -- Retrieve gradient angle for edge direction quantization
               Angle     : constant Float           :=
                 Gradient_Direction
                   (Storage_Count'Pos
                      ((Storage_Count (Y) - 1) * Width +
                       (Storage_Count (X) - 1)) +
                    1);
               Magnitude : constant Storage_Element := Data (Idx);
            begin
               -- Quantize angle to nearest 0, 45, 90, or 135 degrees
               if
                 ((Angle >= -22.5 and Angle <= 22.5) or (Angle <= -157.5) or
                  (Angle >= 157.5))
               then
                  -- Horizontal edge: suppress if not local max
                  if Magnitude <= Data (Idx - Channels) or
                    Magnitude <= Data (Idx + Channels)
                  then
                     Gradient_Magnitude (Idx) := 0;
                  else
                     Gradient_Magnitude (Idx) := Magnitude;
                  end if;
               elsif
                 ((Angle >= 22.5 and Angle <= 67.5) or
                  (Angle <= -112.5 and Angle >= -157.5))
               then
                  -- 45-degree edge: suppress if not local max
                  if Magnitude <= Data (Idx - Channels - Width * Channels) or
                    Magnitude <= Data (Idx + Channels + Width * Channels)
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

      -- Step 4: Double thresholding to classify strong and weak edges
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

      -- Step 5: Edge tracking by hysteresis to finalize edge pixels
      for Y in 2 .. Height - 1 loop
         for X in 2 .. Width - 1 loop
            declare
               Idx : constant Storage_Count :=
                 ((Y - 1) * Width + (X - 1)) * Channels + 1;
            begin
               if Data (Idx) = 128 then
                  -- For weak edges, check 8-connected neighbors for strong edges
                  declare
                     Has_Strong_Neighbor : Boolean := False;
                     Neighbor_Idx        : Storage_Count;
                  begin
                     for DY in -1 .. 1 loop
                        for DX in -1 .. 1 loop
                           -- Calculate neighbor index, ensuring bounds
                           if (Storage_Count (Integer (X) + DX)) >= 1 and
                             (Storage_Count (Integer (X) + DX)) <= Width and
                             (Storage_Count (Integer (Y) + DY)) >= 1 and
                             (Storage_Count (Integer (Y) + DY)) <= Height
                           then

                              Neighbor_Idx :=
                                ((Storage_Count (Integer (Y) + DY) - 1) *
                                 Width +
                                 (Storage_Count (Integer (X) + DX) - 1)) *
                                Channels +
                                1;

                              if Data (Neighbor_Idx) = 255 then
                                 Has_Strong_Neighbor := True;
                                 exit;
                              end if;
                           end if;
                        end loop;
                        exit when Has_Strong_Neighbor;
                     end loop;

                     if Has_Strong_Neighbor then
                        Data (Idx) := 255; -- Confirm weak edge as strong edge

                     else
                        Data (Idx) := 0;   -- Suppress weak edge if isolated
                     end if;
                  end;
               end if;
            end;
         end loop;
      end loop;
   end Canny_Edge_Detection;

   -- Add these type declarations after the existing ones
   type Circle_Parameters is record
      X, Y  : Storage_Count;  -- Center coordinates
      R     : Storage_Count;  -- Radius
      Votes : Natural;       -- Number of votes
   end record;

   type Circle_Array is array (Positive range <>) of Circle_Parameters;
   type Circle_Array_Access is access Circle_Array;

   procedure Hough_Circle_Transform
     (Data       : in out Storage_Array; Width : Storage_Count;
      Height     :        Storage_Count; Channels : Storage_Count;
      Min_Radius :        Storage_Count; Max_Radius : Storage_Count;
      Threshold  :        Natural; Max_Circles : Positive := 10)
   is

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
                        Data (Base_Index + 1) := 0;    -- Red channel
                        Data (Base_Index + 2) := 100;    -- Green channel
                        Data (Base_Index + 3) := 255;  -- Blue channel
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
         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Accumulator_Array, Name => Accumulator_Access);
         procedure Free is new Ada.Unchecked_Deallocation
           (Object => Circle_Array, Name => Circle_Array_Access);
         --Temp_Acc     : Accumulator_Access := Acc;
         Temp_Circles : Circle_Array_Access := Circles;
      begin
         --Free (Temp_Acc);
         Free (Temp_Circles);
      end;
   end Hough_Circle_Transform;

   procedure Region_Of_Interest
     (Data   : in out Storage_Array; Desc : in out QOI.QOI_Desc;
      X1, Y1 : in     Storage_Count;
                                -- Top-left corner of ROI
                                X2,
      Y2     : in     Storage_Count)  -- Bottom-right corner of ROI
   is
      Width  : constant Storage_Count := X2 - X1 + 1;
      Height : constant Storage_Count := Y2 - Y1 + 1;
   begin
      -- Check if ROI is within image bounds
      if X1 < 1 or X1 > Desc.Width or X2 < 1 or X2 > Desc.Width or Y1 < 1 or
        Y1 > Desc.Height or Y2 < 1 or Y2 > Desc.Height
      then
         raise Constraint_Error with "ROI is out of image bounds";
      end if;

      -- Create a temporary array to store the ROI
      declare
         Temp  : Storage_Array (1 .. Width * Height * Desc.Channels);
         Index : Storage_Count := 1;
      begin
         -- Copy the ROI from the original image to the temporary array
         for Y in Y1 .. Y2 loop
            for X in X1 .. X2 loop
               declare
                  Base_Index : constant Storage_Count :=
                    ((Storage_Count (Y) - 1) * Desc.Width +
                     (Storage_Count (X) - 1)) *
                    Desc.Channels;
               begin
                  for C in 1 .. Desc.Channels loop
                     Temp (Index) := Data (Base_Index + C);
                     Index        := Index + 1;
                  end loop;
               end;
            end loop;
         end loop;

         -- Replace the original image with the ROI
         for Y in 1 .. Desc.Height loop
            for X in 1 .. Desc.Width loop
               declare
                  Base_Index : constant Storage_Count :=
                    ((Storage_Count (Y) - 1) * Desc.Width +
                     (Storage_Count (X) - 1)) *
                    Desc.Channels;
               begin
                  for C in 1 .. Desc.Channels loop
                     Data (Base_Index + C) := 0;  -- Clear the original image
                  end loop;
               end;
            end loop;
         end loop;

         -- Copy the ROI back to the original image
         Index := 1;
         for Y in 1 .. Height loop
            for X in 1 .. Width loop
               declare
                  Base_Index : constant Storage_Count :=
                    ((Storage_Count (Y) - 1) * Width +
                     (Storage_Count (X) - 1)) *
                    Desc.Channels;
               begin
                  for C in 1 .. Desc.Channels loop
                     Data (Base_Index + C) := Temp (Index);
                     Index                 := Index + 1;
                  end loop;
               end;
            end loop;
         end loop;

         -- Update the image description
         Desc.Width  := Width;
         Desc.Height := Height;
      end;
   end Region_Of_Interest;

   Input : Input_Data;
begin
   -- Initialize the simulated camera with the folder path containing QOI frames
   Simulated_Camera.Initialize ("frames_folder");

   loop
      -- Get the next frame from the simulated camera
      declare
         Frame_Name : constant String := Simulated_Camera.Get_Frame;
      begin
         Put_Line ("Processing frame: " & Frame_Name);

         -- Load the current frame using Load_QOI
         Input := Load_QOI ("test_images\control.qoi");

         -- Apply region of interest to focus on the lower half of the image
         -- Region_Of_Interest (Input.Data.all, Input.Desc, 201, 101, 500, 400);

         -- Apply Guassian Blur
         --  Gaussian_Blur
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels, Sigma => 5.0 / 6.0);

         -- Rotate the image (optional)
         -- Rotate_Image (Data => Input.Data.all, Desc => Input.Desc, Angle => -10.0);

         -- Flip the image horizontally (optional)
         --  Flip_Image
         --    (Data      => Input.Data.all, Desc => Input.Desc,
         --     Direction => "Horizontal");

         -- Sharpen the image (optional)
         --Sharpen_Image (Data => Input.Data.all, Desc => Input.Desc);

         -- Apply Sobel edge detection
         Sobel_Edge_Detection
           (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
            Input.Desc.Channels);

         -- Increase brightness by 50 (optional)
         --  Adjust_Brightness
         --    (Input.Data.all, Input.Desc, 50, Flip_Brightness => False);

         -- Invert colors of the loaded image (optional)
         -- Invert_Colors (Input.Data.all, Input.Desc);

         -- Increase contrast by a factor of 1.2 (optional)
         -- Adjust_Contrast (Input.Data.all, Input.Desc, Factor => 1.5);

         -- Convert to grayscale first
         -- Convert_To_Grayscale (Input.Data.all, Input.Desc);

         -- Blur the image
         --  Blur_Image
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         -- Apply Canny edge detection
         --  Canny_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels, 30.0 / 255.0, 150.0 / 255.0);

         -- Convert to black and white (optional)
         -- Convert_To_Black_And_White (Input.Data.all, Input.Desc, 128);

         --  -- Apply morphological operations (optional)
         --  Morphological_Operation
         --    (Input.Data.all, Input.Desc, "Closing", "Square", 5);

         --  Sobel_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         --  Canny_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels, Low_Threshold => 0.28,
         --     High_Threshold                     => 0.34);

         --  -- Apply Hough Transform to detect lines
         --  Hough_Transform
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         -- After edge detection and before encoding
         -- Hough_Circle_Transform
         --    (Data        => Input.Data.all,
         --     Width       => Input.Desc.Width,
         --     Height      => Input.Desc.Height,
         --     Channels    => Input.Desc.Channels,
         --     Min_Radius  => 10,
         --     Max_Radius  => 20,
         --     Threshold   => 20,
         --     Max_Circles => 600);

         declare
            Output : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
            Output_Size : Storage_Count;
         begin
            QOI.Encode (Input.Data.all, Input.Desc, Output, Output_Size);
            Write_To_File ("output.qoi", Output, Output_Size);

            Put_Line ("Frame processed and saved: " & Frame_Name);

            --delay 0.1;  -- Optional: Simulate a delay between frames if needed.

            exit when Frame_Name = "frames_folder/frame_0001.qoi";
            -- Replace "frame_end.qoi" with a condition to stop processing.

         end;
      end;
   end loop;
end Load_Qoi;
--     Input := Load_QOI ("lanes_images\lane7.qoi");

--     -- Apply region of interest to focus on the lower half of the image
--     Region_Of_Interest(Input.Data.all, Input.Desc, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1, 3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

--     -- Rotate the image (optional)
--     -- Rotate_Image (Data => Input.Data.all, Desc => Input.Desc, Angle => -10.0);

--     -- Flip the image horizontally (optional)
--     -- Flip_Image (Data => Input.Data.all, Desc => Input.Desc, Direction => "Horizontal");

--     -- Sharpen the image (optional)
--     --Sharpen_Image (Data => Input.Data.all, Desc => Input.Desc);

--     -- Apply Sobel edge detection
--     Sobel_Edge_Detection
--       (Input.Data.all,
--        Input.Desc.Width,
--        Input.Desc.Height,
--        Input.Desc.Channels);

--     -- Increase brightness by 50 (optional)
--     Adjust_Brightness (Input.Data.all, Input.Desc, 50, Flip_Brightness => True);

--     -- Invert colors of the loaded image (optional)
--     --Invert_Colors (Input.Data.all, Input.Desc);

--     -- Increase contrast by a factor of 1.2 (optional)
--     --Adjust_Contrast (Input.Data.all, Input.Desc, Factor => 1.0);

--     -- Convert to black and white (optional)
--     -- Convert_To_Black_And_White (Input.Data.all, Input.Desc);

--     -- Apply morphological operations (optional)
--     --Morphological_Operation (Input.Data.all, Input.Desc, "Erosion", "Square", 3);

--     -- Convert to grayscale first
--     Convert_To_Grayscale (Input.Data.all, Input.Desc);

--     -- Blur the image
--     --  Blur_Image
--     --    (Input.Data.all,
--     --     Input.Desc.Width,
--     --     Input.Desc.Height,
--     --     Input.Desc.Channels);

--     -- Apply Canny edge detection
--     Canny_Edge_Detection
--       (Input.Data.all,
--        Input.Desc.Width,
--        Input.Desc.Height,
--        Input.Desc.Channels,
--        Low_Threshold  => 0.1,
--        High_Threshold => 0.15);

--     -- Apply Hough Transform to detect lines
--     Hough_Transform
--       (Input.Data.all,
--        Input.Desc.Width,
--        Input.Desc.Height,
--        Input.Desc.Channels);

--     -- After edge detection and before encoding
--     --  Hough_Circle_Transform
--     --    (Data        => Input.Data.all,
--     --     Width       => Input.Desc.Width,
--     --     Height      => Input.Desc.Height,
--     --     Channels    => Input.Desc.Channels,
--     --     Min_Radius  => 10  -- Minimum circle radius to detect
--     --     ,
--     --     Max_Radius  => 20  -- Maximum circle radius to detect
--     --     ,
--     --     Threshold   => 20 -- Minimum votes needed to detect a circle
--     --     ,
--     --     Max_Circles => 600   -- Maximum number of circles to detect
--     --    );

--     declare
--        Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
--        Output_Size : Storage_Count;
--     begin
--        QOI.Encode (Input.Data.all, Input.Desc, Output, Output_Size);
--        Write_To_File ("output.qoi", Output, Output_Size);
--     end;
--  end Load_Qoi;
