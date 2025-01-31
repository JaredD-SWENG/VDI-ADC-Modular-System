with CV_Ada.Basic_Transformations;
with Ada.Text_IO;                       use Ada.Text_IO;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body CV_Ada.Basic_Transformations is
   ------------------------------------------------------------------------------
   -- Region_Of_Interest
   --
   -- Extracts a specified rectangular region from an image and modifies the
   -- image data to contain only that region.
   --
   -- Parameters:
   --   Data   - Image data array to be modified (in-place)
   --   Desc   - QOI descriptor containing image metadata
   --   X1, Y1 - Coordinates of the top-left corner of the region
   --   X2, Y2 - Coordinates of the bottom-right corner of the region
   --
   -- Effects:
   --   The image is cropped to the specified region, and pixels outside
   --   this region are removed. The image metadata is updated accordingly.
   ------------------------------------------------------------------------------
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
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Direction : Directions)
   is
      -- Size of each pixel in bytes (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      -- Total number of pixels in one row of the image
      Row_Size : constant Storage_Count := Desc.Width * Pixel_Size;

      -- Temporary buffer to hold a single row during processing
      Temp_Row : Storage_Array (1 .. Row_Size);
   begin
      if Direction = CV_Ada.Basic_Transformations.Horizontal then
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

      elsif Direction = CV_Ada.Basic_Transformations.Vertical then
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

end CV_Ada.Basic_Transformations;
