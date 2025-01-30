with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body OpenCV_Ada.Blur is
   ------------------------------------------------------------------------------
   -- Procedure: Box_Blur
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
   procedure Box_Blur
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      -- Temporary copy of input data to read from while writing blurred result
      Temp : Storage_Array := Data;

      -- Size of the blur kernel (must be even)
      Kernel_Size : constant Storage_Count := 6;

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
   end Box_Blur;

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
end OpenCV_Ada.Blur;
