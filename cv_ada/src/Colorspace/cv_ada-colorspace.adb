with Ada.Text_IO;                       use Ada.Text_IO;
with System.Storage_Elements;           use System.Storage_Elements;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

package body CV_Ada.Colorspace is
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
   procedure Convert_To_Grayscale (Input : in out Input_Data) is
      -- Number of channels per pixel (3 for RGB, 4 for RGBA)
      --  Data       : Storage_Array          := Input.Data.all;
      --  Desc       : QOI.QOI_Desc           := Input.Desc;
      --  Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

      Data       : CV_Ada.Storage_Array_Access := new Storage_Array(1 .. Input.Data.all'Length);
      Desc       : QOI.QOI_Desc                := Input.Desc;
      Pixel_Size : constant Storage_Count      := Storage_Count (Desc.Channels);

      -- Calculated grayscale value for current pixel
      Gray_Value : Storage_Element;
   begin
      Data.all := Input.Data.all;
      --Process each pixel (skipping to start of each pixel using mod)
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
      Input.Data.all := Data.all; -- Should we unreference the data?
   end Convert_To_Grayscale;

   --  procedure Convert_To_Grayscale
   --    (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   --  is
   --     -- Number of channels per pixel (3 for RGB, 4 for RGBA)
   --     Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);

   --     -- Calculated grayscale value for current pixel
   --     Gray_Value : Storage_Element := 0;
   --     number_pixels : Storage_Offset := (Desc.Width * Desc.Height)/Pixel_Size;

   --     function Gray_Pixel_Value (Data : Storage_Array; I : Storage_Offset) return Storage_Element is
   --     begin
   --        return Storage_Element
   --               ((Integer (Data (I)) * 299 + Integer (Data (I + 1)) * 587 +
   --                 Integer (Data (I + 2)) * 114 + 500) / 1_000);
   --     end Gray_Pixel_Value;

   --     idx : Storage_Offset := 0;
   --  begin
   --     -- Process each pixel (skipping to start of each pixel using mod)
   --     for I in 1 .. (number_pixels) loop
   --        idx := I;

   --        Gray_Value := Gray_Pixel_Value (Data, I * Pixel_Size);

   --        -- Apply gray value to RGB channels
   --        Data (I * Pixel_Size)     := Gray_Value; -- Red channel
   --        Data (I * Pixel_Size + 1) := Gray_Value; -- Green channel
   --        Data (I * Pixel_Size + 2) := Gray_Value; -- Blue channel

   --     end loop;

   --  exception
   --     when others =>
   --        Put_Line (idx'Image);
   --        Put_Line (number_pixels'Image);
   --  end Convert_To_Grayscale;

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
     (Input : in out Input_Data;
      Threshold :        Storage_Element := 128)
   is

   Data       : CV_Ada.Storage_Array_Access := new Storage_Array(1 .. Input.Data.all'Length);
   Desc       : QOI.QOI_Desc                := Input.Desc;
      -- Size of each pixel in bytes (3 for RGB, 4 for RGBA)
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      -- Value to set for all color channels (0 for black, 255 for white)
      BW_Value   : Storage_Element;
   begin
      Data.all := Input.Data.all;
      -- Iterate through each pixel in the image data
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         -- Process only the first channel of each pixel
         if I mod Pixel_Size = 1 then
            -- Determine if pixel should be black or white
            if Data (I) >= Threshold then
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
      Input.Data.all := Data.all;
   end Convert_To_Black_And_White;
end CV_Ada.Colorspace;
