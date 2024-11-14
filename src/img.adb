package body Img is 
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
     (Data                    : Storage_Array;
      X, Y                    : Natural;
      Width, Height, Channels : Natural;
      Offset                  : Natural := 0) return Storage_Element
   is
      Index : Natural;
   begin
      -- Return zero if coordinates are out of bounds
      if X < 1 or X > Width or Y < 1 or Y > Height then
         return 0;
      end if;

      -- Calculate index of the pixel in the Data array
      Index := ((Y - 1) * Width + (X - 1)) * Channels + 1 + Offset;

      -- Return pixel data at calculated index
      return Data (Storage_Count (Index));
   end Get_Pixel;
end Img;