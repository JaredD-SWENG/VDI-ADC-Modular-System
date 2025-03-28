package body CV_Ada.Graphics is
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
end CV_Ada.Graphics;
