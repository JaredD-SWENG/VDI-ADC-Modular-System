with CV_Ada.Graphics;             use CV_Ada.Graphics;
with CV_Ada.Blur;                       use CV_Ada.Blur;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body CV_Ada.Edge_Detection is

   procedure Free is new Ada.Unchecked_Deallocation(Storage_Array, Storage_Array_Access);
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
   (Input: in out Input_Data)
is
   Width    : constant Storage_Count := Input.Desc.Width;
   Height   : constant Storage_Count := Input.Desc.Height;
   Channels : constant Storage_Count := Input.Desc.Channels;
   Data     : Storage_Array_Access := Input.Data;
   -- Temporary copy of image data for gradient calculations
   Temp     : Storage_Array_Access := new Storage_Array(Data.all'Range);

   -- Sobel convolution kernels for x and y gradients
   Gx : array (1 .. 3, 1 .. 3) of Integer :=
     ((-1, 0, 1), (-2, 0, 2), (-1, 0, 1));
   Gy : array (1 .. 3, 1 .. 3) of Integer :=
     ((-1, -2, -1), (0, 0, 0), (1, 2, 1));

   -- Track maximum gradient for normalization
   Max_Gradient : Natural := 0;
   
   -- Helper function to get a pixel value from the temporary array
   function Get_Pixel
     (Image    : Storage_Array_Access;
      X, Y     : Storage_Count;
      Width    : Storage_Count;
      Height   : Storage_Count;
      Channels : Storage_Count) return Storage_Element
   is
      -- Calculate position in 1D array from 2D coordinates
      -- Using only the first channel for grayscale edge detection
      Pos : constant Storage_Count := ((Y - 1) * Width + (X - 1)) * Channels + 1;
   begin
      -- Bounds checking
      if X < 1 or X > Width or Y < 1 or Y > Height then
         return 0;
      end if;
      
      return Image(Pos);
   end Get_Pixel;
   
begin
   -- Make a copy of the original data
   Temp.all := Data.all;

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
   
   -- Free the temporary array to avoid memory leaks
   Free(Temp);
end Sobel_Edge_Detection;


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
   (Input : in out Input_Data;
    Low_Threshold : Float := 0.1; High_Threshold : Float := 0.3)
is
   -- Access the input data and descriptor
   Data : Storage_Array_Access := Input.Data;
   Desc : QOI.QOI_Desc := Input.Desc;
   
   -- Image dimensions and properties
   Width    : constant Storage_Count := Desc.Width;
   Height   : constant Storage_Count := Desc.Height;
   Channels : constant Storage_Count := Desc.Channels;

   -- Temporary storage for intermediate image states
   Temp : Storage_Array_Access := new Storage_Array(Data.all'Range);
   Gradient_Magnitude : Storage_Array_Access := new Storage_Array(Data.all'Range);
   Gradient_Direction : array (1 .. Width * Height) of Float;
begin
   -- Copy original data to temporary array
   Temp.all := Data.all;

   -- Step 1: Apply Gaussian blur to reduce noise in the image
   Gaussian_Blur (Input);

   -- Step 2: Compute gradients using the Sobel operator
   Sobel_Edge_Detection (Input);

   -- Step 3: Non-maximum suppression to thin edges
   for Y in 2 .. Height - 1 loop
      for X in 2 .. Width - 1 loop
         declare
            Idx : constant Storage_Count := 
              ((Y - 1) * Width + (X - 1)) * Channels + 1;
            -- Retrieve gradient angle for edge direction quantization
            Angle : constant Float := 
              Gradient_Direction(Storage_Count'Pos((Y - 1) * Width + (X - 1)) + 1);
            Magnitude : constant Storage_Element := Data(Idx);
         begin
            -- Quantize angle to nearest 0, 45, 90, or 135 degrees
            if ((Angle >= -22.5 and Angle <= 22.5) or 
                (Angle <= -157.5) or (Angle >= 157.5)) then
               -- Horizontal edge: suppress if not local max
               if Magnitude <= Data(Idx - Channels) or
                  Magnitude <= Data(Idx + Channels) then
                  Gradient_Magnitude(Idx) := 0;
               else
                  Gradient_Magnitude(Idx) := Magnitude;
               end if;
            elsif ((Angle >= 22.5 and Angle <= 67.5) or
                   (Angle <= -112.5 and Angle >= -157.5)) then
               -- 45-degree edge: suppress if not local max
               if Magnitude <= Data(Idx - Channels - Width * Channels) or
                  Magnitude <= Data(Idx + Channels + Width * Channels) then
                  Gradient_Magnitude(Idx) := 0;
               else
                  Gradient_Magnitude(Idx) := Magnitude;
               end if;
               -- Add similar checks for 90 and 135 degrees
            end if;
         end;
      end loop;
   end loop;

   -- Step 4: Double thresholding to classify strong and weak edges
   declare
      High : constant Storage_Element := Storage_Element(255.0 * High_Threshold);
      Low  : constant Storage_Element := Storage_Element(255.0 * Low_Threshold);
   begin
      for I in Data.all'Range loop
         if Gradient_Magnitude(I) >= High then
            Data(I) := 255;  -- Strong edge
         elsif Gradient_Magnitude(I) >= Low then
            Data(I) := 128;  -- Weak edge
         else
            Data(I) := 0;    -- Non-edge
         end if;
      end loop;
   end;

   -- Step 5: Edge tracking by hysteresis to finalize edge pixels
   for Y in 2 .. Height - 1 loop
      for X in 2 .. Width - 1 loop
         declare
            Idx : constant Storage_Count := ((Y - 1) * Width + (X - 1)) * Channels + 1;
         begin
            if Data(Idx) = 128 then
               -- For weak edges, check 8-connected neighbors for strong edges
               declare
                  Has_Strong_Neighbor : Boolean := False;
                  Neighbor_Idx : Storage_Count;
               begin
                  for DY in -1 .. 1 loop
                     for DX in -1 .. 1 loop
                        -- Calculate neighbor index, ensuring bounds
                        if (Storage_Count(Integer(X) + DX)) >= 1 and
                           (Storage_Count(Integer(X) + DX)) <= Width and
                           (Storage_Count(Integer(Y) + DY)) >= 1 and
                           (Storage_Count(Integer(Y) + DY)) <= Height then

                           Neighbor_Idx := 
                             ((Storage_Count(Integer(Y) + DY) - 1) * Width +
                              (Storage_Count(Integer(X) + DX) - 1)) * Channels + 1;

                           if Data(Neighbor_Idx) = 255 then
                              Has_Strong_Neighbor := True;
                              exit;
                           end if;
                        end if;
                     end loop;
                     exit when Has_Strong_Neighbor;
                  end loop;

                  if Has_Strong_Neighbor then
                     Data(Idx) := 255; -- Confirm weak edge as strong edge
                  else
                     Data(Idx) := 0;   -- Suppress weak edge if isolated
                  end if;
               end;
            end if;
         end;
      end loop;
   end loop;
   
   -- Free the temporary arrays to avoid memory leaks
   Free(Temp);
   Free(Gradient_Magnitude);
end Canny_Edge_Detection;

end CV_Ada.Edge_Detection;
