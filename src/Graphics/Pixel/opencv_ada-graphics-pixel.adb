package body OpenCV_Ada.Graphics.Pixel is
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

   function "+" (Left : pixel; Right : pixel) return pixel is
      P : pixel;
   begin
      P.R := Left.R + Right.R;
      P.G := Left.G + Right.G;
      P.B := Left.B + Right.B;
      return P;
   end;

   function "*" (Left : pixel; Right : Float) return pixel is
      P : pixel;
   begin
      P.R := Left.R * Right;
      P.G := Left.G * Right;
      P.B := Left.B * Right;
      return P;
   end;

   function "/" (Left : pixel; Right : Float) return pixel is
      P : pixel;
   begin
      P.R := Left.R / Right;
      P.G := Left.G / Right;
      P.B := Left.B / Right;
      return P;
   end;
end OpenCV_Ada.Graphics.Pixel;
