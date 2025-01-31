package body CV_Ada.Graphics.Pixel is
   function "+" (Left : pixel; Right : pixel) return pixel is
      P : pixel;
   begin
      P.R := Left.R + Right.R;
      P.G := Left.G + Right.G;
      P.B := Left.B + Right.B;
      return P;
   end;

   function "-" (Left : pixel; Right : pixel) return pixel is
      P : pixel;
   begin
      P.R := Left.R - Right.R;
      P.G := Left.G - Right.G;
      P.B := Left.B - Right.B;
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
end CV_Ada.Graphics.Pixel;
