package CV_Ada.Graphics.Pixel is
   type Pixel is record
      R, G, B : Float;
   end record;

   function "+" (Left : pixel; Right : pixel) return pixel;
   function "-" (Left : pixel; Right : pixel) return pixel;
   function "*" (Left : pixel; Right : Float) return pixel;
   function "/" (Left : pixel; Right : Float) return pixel;
end CV_Ada.Graphics.Pixel;
