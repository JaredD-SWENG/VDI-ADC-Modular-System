package OpenCV_Ada.Graphics.Pixel is
   type Pixel is record
      R, G, B : Float;
   end record;

   function Get_Pixel
     (Data                    : Storage_Array;
      X, Y                    : Storage_Count;
      Width, Height, Channels : Storage_Count) return Storage_Element;

   function "+" (Left : pixel; Right : pixel) return pixel;
   function "*" (Left : pixel; Right : Float) return pixel;
   function "/" (Left : pixel; Right : Float) return pixel;
end OpenCV_Ada.Graphics.Pixel;
