package CV_Ada.Basic_Transformations is
   type Directions is (Horizontal, Vertical);

   procedure Region_Of_Interest
     (Data   : in out Storage_Array; Desc : in out QOI.QOI_Desc;
      X1, Y1 : in     Storage_Count;  -- Top-left corner of ROI
      X2, Y2 : in     Storage_Count); -- Bottom-right corner of ROI

   procedure Rotate_Image
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Angle : Float);

   procedure Flip_Image
     (Input : in out CV_Ada.Input_Data; Direction : Directions);

   procedure Adjust_Brightness
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Brightness : Integer;
      Flip_Brightness :        Boolean);

   procedure Adjust_Contrast
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc; Factor : Float);

   procedure Invert_Colors (Input : in out Input_Data);

   procedure Sharpen_Image (Data : in out Storage_Array; Desc : QOI.QOI_Desc);
end CV_Ada.Basic_Transformations;
