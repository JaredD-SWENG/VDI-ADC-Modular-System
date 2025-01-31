package CV_Ada.Colorspace is
   procedure Convert_To_Grayscale
      (Data        : in out Storage_Array;
       Desc        : QOI.QOI_Desc);

   procedure Convert_To_Black_And_White
      (Data        : in out Storage_Array;
       Desc        : QOI.QOI_Desc;
       Threshold   : Storage_Element := 128);
end CV_Ada.Colorspace;
