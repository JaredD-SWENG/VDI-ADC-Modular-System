with QOI;
with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Colorspace is
   -- Convert RGB(A) image to grayscale
   procedure Convert_To_Grayscale
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc);
   
   procedure Convert_To_Black_And_White
     (Data      : in out Storage_Array;
      Desc      : QOI.QOI_Desc;
      Threshold : Storage_Element := 128);
end OpenCV_Ada.Colorspace;
