with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Blur is
   procedure Average_Blur
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count);

   procedure Gaussian_Blur
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Sigma                   : Float := 1.4);
end OpenCV_Ada.Blur;
