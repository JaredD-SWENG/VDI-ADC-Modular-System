with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Edge_Detection is
   procedure Sobel_Edge_Detection
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count);

   procedure Canny_Edge_Detection
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Low_Threshold           : Float := 0.1;
      High_Threshold          : Float := 0.3);
end OpenCV_Ada.Edge_Detection;
