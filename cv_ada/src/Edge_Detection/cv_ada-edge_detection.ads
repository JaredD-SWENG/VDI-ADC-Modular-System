package CV_Ada.Edge_Detection is
   procedure Sobel_Edge_Detection
      (Input : in out Input_Data);

   procedure Canny_Edge_Detection
      (Input : in out Input_Data;
       Low_Threshold           : Float := 0.1;
       High_Threshold          : Float := 0.3);
end CV_Ada.Edge_Detection;


--  package CV_Ada.Edge_Detection is
--     procedure Sobel_Edge_Detection
--        (Data                    : in out Storage_Array;
--         Width, Height, Channels : Storage_Count);

--     procedure Canny_Edge_Detection
--        (Data                    : in out Storage_Array;
--         Width, Height, Channels : Storage_Count;
--         Low_Threshold           : Float := 0.1;
--         High_Threshold          : Float := 0.3);
--  end CV_Ada.Edge_Detection;
