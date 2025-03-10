with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;
with Camera;

with QOI;
with CV_Ada;
with CV_Ada.Basic_Transformations;
with CV_Ada.Edge_Detection;
with CV_Ada.IO_Operations;
with CV_Ada.Morphological_Operations;
with CV_Ada.Colorspace;
with CV_Ada.Blur;
with CV_Ada.Hough_Transform;
with System.Storage_Elements; use System.Storage_Elements;

procedure Dummy is
   --F_Gen : Ada.Numerics.Float_Random.Generator;
   Input      : CV_Ada.Input_Data;
   --Input      : CV_Ada.Img_Input;
   --Temp_Data  : CV_Ada.Input_Data;

   --  task body Lane_Detection_Task is
   --Offset_Value : Float;

   Frame_Name : String := "..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder\frame_0000.qoi";
   begin
      --  accept Start;
      Put_Line ("Lane Detection Started");

      -- Initialize the simulated camera with the folder path containing QOI frames
      Camera.Initialize ("..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder");

      --Ada.Numerics.Float_Random.Reset (F_Gen);

      while Frame_Name /= "..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder\frame_0900.qoi" loop
         --Put_Line ("Processing frame: " & Frame_Name);
         Frame_Name := Camera.Get_Frame;

         -- Load the current frame using Load_QOI
         --  Temp_Data := CV_Ada.IO_Operations.Load_QOI (Frame_Name);
         --  Input := CV_Ada.Input_Data'(Data => Temp_Data.Data, Desc => Temp_Data.Desc);
         Input := CV_Ada.IO_Operations.Load_QOI (Frame_Name);

         -- Apply region of interest to focus on the lower half of the image
         --  CV_Ada.Basic_Transformations.Region_Of_Interest
         --    (Input.Data.all, Input.Desc, Input.Desc.Width / 4 + 1,
         --     Input.Desc.Height / 3 + 1, 3 * Input.Desc.Width / 4,
         --     2 * Input.Desc.Height / 3);

         -- Convert to grayscale first
         --  CV_Ada.Colorspace.Convert_To_Grayscale (Input);

         -- Rotate the image (optional)
         -- CV_Ada.Basic_Transformations.Rotate_Image (Input.Data.all, Input.Desc, 0.0);
         -- Flip the image horizontally (optional)
         --  CV_Ada.Basic_Transformations.Flip_Image
         --    (Input     => Input,
         --     Direction => CV_Ada.Basic_Transformations.Horizontal);

         -- Sharpen the image (optional)
         -- CV_Ada.Basic_Transformations.Sharpen_Image (Data => Input.Data.all, Desc => Input.Desc);

         -- Apply Sobel edge detection
         --  CV_Ada.Edge_Detection.Sobel_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         -- Increase brightness by 50 (optional)
         --  CV_Ada.Basic_Transformations.Adjust_Brightness
         --    (Input.Data.all, Input.Desc, 50, Flip_Brightness => True);

         -- Invert colors of the loaded image (optional)
         -- CV_Ada.Basic_Transformations.Invert_Colors (Input);

         -- Increase contrast by a factor of 1.2 (optional)
         --Adjust_Contrast (Input.Data.all, Input.Desc, Factor => 1.0);

         -- Blur the image
         -- Blur_Image
         --    (Input.Data.all,
         --     Input.Desc.Width,
         --     Input.Desc.Height,
         --     Input.Desc.Channels);

         -- Apply Canny edge detection
         --  CV_Ada.Edge_Detection.Canny_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels, Low_Threshold => 0.1, High_Threshold => 0.15);

         -- Convert to black and white (optional) WORKS
         --  CV_Ada.Colorspace.Convert_To_Black_And_White
         --    (Input.Data.all, Input.Desc);
         --  CV_Ada.Colorspace.Convert_To_Black_And_White
         --    (Input);

         -- SUPER SLOW OR NOT WORKING
         -- Apply morphological operations (optional)
         --  CV_Ada.Morphological_Operations.Morphological_Operation
         --    (Input, CV_Ada.Morphological_Operations.Erosion, CV_Ada.Morphological_Operations.SE_Square, 10);

         --  CV_Ada.Edge_Detection.Sobel_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         --  CV_Ada.Edge_Detection.Canny_Edge_Detection
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels, Low_Threshold => 0.28,
         --     High_Threshold                     => 0.34);

         -- Apply Hough Transform to detect lines
         --  CV_Ada.Hough_Transform.Hough_Line_Transform
         --    (Input.Data.all, Input.Desc.Width, Input.Desc.Height,
         --     Input.Desc.Channels);

         -- After edge detection and before encoding
         -- Hough_Circle_Transform
         --    (Data        => Input.Data.all,
         --     Width       => Input.Desc.Width,
         --     Height      => Input.Desc.Height,
         --     Channels    => Input.Desc.Channels,
         --     Min_Radius  => 10,
         --     Max_Radius  => 20,
         --     Threshold   => 20,
         --     Max_Circles => 600);

         declare
            --  type Storage_array_access is access all Storage_Array;
            --  Output      : Storage_array_access :=
            --     new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
            --  Output_Size : Storage_Count;
         begin
            --  QOI.Encode
            --     (Input.Data.all, Input.Desc, Output.all, Output_Size);
            --  CV_Ada.IO_Operations.Write_To_File
            --     ("output.qoi", Output.all, Output_Size);

            Put_Line ("Frame processed and saved: " & Frame_Name);

            --delay 0.1;  -- Optional: Simulate a delay between frames if needed.
            -- Replace "frame_end.qoi" with a condition to stop processing.
         end;

         --  Offset_Value :=
         --    (Ada.Numerics.Float_Random.Random (F_Gen) * 2.0) - 1.0;
         --  Offset_Value := Float'Rounding (Offset_Value * 100.0) / 100.0;
         --  Queue_Manager.Enqueue
         --    (new Offset'(Event_Kind => Offset_Event, Value => Offset_Value));
         --  Put_Line ("Lane Detection Offset: " & Float'Image (Offset_Value));
         --  delay 1.0;
      end loop;
   --  end Lane_Detection_Task;
end Dummy;
