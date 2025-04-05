with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Event_Types;             use Event_Types;
with Event_Queue;             use Event_Queue;
with Interfaces;              use Interfaces;
with QOI;
with CV_Ada;
with CV_Ada.Basic_Transformations;
with CV_Ada.Edge_Detection;
with CV_Ada.IO_Operations;
with CV_Ada.Morphological_Operations;
with CV_Ada.Colorspace;
with CV_Ada.Blur;
with CV_Ada.Hough_Transform;  use CV_Ada.Hough_Transform;
with System.Storage_Elements; use System.Storage_Elements;
with Camera;

package body Signal_Recognition is
   package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
   S_Gen : Signal_Generator.Generator;

   -- Function to convert RGB to HSV
   procedure RGB_To_HSV (R, G, B : in Float; H, S, V : out Float) is
      R_Norm, G_Norm, B_Norm   : Float;
      C_Max, C_Min, Difference : Float;
   begin
      -- Normalize RGB values to [0,1]
      R_Norm := R / 255.0;
      G_Norm := G / 255.0;
      B_Norm := B / 255.0;

      -- Find maximum and minimum values
      C_Max      := Float'Max (R_Norm, Float'Max (G_Norm, B_Norm));
      C_Min      := Float'Min (R_Norm, Float'Min (G_Norm, B_Norm));
      Difference := C_Max - C_Min;

      -- Calculate Hue
      if Difference = 0.0 then
         H := 0.0;
      elsif C_Max = R_Norm then
         H := 60.0 * (Float'Remainder ((G_Norm - B_Norm) / Difference, 6.0));
      elsif C_Max = G_Norm then
         H := 60.0 * (((B_Norm - R_Norm) / Difference) + 2.0);
      else
         H := 60.0 * (((R_Norm - G_Norm) / Difference) + 4.0);
      end if;

      -- Make sure H is in [0, 360]
      if H < 0.0 then
         H := H + 360.0;
      end if;

      -- Calculate Saturation
      if C_Max = 0.0 then
         S := 0.0;
      else
         S := Difference / C_Max;
      end if;

      -- Calculate Value
      V := C_Max;
   end RGB_To_HSV;

   task body Signal_Recognition_Task is
      Signal_Value   : Signal_Color;
      Event_Priority : Event_Types.Priority_Level;
   begin
      accept Start (Priority : Event_Types.Priority_Level := 1) do
         Event_Priority := Priority;
      end Start;
      Put_Line
        ("Signal State Started with Priority:" &
         Event_Types.Priority_Level'Image (Event_Priority));
      loop
         declare
            Current_Frame_Path : String            :=
              Camera.Get_Next_Frame_Path ("Signal_Recognition");
            Input              : CV_Ada.Input_Data :=
              CV_Ada.IO_Operations.Load_QOI (Current_Frame_Path);
            -- Parameters for circle detection
            Min_Radius         : constant Positive :=
              3;     -- Minimum radius to detect
            Max_Radius         : constant Positive :=
              10;     -- Maximum radius to detect
            Vote_Threshold     : constant Positive :=
              60;     -- Minimum votes needed
            Max_Circles        : constant Positive :=
              20;      -- Maximum number of circles to detect
         begin
            -- Apply region of interest to focus on the traffic signal area
            CV_Ada.Basic_Transformations.Region_Of_Interest
              (Input, 2 * Input.Desc.Width / 5 + 1,
               1,           -- Start at 2/5 of width
               3 * Input.Desc.Width / 5, Input.Desc.Height / 2);

            -- Calculate color matches within the ROI for specific traffic signal colors
            declare
               Red_Count    : Integer := 0;
               Yellow_Count : Integer := 0;
               Green_Count  : Integer := 0;
               Pixel_Count  : Integer := 0;

               -- Color values for traffic signals with tolerance ranges in HSV
               -- Green traffic light in HSV
               Green_H_Min : constant Float := 70.0;
               Green_H_Max : constant Float := 170.0;
               Green_S_Min : constant Float := 0.5;
               Green_S_Max : constant Float := 1.0;
               Green_V_Min : constant Float := 0.5;
               Green_V_Max : constant Float := 1.0;

               -- Red traffic light in HSV (red can wrap around 0/360)
               Red_H_Min : constant Float := 340.0;
               Red_H_Max : constant Float := 15.0;  -- Wraps around 0
               Red_S_Min : constant Float := 0.4;
               Red_S_Max : constant Float := 1.0;
               Red_V_Min : constant Float := 0.3;
               Red_V_Max : constant Float := 1.0;

               -- Yellow traffic light in HSV
               Yellow_H_Min : constant Float := 20.0;
               Yellow_H_Max : constant Float := 40.0;
               Yellow_S_Min : constant Float := 0.4;
               Yellow_S_Max : constant Float := 1.0;
               Yellow_V_Min : constant Float := 0.4;
               Yellow_V_Max : constant Float := 1.0;

               -- Helper function to get RGB values from pixel
               procedure Extract_RGB
                 (Index : Storage_Offset; R, G, B : out Float)
               is
               begin
                  -- Assuming RGBA format where each component is one byte
                  -- The correct order depends on your image format
                  R := Float (Unsigned_8 (Input.Data (Index)));
                  G := Float (Unsigned_8 (Input.Data (Index + 1)));
                  B := Float (Unsigned_8 (Input.Data (Index + 2)));
                  -- Alpha would be at Index + 3 if needed
               end Extract_RGB;

               -- Helper function to check if a color is within range
               function Is_In_Range (Value, Min, Max : Float) return Boolean is
               begin
                  return Value >= Min and Value <= Max;
               end Is_In_Range;

               -- Helper function to check if hue is within range, handling the wrap-around at 360 degrees
               function Is_Hue_In_Range (H, Min, Max : Float) return Boolean is
               begin
                  if Min <= Max then
                     return H >= Min and H <= Max;
                  else
                     -- Handle the red hue that wraps around 0
                     return H >= Min or H <= Max;
                  end if;
               end Is_Hue_In_Range;

            begin
               -- Process each pixel in the ROI to count color matches
               for I in 0 .. (Input.Data'Length / 4) - 1 loop
                  declare
                     Index   : constant Storage_Offset :=
                       Storage_Offset
                         (4 * I + 1); -- Base 1 indexing
                     R, G, B : Float;
                     H, S, V : Float;
                  begin
                     Extract_RGB (Index, R, G, B);
                     RGB_To_HSV (R, G, B, H, S, V);

                     -- Check if pixel matches any of the target colors in HSV space
                     if Is_Hue_In_Range (H, Red_H_Min, Red_H_Max) and
                       Is_In_Range (S, Red_S_Min, Red_S_Max) and
                       Is_In_Range (V, Red_V_Min, Red_V_Max)
                     then
                        Red_Count := Red_Count + 1;
                     end if;

                     if Is_Hue_In_Range (H, Yellow_H_Min, Yellow_H_Max) and
                       Is_In_Range (S, Yellow_S_Min, Yellow_S_Max) and
                       Is_In_Range (V, Yellow_V_Min, Yellow_V_Max)
                     then
                        Yellow_Count := Yellow_Count + 1;
                     end if;

                     if Is_Hue_In_Range (H, Green_H_Min, Green_H_Max) and
                       Is_In_Range (S, Green_S_Min, Green_S_Max) and
                       Is_In_Range (V, Green_V_Min, Green_V_Max)
                     then
                        Green_Count := Green_Count + 1;
                     end if;

                     Pixel_Count := Pixel_Count + 1;
                  end;
               end loop;

               -- Determine signal color based on highest count and percentage thresholds
               Put_Line
                 ("Signal color counts - Red: " & Integer'Image (Red_Count) &
                  " Yellow: " & Integer'Image (Yellow_Count) & " Green: " &
                  Integer'Image (Green_Count) & " out of " &
                  Integer'Image (Pixel_Count) & " pixels");

               -- Use proportional thresholds instead of absolute values
               declare
                  Red_Percentage    : Float :=
                    Float (Red_Count) / Float (Pixel_Count);
                  Yellow_Percentage : Float :=
                    Float (Yellow_Count) / Float (Pixel_Count);
                  Green_Percentage  : Float :=
                    Float (Green_Count) / Float (Pixel_Count);
               begin
                  Put_Line
                    ("Red: " & Float'Image (Red_Percentage * 100.0) &
                     "% Yellow: " & Float'Image (Yellow_Percentage * 100.0) &
                     "% Green: " & Float'Image (Green_Percentage * 100.0) &
                     "%");

                  if Red_Percentage >= Yellow_Percentage and
                    Red_Percentage >= Green_Percentage
                  then
                     Signal_Value := Red;
                  elsif Yellow_Percentage >= Red_Percentage and
                    Yellow_Percentage >= Green_Percentage
                  then
                     Signal_Value := Yellow;
                  else
                     Signal_Value := Green;
                  end if;
               end;
            end;
            -- WRITE OUTPUT TO FILE
            declare
               Output      : CV_Ada.Storage_Array_Access :=
                 new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
               Output_Size : Storage_Count;
            begin
               QOI.Encode
                 (Input.Data.all, Input.Desc, Output.all, Output_Size);
               CV_Ada.IO_Operations.Write_To_File
                 ("output2.qoi", Output, Output_Size);
               CV_Ada.Free_Storage_Array (Output);
            end;
            Queue_Manager.Enqueue
              (new Signal_State'
                 (Event_Kind => Signal_Event, Priority => Event_Priority,
                  Color      => Signal_Value));
            --Queue_Manager.PrintQueue;
            Put_Line
              ("Signal Recognition State: " &
               Signal_Color'Image (Signal_Value));

            --  CV_Ada.Free_Input_Data (Input);
            CV_Ada.Free_Storage_Array (Input.Data);
            delay 0.5;
         end;
      end loop;
   end Signal_Recognition_Task;
end Signal_Recognition;

--HOUGH CIRCLE (old)
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Unchecked_Deallocation;  -- Add this import for deallocation
--  with Event_Types; use Event_Types;
--  with Event_Queue; use Event_Queue;

--  with QOI;
--  with CV_Ada;
--  with CV_Ada.Basic_Transformations;
--  with CV_Ada.Edge_Detection;
--  with CV_Ada.IO_Operations;
--  with CV_Ada.Morphological_Operations;
--  with CV_Ada.Colorspace;
--  with CV_Ada.Blur;
--  with CV_Ada.Hough_Transform; use CV_Ada.Hough_Transform;  -- Use clause for visibility
--  with System.Storage_Elements; use System.Storage_Elements;

--  with Camera;

--  package body Signal_Recognition is
--     package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
--     S_Gen : Signal_Generator.Generator;

--     task body Signal_Recognition_Task is
--        Signal_Value   : Signal_Color;
--        Event_Priority : Event_Types.Priority_Level;
--     begin
--        accept Start (Priority : Event_Types.Priority_Level := 1) do
--           Event_Priority := Priority;
--        end Start;

--        Put_Line
--          ("Signal State Started with Priority:" &
--           Event_Types.Priority_Level'Image (Event_Priority));

--        loop
--           declare
--              Current_Frame_Path : String            := Camera.Get_Next_Frame_Path ("Signal_Recognition");
--              Input              : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Current_Frame_Path);
--              Detected_Circles   : Circle_Array_Access;  -- Now visible due to use clause

--              -- Parameters for circle detection
--              Min_Radius      : constant Positive := 10;     -- Minimum radius to detect
--              Max_Radius      : constant Positive := 75;     -- Maximum radius to detect
--              Vote_Threshold  : constant Positive := 50;     -- Minimum votes needed
--              Max_Circles     : constant Positive := 25;      -- Maximum number of circles to detect
--           begin
--              -- Apply region of interest to focus on the lower half of the image
--              CV_Ada.Basic_Transformations.Region_Of_Interest
--                (Input, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1,
--                 3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

--              -- Convert to grayscale first
--              CV_Ada.Colorspace.Convert_To_Grayscale (Input);

--              -- Apply edge detection to find edges
--              CV_Ada.Edge_Detection.Canny_Edge_Detection
--                (Input, Low_Threshold => 0.1, High_Threshold => 0.15);

--              -- Apply Hough Circle Transform to detect circles
--              CV_Ada.Hough_Transform.Hough_Circle_Transform
--                (Input,
--                 Min_Radius     => Min_Radius,
--                 Max_Radius     => Max_Radius,
--                 Radius_Step    => 2,            -- Use step 2 for faster performance
--                 Vote_Threshold => Vote_Threshold,
--                 Max_Circles    => Max_Circles,
--                 Circles        => Detected_Circles);

--              -- Process detected circles
--              if Detected_Circles /= null and then Detected_Circles'Length > 0 then
--                 -- Log the number of circles found
--                 Put_Line("Found" & Integer'Image(Detected_Circles'Length) & " traffic signal circles");

--                 -- Use the largest/strongest circle for signal color detection
--                 -- In a real implementation, you might analyze the pixel colors within the circle
--                 -- Here we're just using the random generator as a placeholder
--                 Signal_Value := Signal_Generator.Random (S_Gen);

--                 -- You could also use the size or position of the circles to determine if it's a traffic signal
--                 -- For example, analyze the largest detected circle:
--                 Put_Line("Strongest circle at position (" &
--                          Integer'Image(Detected_Circles(1).X) & "," &
--                          Integer'Image(Detected_Circles(1).Y) & ") with radius" &
--                          Integer'Image(Detected_Circles(1).Radius));

--                 -- Free the allocated memory for circles
--                 declare
--                    -- Define the procedure properly with the correct types
--                    procedure Free_Circles is new Ada.Unchecked_Deallocation
--                      (Object => Circle_Array,
--                       Name   => Circle_Array_Access);
--                 begin
--                    Free_Circles(Detected_Circles);
--                 end;
--              else
--                 -- No circles detected, use default behavior
--                 Signal_Value := Signal_Generator.Random (S_Gen);
--                 Put_Line("No traffic signal circles detected");
--              end if;

--              -- WRITE OUTPUT TO FILE
--              declare
--                 Output      : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
--                 Output_Size : Storage_Count;
--              begin
--                 QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
--                 CV_Ada.IO_Operations.Write_To_File ("output2.qoi", Output, Output_Size);
--                 CV_Ada.Free_Storage_Array (Output);
--              end;

--              Queue_Manager.Enqueue
--                (new Signal_State'
--                   (Event_Kind => Signal_Event, Priority => Event_Priority,
--                    Color      => Signal_Value));
--              --Queue_Manager.PrintQueue;
--              Put_Line
--                ("Signal Recognition State: " &
--                 Signal_Color'Image (Signal_Value));

--              --  CV_Ada.Free_Input_Data (Input);
--              CV_Ada.Free_Storage_Array (Input.Data);
--              delay 0.1;
--           end;
--        end loop;
--     end Signal_Recognition_Task;
--  end Signal_Recognition;

-- Random Colors
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Text_IO; use Ada.Text_IO;
--  with Event_Types; use Event_Types;
--  with Event_Queue; use Event_Queue;

--  with QOI;
--  with CV_Ada;
--  with CV_Ada.Basic_Transformations;
--  with CV_Ada.Edge_Detection;
--  with CV_Ada.IO_Operations;
--  with CV_Ada.Morphological_Operations;
--  with CV_Ada.Colorspace;
--  with CV_Ada.Blur;
--  with CV_Ada.Hough_Transform;
--  with System.Storage_Elements; use System.Storage_Elements;

--  with Camera;

--  package body Signal_Recognition is
--     package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
--     S_Gen : Signal_Generator.Generator;

--     task body Signal_Recognition_Task is
--        Signal_Value   : Signal_Color;
--        Event_Priority : Event_Types.Priority_Level;
--     begin
--        accept Start (Priority : Event_Types.Priority_Level := 1) do
--           Event_Priority := Priority;
--        end Start;

--        Put_Line
--          ("Signal State Started with Priority:" &
--           Event_Types.Priority_Level'Image (Event_Priority));

--        loop
--           declare
--              Current_Frame_Path : String            := Camera.Get_Next_Frame_Path ("Signal_Recognition");
--              Input              : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Current_Frame_Path);
--           begin
--              -- Apply region of interest to focus on the lower half of the image
--              CV_Ada.Basic_Transformations.Region_Of_Interest
--                (Input, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1,
--                 3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

--              -- Convert to grayscale first
--              CV_Ada.Colorspace.Convert_To_Grayscale (Input);

--              -- Apply edge detection to find edges
--              --  CV_Ada.Edge_Detection.Canny_Edge_Detection
--              --    (Input, Low_Threshold => 0.1, High_Threshold => 0.15);

--              -- Apply Hough Transform to detect lines
--              --  CV_Ada.Hough_Transform.Hough_Circle_Transform
--              --    (Input, 10, 100, 1, 1);

--              Signal_Value := Signal_Generator.Random (S_Gen);
--              -- WRITE OUTPUT TO FILE
--              declare
--                 Output      : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
--                 Output_Size : Storage_Count;
--              begin
--                 QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
--                 CV_Ada.IO_Operations.Write_To_File ("output2.qoi", Output, Output_Size);
--                 CV_Ada.Free_Storage_Array (Output);
--              end;

--              Queue_Manager.Enqueue
--                (new Signal_State'
--                   (Event_Kind => Signal_Event, Priority => Event_Priority,
--                    Color      => Signal_Value));
--              --Queue_Manager.PrintQueue;
--              Put_Line
--                ("Signal Recognition State: " &
--                 Signal_Color'Image (Signal_Value));

--              --  CV_Ada.Free_Input_Data (Input);
--              CV_Ada.Free_Storage_Array (Input.Data);
--              delay 0.1;
--           end;
--        end loop;
--     end Signal_Recognition_Task;
--  end Signal_Recognition;
