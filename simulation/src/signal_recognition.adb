with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;  -- Add this import for deallocation
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;

with QOI;
with CV_Ada;
with CV_Ada.Basic_Transformations;
with CV_Ada.Edge_Detection;
with CV_Ada.IO_Operations;
with CV_Ada.Morphological_Operations;
with CV_Ada.Colorspace;
with CV_Ada.Blur;
with CV_Ada.Hough_Transform; use CV_Ada.Hough_Transform;  -- Use clause for visibility
with System.Storage_Elements; use System.Storage_Elements;

with Camera;

package body Signal_Recognition is
   package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
   S_Gen : Signal_Generator.Generator;

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
            Current_Frame_Path : String            := Camera.Get_Next_Frame_Path ("Signal_Recognition");
            Input              : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Current_Frame_Path);
            Detected_Circles   : Circle_Array_Access;  -- Now visible due to use clause
            
            -- Parameters for circle detection
            Min_Radius      : constant Positive := 10;     -- Minimum radius to detect
            Max_Radius      : constant Positive := 75;     -- Maximum radius to detect
            Vote_Threshold  : constant Positive := 50;     -- Minimum votes needed
            Max_Circles     : constant Positive := 25;      -- Maximum number of circles to detect
         begin
            -- Apply region of interest to focus on the lower half of the image
            CV_Ada.Basic_Transformations.Region_Of_Interest
              (Input, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1,
               3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

            -- Convert to grayscale first
            CV_Ada.Colorspace.Convert_To_Grayscale (Input);

            -- Apply edge detection to find edges
            CV_Ada.Edge_Detection.Canny_Edge_Detection
              (Input, Low_Threshold => 0.1, High_Threshold => 0.15);

            -- Apply Hough Circle Transform to detect circles
            CV_Ada.Hough_Transform.Hough_Circle_Transform
              (Input, 
               Min_Radius     => Min_Radius,
               Max_Radius     => Max_Radius,
               Radius_Step    => 2,            -- Use step 2 for faster performance
               Vote_Threshold => Vote_Threshold,
               Max_Circles    => Max_Circles,
               Circles        => Detected_Circles);

            -- Process detected circles
            if Detected_Circles /= null and then Detected_Circles'Length > 0 then
               -- Log the number of circles found
               Put_Line("Found" & Integer'Image(Detected_Circles'Length) & " traffic signal circles");
               
               -- Use the largest/strongest circle for signal color detection
               -- In a real implementation, you might analyze the pixel colors within the circle
               -- Here we're just using the random generator as a placeholder
               Signal_Value := Signal_Generator.Random (S_Gen);
               
               -- You could also use the size or position of the circles to determine if it's a traffic signal
               -- For example, analyze the largest detected circle:
               Put_Line("Strongest circle at position (" & 
                        Integer'Image(Detected_Circles(1).X) & "," & 
                        Integer'Image(Detected_Circles(1).Y) & ") with radius" &
                        Integer'Image(Detected_Circles(1).Radius));
               
               -- Free the allocated memory for circles
               declare
                  -- Define the procedure properly with the correct types
                  procedure Free_Circles is new Ada.Unchecked_Deallocation
                    (Object => Circle_Array,
                     Name   => Circle_Array_Access);
               begin
                  Free_Circles(Detected_Circles);
               end;
            else
               -- No circles detected, use default behavior
               Signal_Value := Signal_Generator.Random (S_Gen);
               Put_Line("No traffic signal circles detected");
            end if;

            -- WRITE OUTPUT TO FILE
            declare
               Output      : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
               Output_Size : Storage_Count;
            begin
               QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
               CV_Ada.IO_Operations.Write_To_File ("output2.qoi", Output, Output_Size);
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
            delay 0.1;
         end;
      end loop;
   end Signal_Recognition_Task;
end Signal_Recognition;
