with Ada.Numerics.Discrete_Random;
with Ada.Text_IO; use Ada.Text_IO;
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
with CV_Ada.Hough_Transform;
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
         begin
            -- Apply region of interest to focus on the lower half of the image
            CV_Ada.Basic_Transformations.Region_Of_Interest
              (Input, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1,
               3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

            -- Convert to grayscale first
            CV_Ada.Colorspace.Convert_To_Grayscale (Input);

            -- Apply edge detection to find edges
            --  CV_Ada.Edge_Detection.Canny_Edge_Detection
            --    (Input, Low_Threshold => 0.1, High_Threshold => 0.15);

            -- Apply Hough Transform to detect lines
            --  CV_Ada.Hough_Transform.Hough_Circle_Transform
            --    (Input, 10, 100, 1, 1);

            Signal_Value := Signal_Generator.Random (S_Gen);
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
