with Ada.Numerics.Float_Random;
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
with GUI_Function;

package body Lane_Detection is
   type Point is record
      X, Y : Integer;
   end record;

   -- Function to calculate intersection point of two lines
   function Calculate_Intersection
     (Line1_X1, Line1_Y1, Line1_X2, Line1_Y2 : Integer;
      Line2_X1, Line2_Y1, Line2_X2, Line2_Y2 : Integer; Found : out Boolean)
      return Point
   is
      -- Line equations: ax + by + c = 0
      A1 : Float := Float (Line1_Y2 - Line1_Y1);
      B1 : Float := Float (Line1_X1 - Line1_X2);
      C1 : Float := Float (Line1_X2 * Line1_Y1 - Line1_X1 * Line1_Y2);

      A2 : Float := Float (Line2_Y2 - Line2_Y1);
      B2 : Float := Float (Line2_X1 - Line2_X2);
      C2 : Float := Float (Line2_X2 * Line2_Y1 - Line2_X1 * Line2_Y2);

      Det    : Float := A1 * B2 - A2 * B1;
      Result : Point;
   begin
      -- Check if lines are parallel
      if abs (Det) < 0.001 then
         Found := False;
         return (0, 0);
      end if;

      -- Calculate intersection
      Result.X := Integer (Float'Rounding ((B1 * C2 - B2 * C1) / Det));
      Result.Y := Integer (Float'Rounding ((A2 * C1 - A1 * C2) / Det));
      Found    := True;

      return Result;
   end Calculate_Intersection;

   task body Lane_Detection_Task is
      Offset_Value          : Float;
      Left_Line, Right_Line : CV_Ada.Hough_Transform.Line_Parameters;

      -- Default values
      Intersection_Found : Boolean := False;
      Intersection_Point : Point;
      Image_Center       : Integer;

   begin
      accept Start (Priority : Event_Types.Priority_Level := 1) do
         Event_Priority := Priority;
      end Start;

      Put_Line
        ("Lane Detection Started with Priority:" &
         Event_Types.Priority_Level'Image (Event_Priority));

      loop
         declare
            Current_Frame_Path : String            := Camera.Get_Next_Frame_Path ("Lane_Detection");
            Input              : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Current_Frame_Path);
         begin
            if Current_Frame_Path = "" then
               exit; -- No more frames
            end if;
            -- Apply region of interest to focus on the lower half of the image
            CV_Ada.Basic_Transformations.Region_Of_Interest
              (Input, Input.Desc.Width / 4 + 1, Input.Desc.Height / 3 + 1,
               3 * Input.Desc.Width / 4, 2 * Input.Desc.Height / 3);

            -- Convert to grayscale first
            CV_Ada.Colorspace.Convert_To_Grayscale (Input);

            -- Apply Canny edge detection
            CV_Ada.Edge_Detection.Canny_Edge_Detection (Input, Low_Threshold => 0.1, High_Threshold => 0.15);

            -- Apply Hough Transform to detect lines
            CV_Ada.Hough_Transform.Hough_Line_Transform (Input, Left_Line => Left_Line, Right_Line => Right_Line);

            -- Calculate image center (horizontal midpoint)
            Image_Center := Integer (Input.Desc.Width) / 2;

            -- Calculate intersection of left and right lane lines
            if Left_Line.X1 /= 0 and Right_Line.X1 /= 0 then
               Intersection_Point :=
                 Calculate_Intersection
                   (Left_Line.X1, Left_Line.Y1, Left_Line.X2, Left_Line.Y2,
                    Right_Line.X1, Right_Line.Y1, Right_Line.X2, Right_Line.Y2,
                    Intersection_Found);

               -- Calculate offset from center
               if Intersection_Found then
                  -- Calculate offset as distance from center (positive = right, negative = left)
                  Offset_Value :=
                    Float (Intersection_Point.X - Image_Center) /
                    Float (Image_Center);
                  -- Normalize to -1.0 to 1.0 range
                  Offset_Value :=
                    Float'Min (Float'Max (Offset_Value, -1.0), 1.0);
               else
                  -- Fallback if no intersection found
                  Offset_Value := 0.0;
               end if;
            else
               -- Fallback if lanes not detected
               Offset_Value := 0.0;
            end if;

            -- Round to 2 decimal places
            Offset_Value := Float'Rounding (Offset_Value * 100.0) / 100.0;

            --  GUI_Functions.Simulation_Wrapper.SetLeftImage (Input);
            --  GUI_Function.Simulation_Task.SetLeftImage (Input);
            --  GUI_Function.SetLeftImage (Input);

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
              (new Offset'
                 (Event_Kind => Offset_Event, Priority => Event_Priority,
                  Value      => Offset_Value));
            --  Queue_Manager.PrintQueue;
            -- For some reason, path planning is not receiving the offset event
            Put_Line ("Lane Detection Offset: " & Float'Image (Offset_Value));
            --  GUI_Functions.Simulation_Wrapper.AddConsoleText ("Lane Detection Offset: " & Float'Image (Offset_Value));
            --  GUI_Function.Simulation_Task.AddConsoleText ("Lane Detection Offset: " & Float'Image (Offset_Value));
            --  GUI_Function.AddConsoleText ("Lane Detection Offset: " & Float'Image (Offset_Value));
            
            --  CV_Ada.Free_Input_Data (Input);
            CV_Ada.Free_Storage_Array (Input.Data);
            delay 0.1;
         end;
      end loop;
   end Lane_Detection_Task;
end Lane_Detection;