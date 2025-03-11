with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;

-- To find process heap memory use (linux) : cat /proc/$(pgrep -n simulation)/status | grep VmRSS

with System;

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

package body Lane_Detection is
   CAMERA_PATH : constant String := "..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder";

   task body Lane_Detection_Task is
   begin
      accept Start;
         Put_Line ("Lane Detection Started");
         Camera.Initialize (CAMERA_PATH);
         --  Camera.Set_Frame_End_Index (10);

         while Camera.Get_Frame_Index < Camera.Get_Frame_End_Index loop
            declare
               Input       : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Camera.Get_Next_Frame_Path (True));
               Output      : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
               Output_Size : Storage_Count;
            begin
               --  So much damn memory leak everywhere...
               QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
               CV_Ada.IO_Operations.Write_To_File ("output.qoi", Output, Output_Size);
               
               CV_Ada.Free_Input_Data (Input);
               CV_Ada.Free_Storage_Array (Output);
            end;
         end loop;
   end Lane_Detection_Task;
end Lane_Detection;
