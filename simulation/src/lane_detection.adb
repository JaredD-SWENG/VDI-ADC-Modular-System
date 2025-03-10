with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;
with Camera;


with Ada.Directories; -- Dir : String := Ada.Directories.Current_Directory;
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

with Ada.Unchecked_Deallocation;

package body Lane_Detection is

   Base_Path : String := "../data/";

   subtype Image_Count is Integer range 0 .. 3;

   function Get_Next_Frame_Path (Idx : Image_count) return String is
      Nbr : String := Idx'Image;
      Formatted_Nbr : String := Nbr (2 .. Nbr'Last);
      Last_Zero : Integer := 5 - Formatted_Nbr'Length;
      Formatted_Str : String (1 .. 5) := (1 .. Last_Zero => '0') & Formatted_Nbr;
      Frame_Path : String := Base_Path & Formatted_Str & ".qoi";
   begin
      return Frame_Path;
   end Get_Next_Frame_Path;

   task body Lane_Detection_Task is
   begin
      accept Start;
         Put_Line ("Lane Detection Started");
         for Idx in Image_Count'Range loop
            declare
               Frame_Path : String := Get_Next_Frame_Path (Idx);
               Input : CV_Ada.Input_Data := CV_Ada.IO_Operations.Load_QOI (Frame_Path);
               Output : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
               Output_Size : Storage_Count;
            begin
               QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
               CV_Ada.Free_Storage_Array (Input.Data);
               CV_Ada.Free_Storage_Array (Output);
               Put_Line ("Processing frame: " & Frame_Path);
            end;
         end loop;
         
   end Lane_Detection_Task;
end Lane_Detection;
