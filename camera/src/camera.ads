with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;

with CV_Ada;
with System; use System;
with Ada.Containers.Indefinite_Vectors;

package Camera is
   -- Functions to get camera state
   function Get_Folder_Path return String;
   function Get_Frame_Max_Count return Integer;
   function Get_Frame_Index return Integer;
   function Get_Frame_End_Index return Integer;
   function Get_Current_Global_Frame return Integer;
   
   -- Procedures to modify camera state
   procedure Set_Frame_Index (Index : Integer);
   procedure Set_Frame_End_Index (Index : Integer);
   procedure Advance_Global_Frame;
   
   -- Initialize the camera with a folder path
   procedure Initialize (Folder_Path : String; Path_Pattern : String := "frame_*.qoi");
   
   -- Get the path to the current frame
   function Get_Next_Frame_Path (Module_Name : String := ""; Prefix : String := "frame_"; Log : Boolean := False) return String;
   
   function "=" (Left, Right : CV_Ada.Input_Data) return Boolean is
     (Left'Address = Right'Address);
   
   package Frames is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Natural,
      Element_Type => CV_Ada.Input_Data,
      "="          => "=");

   subtype Video is Frames.Vector;

   Vid : Video;
private
   -- Private variables for internal state
   Folder_Path_Unbounded : Unbounded_String;
   Max_Frames : Integer := 0;
   Frame_Index : Integer := 0;
   Frame_End_Index : Integer := 0;
   
   -- Helper procedure for counting frames
   procedure Update_Frame_Count (Search_Item : Directory_Entry_Type);
   function Load_Video (Dir : String) return Video;
end Camera;