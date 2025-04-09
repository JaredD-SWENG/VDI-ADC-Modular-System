with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Directories;       use Ada.Directories;

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
   
private
   -- Private variables for internal state
   Folder_Path_Unbounded : Unbounded_String;
   Max_Frames : Integer := 0;
   Frame_Index : Integer := 0;
   Frame_End_Index : Integer := 0;
   
   -- Helper procedure for counting frames
   procedure Update_Frame_Count (Search_Item : Directory_Entry_Type);
end Camera;