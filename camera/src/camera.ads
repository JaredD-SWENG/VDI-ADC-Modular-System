with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Camera is
   function  Get_Folder_Path     return String;
   function  Get_Frame_Max_Count return Integer;
   function  Get_Frame_Index     return Integer;
   function  Get_Frame_End_Index return Integer;

   procedure Set_Frame_Index     (Index : Integer);
   procedure Set_Frame_End_Index (Index : Integer);

   -- Initialize the camera with the path to the folder containing frames
   procedure Initialize (Folder_Path : String; Path_Pattern : String := "frame_*.qoi");
   
   -- Get the next frame path in sequence
   function Get_Next_Frame_Path (Log : Boolean := False) return String;
private
   Folder_Path_Unbounded   : Unbounded_String;
   Max_Frames              : Integer;
   Frame_Index             : Integer;
   Frame_End_Index         : Integer;
end Camera;