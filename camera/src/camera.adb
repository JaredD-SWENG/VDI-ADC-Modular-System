with Ada.Directories;         use Ada.Directories;
with Ada.Text_IO;             use Ada.Text_IO;

package body Camera is
   function Get_Folder_Path return String is
   begin
      return To_String(Folder_Path_Unbounded);
   end Get_Folder_Path;

   function Get_Frame_Max_Count return Integer is
   begin
      return Max_Frames;
   end Get_Frame_Max_Count;

   function Get_Frame_Index return Integer is
   begin
      return Frame_Index;
   end Get_Frame_Index;

   function Get_Frame_End_Index return Integer is
   begin
      return Frame_End_Index;
   end Get_Frame_End_Index;

   procedure Set_Frame_Index (Index : Integer) is
   begin
      if Index < 0 or Index > Max_Frames then
         raise Constraint_Error with "Invalid frame index";
      else Frame_Index := Index; end if;
   end Set_Frame_Index;

   procedure Set_Frame_End_Index (Index : Integer) is
   begin
      if Index < 0 or Index > Max_Frames then
         raise Constraint_Error with "Invalid frame end index";
      else Frame_End_Index := Index; end if;
   end Set_Frame_End_Index;

   procedure Update_Frame_Count (Search_Item : Directory_Entry_Type) is
   begin
      Max_Frames := @ + 1;
   end Update_Frame_Count;

   procedure Initialize (Folder_Path : String; Path_Pattern : String := "frame_*.qoi") is
      Search_Filter : constant Filter_Type := (Directory => False, Ordinary_File => True, Special_File => False);
   begin
      Folder_Path_Unbounded := To_Unbounded_String(Folder_Path);

      -- Get the number of frames in the folder
      Search(To_String(Folder_Path_Unbounded), Path_Pattern, Search_Filter, Update_Frame_Count'Access);

      Frame_Index     := 0;
      Frame_End_Index := Max_Frames;

   end Initialize;

   function Get_Next_Frame_Path (Log : Boolean := False) return String is
      D_Places       : constant Integer := Max_Frames'Image'Length;
      Nbr            : String := Frame_Index'Image;
      Formatted_Nbr  : String := Nbr (2 .. Nbr'Last);
      Last_Zero      : Integer := D_Places - Formatted_Nbr'Length;
      Formatted_Str  : String (1 .. D_Places) := (1 .. Last_Zero => '0') & Formatted_Nbr;
      Frame_Path     : String := To_String(Folder_Path_Unbounded) & "\frame_" & Formatted_Str & ".qoi";
   begin
      if Log then Put_Line("Frame_Path[" & Frame_Index'Image & " ]: " & Frame_Path ); end if;
      if Frame_Index > Max_Frames then
         return ""; -- No more frames to process
      end if;

      Frame_Index := @ + 1; -- Set the next frame index
      return Frame_Path;
   end Get_Next_Frame_Path;
end Camera;
