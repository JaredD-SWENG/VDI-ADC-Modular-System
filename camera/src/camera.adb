package body Camera is
   Folder_Path : Unbounded_String := Null_Unbounded_String;
   Frame_Index : Integer := 0;
   Max_Frames  : constant Integer := 954;

   procedure Initialize (Folder : String) is
   begin
      -- Convert the folder path to an unbounded string
      Folder_Path := To_Unbounded_String(Folder);
      if To_String(Folder_Path)(Folder'Last) /= '/' then
         Folder_Path := Folder_Path & To_Unbounded_String("/");
      end if;
      Frame_Index := 0;
   end Initialize;

   function Get_Frame return String is
      Frame_Name : Unbounded_String;
   begin
      if Frame_Index > Max_Frames then
         return ""; -- No more frames to process
      end if;

      -- Hard-code zero-padding logic for frame numbers
      if Frame_Index < 10 then
         Frame_Name := Folder_Path & To_Unbounded_String("frame_000") &
                       To_Unbounded_String(Integer'Image(Frame_Index)(2 .. 2)) &
                       To_Unbounded_String(".qoi");
      elsif Frame_Index < 100 then
         Frame_Name := Folder_Path & To_Unbounded_String("frame_00") &
                       To_Unbounded_String(Integer'Image(Frame_Index)(2 .. 3)) &
                       To_Unbounded_String(".qoi");
      elsif Frame_Index < 1000 then
         Frame_Name := Folder_Path & To_Unbounded_String("frame_0") &
                       To_Unbounded_String(Integer'Image(Frame_Index)(2 .. 4)) &
                       To_Unbounded_String(".qoi");
      else
         raise Constraint_Error with "Frame index exceeds maximum allowed digits.";
      end if;

      -- Increment the frame index for the next call
      Frame_Index := Frame_Index + 1;

      return To_String(Frame_Name); -- Convert unbounded string back to a fixed string for return
   end Get_Frame;
end Camera;