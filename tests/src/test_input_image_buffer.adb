with Ada.Command_Line;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;

with QOI;
with AAA.Strings;
with System.Storage_Elements; use System.Storage_Elements;

with Load_QOI;                use Load_QOI;

procedure Test_Input_Image_Buffer is
Input1            : Input_Data;
Input2            : Input_Data;
    begin
        if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error, "Usage: tests <infile> <outfile>");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".qoi") and AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".qoi") then
      Put_Line ("QOI Image 1: " & Ada.Command_Line.Argument (1));
      Put_Line ("QOI Image 2: " & Ada.Command_Line.Argument (2));
      Input1 := Get_QOI ("src\images\" & Ada.Command_Line.Argument (1));
      Input2 := Get_QOI ("src\images\" & Ada.Command_Line.Argument (2));
   else
      Put_Line (Standard_Error, "File extension can only be '.qoi' for both!");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

    for Index in 1 .. Input1.Desc.Width * Input1.Desc.Height loop
        if Input1.
    end loop;

   declare
        Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Input1.Desc));
        Output_Size : Storage_Count;
    begin

    end;
end Test_Input_Image_Buffer;