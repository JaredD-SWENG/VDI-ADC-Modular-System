with Ada.Command_Line;
with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;

with QOI;
with AAA.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with System.Storage_Elements; use System.Storage_Elements;

with Load_QOI; use Load_QOI;

procedure Tests is
   Input1 : Input_Data;
   Input2 : Input_Data;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Put_Line (Standard_Error, "Usage: test_input_image_buffer <testing image> <actual image>");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (1), ".qoi")
     and AAA.Strings.Has_Suffix (Ada.Command_Line.Argument (2), ".qoi")
   then
      Put_Line ("QOI Image 1: " & Ada.Command_Line.Argument (1));
      Put_Line ("QOI Image 2: " & Ada.Command_Line.Argument (2));
      Input1 := Get_QOI (Ada.Command_Line.Argument (1));
      Input2 := Get_QOI (Ada.Command_Line.Argument (2));
   else
      Put_Line (Standard_Error, "File extension can only be '.qoi' for both!");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Input1.Desc.Width * Input1.Desc.Height /= Input2.Desc.Width * Input2.Desc.Height
   then
      Put_Line ("Image sizes are different!");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   declare
      Output           : Storage_Array (1 .. QOI.Encode_Worst_Case (Input1.Desc));
      Output_Size      : Storage_Count;
      Difference_Found : Integer := 0;
      Index            : Storage_Offset := 1;
   begin
      while Index <= Input1.Desc.Width * Input1.Desc.Height * Input1.Desc.Channels loop
         if Input1.Data (Index) = Input2.Data (Index)
           and Input1.Data (Index + 1) = Input2.Data (Index + 1)
           and Input1.Data (Index + 2) = Input2.Data (Index + 2)
         then
            Input1.Data (Index) := 0;
            Input1.Data (Index + 1) := 0;
            Input1.Data (Index + 2) := 0;
         else
            Put_Line ("Difference [" & Index'Image & " ]: " 
               & "[ " & Tail(Input1.Data (Index)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 2)'Image, 3, ' ') & " ]" 
               & " != "
               & "[ " & Tail(Input2.Data (Index)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 2)'Image, 3, ' ') & " ]");

            Input1.Data (Index) := 255;
            Input1.Data (Index + 1) := 0;
            Input1.Data (Index + 2) := 0;
            Difference_Found := 1;
         end if;
         Index := Index + 3;
      end loop;

      if Difference_Found = 0 then
         Put_Line ("Image are the same.");
      end if;

      QOI.Encode (Input1.Data.all, Input1.Desc, Output, Output_Size);

      if Output_Size /= 0 then
         Write_To_File ("src\test_cases\" & "image_comparison.qoi", Output, Output_Size);
      else
         Put_Line ("Encode failed");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end;
end Tests;