with Ada.Command_Line;
with GNAT.OS_Lib;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Float_Text_IO;

with QOI;
with AAA.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with System.Storage_Elements; use System.Storage_Elements;

with CV_Ada.IO_Operations;   use CV_Ada.IO_Operations;
with CV_Ada;                 use CV_Ada;

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
      New_Line;
      Input1 := Load_QOI (Ada.Command_Line.Argument (1));
      Input2 := Load_QOI (Ada.Command_Line.Argument (2));
      New_Line;
   else
      Put_Line (Standard_Error, "File extension can only be '.qoi' for both!");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   if Input1.Desc.Width * Input1.Desc.Height /= Input2.Desc.Width * Input2.Desc.Height
   then
      Put ("Image sizes are different! [");
      Put (Input1.Desc.Width'Image & "x" & Trim(Input1.Desc.Height'Image, Ada.Strings.Left));
      Put (" !=");
      Put (Input2.Desc.Width'Image & "x" & Trim(Input2.Desc.Height'Image, Ada.Strings.Left) & " ]");
      GNAT.OS_Lib.OS_Exit (1);
   end if;

   declare
      Output           : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input1.Desc));
      Output_Size      : Storage_Count;
      Difference_Count : Integer          := 0;
      Index            : Storage_Offset   := 1;
      Pixel_Index      : Storage_Offset;
      Threshold        : constant Integer := 5 * 3;
      Visual_Count     : Integer          := 0;
      Log_Raw          : constant Boolean := False; -- Set to True to output raw differences
   begin
      while Index <= Input1.Desc.Width * Input1.Desc.Height * Input1.Desc.Channels loop
         Pixel_Index := (Index - 1) / 3 + 1;

         if Input1.Data (Index) = Input2.Data (Index)
           and Input1.Data (Index + 1) = Input2.Data (Index + 1)
           and Input1.Data (Index + 2) = Input2.Data (Index + 2)
         then
            Input1.Data (Index) := 0;
            Input1.Data (Index + 1) := 0;
            Input1.Data (Index + 2) := 0;
         else
            if Log_Raw then
               Put_Line ("Difference [" & Pixel_Index'Image & " ]: " 
                  & "[ " & Tail(Input1.Data (Index)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 2)'Image, 3, ' ') & " ]" 
                  & " != "
                  & "[ " & Tail(Input2.Data (Index)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 2)'Image, 3, ' ') & " ]");
            end if;
            
            if abs(Integer(Input1.Data(Index)+Input1.Data(Index+1)+Input1.Data(Index+2)) - Integer(Input2.Data(Index)+Input2.Data(Index+1)+Input2.Data(Index+2))) > Threshold
            then
               if not Log_Raw then
                  Put_Line ("Difference [" & Pixel_Index'Image & " ]: " 
                  & "[ " & Tail(Input1.Data (Index)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input1.Data (Index + 2)'Image, 3, ' ') & " ]" 
                  & " != "
                  & "[ " & Tail(Input2.Data (Index)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 1)'Image, 3, ' ') & ", " & Tail(Input2.Data (Index + 2)'Image, 3, ' ') & " ]");
               end if;
               Visual_Count := Visual_Count + 1;
            end if;

            Input1.Data (Index) := 255;
            Input1.Data (Index + 1) := 0;
            Input1.Data (Index + 2) := 0;
            Difference_Count := Difference_Count + 1;
         end if;
         Index := Index + 3;
      end loop;

      if Difference_Count = 0 then
         Put_Line ("Images are the same.");
      else
         New_Line;
         Put_Line ("Pixel Difference:" & Difference_Count'Image & " /" & Integer(Input1.Desc.Width * Input1.Desc.Height)'Image);
         Put ("Error Percentage (Raw): ");
         Ada.Float_Text_IO.Put ((Float(Difference_Count) / Float(Input1.Desc.Width * Input1.Desc.Height)) * 100.0, 0, 3, 0);
         Put_Line ("%");
         Put ("Error Percentage (Visual): ");
         Ada.Float_Text_IO.Put ((Float(Visual_Count) / Float(Input1.Desc.Width * Input1.Desc.Height)) * 100.0, 0, 3, 0);
         Put_Line ("%");
      end if;

      QOI.Encode (Input1.Data.all, Input1.Desc, Output.all, Output_Size);

      if Output_Size /= 0 then
         Write_To_File ("src\test_cases\" & "image_comparison.qoi", Output, Output_Size);
      else
         Put_Line ("Encode failed");
         GNAT.OS_Lib.OS_Exit (1);
      end if;
   end;
end Tests;