with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with Reference_QOI;

package body Load_QOI is
   procedure Write_To_File
     (Filename : String; D : Storage_Array; Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;
      Ret : Integer;
   begin

      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Ret := Write (FD, D'Address, Integer (Size));

      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);
   end Write_To_File;

   function Get_QOI (Filename : String) return Input_Data is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;
      Ret : Integer;

      Result : Input_Data;
   begin

      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         Len     : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : constant Storage_Array_Access :=
           new Storage_Array (1 .. Len);
      begin
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         QOI.Get_Desc (In_Data.all, Result.Desc);

         declare
            Out_Len     : constant Storage_Count :=
              Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
            Out_Data    : constant Storage_Array_Access :=
              new Storage_Array (1 .. Out_Len);
            Output_Size : Storage_Count;
         begin
            QOI.Decode
              (Data        => In_Data.all,
               Desc        => Result.Desc,
               Output      => Out_Data.all,
               Output_Size => Output_Size);

            Result.Data := Out_Data;

            if Reference_QOI.Check_Decode
                 (In_Data.all,
                  Result.Desc,
                  Out_Data.all
                    (Out_Data'First .. Out_Data'First + Output_Size - 1))
            then
               Put_Line ("Compare with reference decoder: OK");
            else
               Put_Line ("Compare with reference decoder: FAIL");
               GNAT.OS_Lib.OS_Exit (1);
            end if;

            return Result;
         end;
      end;
   end Get_QOI;
end Load_QOI;