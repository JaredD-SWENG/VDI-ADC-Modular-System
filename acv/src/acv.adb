with Ada.Text_IO;
with Ada.Directories;

with GNAT.OS_Lib;
with System.Storage_Elements; use System.Storage_Elements;

package body Acv is

   package IO renames Ada.Text_IO;
   package D renames Ada.Directories;
   package OS renames GNAT.OS_Lib;
   function Black_And_White
     (Img : Image_T; Threshold : Intensity_T) return Image_T
   is
      Result : Image_T (Img'Range (1), Img'Range (2));
      Gray   : Intensity_T;
   begin
      for Y in Img'Range (2) loop
         for X in Img'Range (1) loop
            Gray          := (Img (X, Y).R + Img (X, Y).G + Img (X, Y).B) / 3;
            Result (X, Y) :=
              (if Gray < Threshold then (0, 0, 0) else (255, 255, 255));
         end loop;
      end loop;
      return Result;
   end Black_And_White;

   function Gray (Img : Image_T) return Image_T is
      Result : Image_T (Img'Range (1), Img'Range (2));
      Gray   : Intensity_T;
   begin
      for Y in Img'Range (2) loop
         for X in Img'Range (1) loop
            Gray          := (Img (X, Y).R + Img (X, Y).G + Img (X, Y).B) / 3;
            Result (X, Y) := (Gray, Gray, Gray);
         end loop;
      end loop;
      return Result;
   end Gray;

   function Raw_Img_Data
     (Directory : String; File_Name : String) return Storage_Array
   is
      use OS;
      File_Path : String := D.Compose (Directory, File_Name, "qoi");
      FD        : File_Descriptor;
      Ret       : Integer;
   begin
      IO.Put_Line ("File Path: " & File_Path);
      FD := OS.Open_Read (File_Path, Binary);
      if FD = Invalid_FD then
         IO.Put_Line (IO.Standard_Error, Errno_Message);
         OS_Exit (1);
      end if;

      declare
         Len      : constant Storage_Count := Storage_Count (File_Length (FD));
         Img_Data : Storage_Array (0 .. Len - 1);
      begin
         Ret := Read (FD, Img_Data'Address, Img_Data'Length);
         if Ret /= Img_Data'Length then
            IO.Put_Line (Errno_Message);
            OS_Exit (1);
         end if;

         Close (FD);

         return Img_Data;
      end;
   end Raw_Img_Data;

   function Qoi_Desc
     (Directory : String; File_Name : String) return Qoi.QOI_Desc
   is
      Img_Data : Storage_Array := Raw_Img_Data (Directory, File_Name);
      Desc     : QOI.QOI_Desc;
   begin
      QOI.Get_Desc (Img_Data, Desc);
      return Desc;
   end Qoi_Desc;

   function From_QOI (Directory : String; File_Name : String) return Image_T is
      use OS;
      Img_Data : Storage_Array := Raw_Img_Data (Directory, File_Name);
      Desc     : QOI.QOI_Desc;
   begin
      QOI.Get_Desc (Img_Data, Desc);
      if Desc.Channels /= 3 then
         IO.Put_Line ("Error: Image must have 3 channels");
         OS_Exit (1);
      end if;

      declare
         Out_Len     : constant Storage_Count :=
           Desc.Width * Desc.Height * Desc.Channels;
         Output      : Storage_Array (0 .. Out_Len - 1);
         Img         :
           Image_T
             (0 .. Integer (Desc.Width) - 1,
              1 .. Integer (Desc.Height) - 1) with
           Address => Output'Address;
         Output_Size : Storage_Count;
      begin
         QOI.Decode (Img_Data, Desc, Output, Output_Size);
         return Img;
      end;
   end From_QOI;

   function To_QOI (Img : Image_T; Output_Size : out Storage_Count) return Storage_Array is
      Desc : QOI.QOI_Desc := (
         Width      => Storage_Count (Img'Length (1)),
         Height     => Storage_Count (Img'Length (2)),
         Channels   => 3,
         Colorspace => QOI.SRGB);
      Input       : Storage_Array (1 .. (Img'Length (1) * Img'Length (2) * 3)) with Address => Img'Address;
      Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Desc));
   begin
      QOI.Encode (Input, Desc, Output, Output_Size);
      return Output;
   end To_Qoi;

   procedure Write_To_File (Filename : String; D : Storage_Array; Size : Storage_Count)
   is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;  -- File descriptor for the output file
      Ret : Integer;          -- Return value from Write operation
   begin
      -- Create a new binary file
      FD := GNAT.OS_Lib.Create_File (Filename, Binary);

      -- Check if file creation failed
      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      -- Write Size bytes from array D to file
      Ret := Write (FD, D'Address, Integer (Size));

      -- Verify all bytes were written
      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      -- Close the file
      Close (FD);
   end Write_To_File;

   function Imgs_Count_In_Dir (Dir : String) return Natural is
      use Ada.Directories;
      Iter     : Search_Type;
      Dir_Type : Directory_Entry_Type;
      Count    : Natural := 0;
   begin
      Start_Search
        (Search => Iter, Directory => Dir, Pattern => "*.qoi",
         Filter => (Ordinary_File => True, others => False));

      while More_Entries (Iter) loop
         Count := Count + 1;
         Get_Next_Entry (Iter, Dir_Type);
      end loop;

      End_Search (Iter);

      return Count;
   end Imgs_Count_In_Dir;

end Acv;
