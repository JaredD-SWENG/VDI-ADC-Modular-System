with Ada.Text_IO;
with Ada.Directories;

with GNAT.OS_Lib;
with System.Storage_Elements; use System.Storage_Elements;
with System.Multiprocessors;  use System.Multiprocessors;

package body Acv is

   package IO renames Ada.Text_IO;
   package D renames Ada.Directories;
   package OS renames GNAT.OS_Lib;

   function Black_And_White_MT
     (Img : Image_Access; Threshold : Intensity_T) return Image_Access
   is
      Result : Image_Access := new Image_T (Img'Range (1), Img'Range (2));

      Total_Rows    : constant Natural  := Img'Last (2) - Img'First (2) + 1;
      Num_Tasks     : constant Positive :=
        Positive'Min (Integer (Number_Of_CPUs), 4);  -- Limit to 4 threads
      Rows_Per_Task : constant Natural  := Total_Rows / Num_Tasks;
      Remainder     : constant Natural  := Total_Rows rem Num_Tasks;
   begin
      declare
         protected Completion is
            procedure Task_Done;
            entry Wait_Until_Done;
         private
            Tasks_Remaining : Natural := Num_Tasks;
         end Completion;

         protected body Completion is
            procedure Task_Done is
            begin
               Tasks_Remaining := Tasks_Remaining - 1;
            end Task_Done;

            entry Wait_Until_Done when Tasks_Remaining = 0 is
            begin
               null;
            end Wait_Until_Done;
         end Completion;

         task type Process_Rows (Start_Y, End_Y : Natural);

         task body Process_Rows is
         begin
            for Y in Start_Y .. End_Y loop
               for X in Img'Range (1) loop
                  declare
                     Gray : constant Intensity_T :=
                       (Img (X, Y).R + Img (X, Y).G + Img (X, Y).B) / 3;
                  begin
                     Result (X, Y) :=
                       (if Gray < Threshold then (0, 0, 0)
                        else (255, 255, 255));
                  end;
               end loop;
            end loop;
            Completion.Task_Done;
         end Process_Rows;

         type Task_Access is access Process_Rows;
         Tasks   : array (1 .. Num_Tasks) of Task_Access;
         Start_Y : Natural := Img'First (2);
      begin
         for I in 1 .. Num_Tasks loop
            declare
               Adjusted_Rows : Natural := Rows_Per_Task;
            begin
               if I <= Remainder then
                  Adjusted_Rows := Adjusted_Rows + 1;
               end if;

               declare
                  End_Y : constant Natural := Start_Y + Adjusted_Rows - 1;
               begin
                  Tasks (I) :=
                    new Process_Rows
                      (Start_Y => Start_Y,
                       End_Y   => Natural'Min (End_Y, Img'Last (2)));
                  Start_Y   := Start_Y + Adjusted_Rows;
               end;
            end;
         end loop;

         Completion.Wait_Until_Done;
      end;

      return Result;
   end Black_And_White_MT;

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

   function To_QOI
     (Img : Image_T; Output_Size : out Storage_Count) return Storage_Array
   is
      Desc   : QOI.QOI_Desc :=
        (Width      => Storage_Count (Img'Length (1)),
         Height     => Storage_Count (Img'Length (2)), Channels => 3,
         Colorspace => QOI.SRGB);
      Input  : Storage_Array (1 .. (Img'Length (1) * Img'Length (2) * 3)) with
        Address => Img'Address;
      Output : Storage_Array (1 .. QOI.Encode_Worst_Case (Desc));
   begin
      QOI.Encode (Input, Desc, Output, Output_Size);
      return Output;
   end To_QOI;

   procedure Write_To_File
     (Filename : String; D : Storage_Array; Size : Storage_Count)
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
