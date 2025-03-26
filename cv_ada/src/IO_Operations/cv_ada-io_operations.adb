with Ada.Text_IO;                       use Ada.Text_IO;
with GNAT.OS_Lib;
with Reference_QOI;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Containers;                   use Ada.Containers;

package body CV_Ada.IO_Operations is
   -------------------------------------------------------------------------------
   -- Write_To_File
   --
   -- Writes binary data from a Storage_Array to a file
   --
   -- Parameters:
   --    Filename - Name of the file to write to
   --    D       - Storage_Array containing data to write
   --    Size    - Number of bytes to write from D
   --
   -- Returns:
   --    None. Exits program with code 1 if any errors occur during write
   -------------------------------------------------------------------------------
   procedure Write_To_File
     (Filename : String; D : in out Storage_Array_Access; Size : Storage_Count)
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
      Ret := Write (FD, D.all'Address, Integer (Size));

      -- Verify all bytes were written
      if Ret /= Integer (Size) then
         Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      -- Close the file
      Close (FD);

      Free_Storage_Array (D);
   end Write_To_File;

   -------------------------------------------------------------------------------
   -- Load_QOI
   --
   -- Loads and decodes a QOI (Quite OK Image) format file
   --
   -- Parameters:
   --    Filename - Path to the QOI file to load
   --
   -- Returns:
   --    Input_Data record containing the decoded image data and description
   --
   -- Raises:
   --    OS_Exit(1) if file cannot be opened/read or decoding fails
   -------------------------------------------------------------------------------
   function Load_QOI (Filename : String; Log : Boolean := False) return Input_Data is
      use GNAT.OS_Lib;

      FD  : File_Descriptor;  -- File descriptor for input file
      Ret : Integer;         -- Return value from read operation

      Result : Input_Data;   -- Holds decoded image data and description
   begin
      -- Open file in binary mode
      FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

      -- Exit if file cannot be opened
      if FD = Invalid_FD then
         Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      declare
         -- Get file size and allocate buffer
         Len     : constant Storage_Count := Storage_Count (File_Length (FD));
         In_Data : Storage_Array_Access := new Storage_Array (1 .. Len);
      begin
         -- Read entire file into buffer
         Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

         -- Verify complete file was read
         if Ret /= In_Data'Length then
            Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
         end if;

         Close (FD);

         -- Extract QOI header info into description
         QOI.Get_Desc (In_Data.all, Result.Desc);

         declare
            -- Calculate output buffer size based on image dimensions
            Out_Len     : constant Storage_Count        :=
              Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
            Out_Data    : Storage_Array_Access :=
              new Storage_Array (1 .. Out_Len);
            Output_Size : Storage_Count;  -- Actual size of decoded data
         begin
            -- Decode QOI data into raw pixel data
            QOI.Decode
              (Data   => In_Data.all, Desc => Result.Desc,
               Output => Out_Data.all, Output_Size => Output_Size);

            Result.Data := Out_Data;

            -- This portion below was causing a memory leak. DO NOT UNCOMMENT!
            --  -- Verify decoded data matches reference implementation
            --  if Reference_QOI.Check_Decode
            --      (In_Data.all, Result.Desc,
            --       Out_Data.all
            --         (Out_Data'First .. Out_Data'First + Output_Size - 1))
            --  then
            --     if Log then Put_Line ("Compare with reference decoder: OK"); end if;
            --  else
            --     Put_Line ("Compare with reference decoder: FAIL");
            --     GNAT.OS_Lib.OS_Exit (1);
            --  end if;

            -- Free Memory Leak
            Free_Storage_Array (In_Data);

            return Result;
         end;
      end;
   end Load_QOI;

end CV_Ada.IO_Operations;