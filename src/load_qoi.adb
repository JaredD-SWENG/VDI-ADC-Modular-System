with QOI;
with Ada.Text_IO;             use Ada.Text_IO;
with System.Storage_Elements; use System.Storage_Elements;
with GNAT.OS_Lib;
with Reference_QOI;

procedure Load_Qoi is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

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

   function Load_QOI (Filename : String) return Input_Data is
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
   end Load_QOI;

   -- Convert RGB(A) image to grayscale
   procedure Convert_To_Grayscale (Data : in out Storage_Array; Desc : QOI.QOI_Desc) is
      Pixel_Size : constant Storage_Count := Storage_Count (Desc.Channels);
      Gray_Value : Storage_Element;
   begin
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if I mod Pixel_Size = 1 then
            -- Convert to grayscale using luminosity method
            -- Y = 0.299R + 0.587G + 0.114B
            Gray_Value := Storage_Element
              ((Integer (Data (I)) * 299 +
                Integer (Data (I + 1)) * 587 +
                Integer (Data (I + 2)) * 114) / 1000);
            
            -- Set RGB channels to the same gray value
            Data (I) := Gray_Value;     -- R
            Data (I + 1) := Gray_Value; -- G
            Data (I + 2) := Gray_Value; -- B
            
            -- Preserve alpha channel if it exists
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Grayscale;

   -- Add blur functionality
   procedure Blur_Image (Data : in out Storage_Array; Width, Height, Channels : Storage_Count) is
      Temp : Storage_Array := Data;
      Kernel_Size : constant Storage_Count := 6;
      Half_Kernel : constant Storage_Count := Kernel_Size / 2;
   begin
      for Y in Half_Kernel .. Height - Half_Kernel - 1 loop
         for X in Half_Kernel .. Width - Half_Kernel - 1 loop
            for C in 0 .. Channels - 1 loop
               declare
                  Sum : Integer := 0;
                  Count : Integer := 0;
               begin
                  -- Apply box blur kernel
                  for KY in -Half_Kernel .. Half_Kernel loop
                     for KX in -Half_Kernel .. Half_Kernel loop
                        declare
                           Pos : constant Storage_Count := 
                             ((Y + KY) * Width + (X + KX)) * Channels + C + 1;
                        begin
                           Sum := Sum + Integer (Temp (Pos));
                           Count := Count + 1;
                        end;
                     end loop;
                  end loop;
                  
                  -- Store averaged result
                  Data (((Y * Width) + X) * Channels + C + 1) := 
                    Storage_Element (Sum / Count);
               end;
            end loop;
         end loop;
      end loop;
   end Blur_Image;

   Input : Input_Data;
begin
   Input := Load_QOI ("input.qoi");
   
   -- Convert the image to grayscale
   Convert_To_Grayscale (Input.Data.all, Input.Desc);

   -- Apply blur to the decoded image data
   Blur_Image (Input.Data.all, 
               Input.Desc.Width, 
               Input.Desc.Height, 
               Input.Desc.Channels);
   
   declare
      Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
      Output_Size : Storage_Count;
   begin
      QOI.Encode (Input.Data.all, Input.Desc, Output, Output_Size);
      Write_To_File ("output.qoi", Output, Output_Size);
   end;
end Load_Qoi;