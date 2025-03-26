with QOI;
with System.Storage_Elements; use System.Storage_Elements;
-- with Ada.Strings;             use Ada.Strings;
-- with Ada.Text_IO;             use Ada.Text_IO;

with CV_Ada.IO_Operations;   use CV_Ada.IO_Operations;
with CV_Ada.Colorspace;      use CV_Ada.Colorspace;
with CV_Ada.Edge_Detection;  use CV_Ada.Edge_Detection;
with CV_Ada.Hough_Transform; use CV_Ada.Hough_Transform;
with CV_Ada.Basic_Transformations; use CV_Ada.Basic_Transformations;

procedure Testing_Image_Functions is
   -- Input_File_Name   : String := "src\images\" & "5x5BlackSquare.qoi";
   -- Input_File_Name  : String := "src\images\" & "hi144p.qoi";
   -- Input_File_Name  : constant String := "src\images\" & "lane1.qoi";
   Base_Path        : constant String := "..\..\..\..\..\..\2025 1 Spring Semester\SWENG 481\Function Testing Images\Hough Cricle Transform\";
   Input_File_Name  : constant String := Base_Path & "coins_scattered.qoi";
   Output_File_Name : constant String := "output.qoi";
   Input            : CV_Ada.Input_Data;
begin
   Input := Load_QOI (Input_File_Name, True);

   ----------------- PRE-REQs ---------------- (Function Pre-requisites)
   -- Convert to grayscale first
   Convert_To_Grayscale (Input);

   -- Blur the image
   --  Blur_Image
   --    (Input.Data.all,
   --     Input.Desc.Width,
   --     Input.Desc.Height,
   --     Input.Desc.Channels);

   -- Apply Sobel edge detection
   Sobel_Edge_Detection (Input);

   -- Apply Canny edge detection
   Canny_Edge_Detection (Input,
      Low_Threshold  => 0.01,
      High_Threshold => 0.1);

   -- Apply Hough Transform to detect lines
   CV_Ada.Hough_Transform.Hough_Circle_Transform(Input,
      Min_Radius  => 100,
      Max_Radius  => 100, 
      Threshold   => 250,
      Max_Circles => 100);
   ------------------  END  ------------------
   
   ----------------- START ----------------- (Function Testing)
   
   -----------------  END  -----------------

   declare
      Output      : CV_Ada.Storage_Array_Access := new Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
      -- Output_Temp : Storage_Array (1 .. Input.Desc.Width * Input.Desc.Height * Input.Desc.Channels) := (others => 150);
      Output_Size : Storage_Count;
   begin
      QOI.Encode (Input.Data.all, Input.Desc, Output.all, Output_Size);
      Write_To_File (Output_File_Name, Output, Output_Size);
   end;
end Testing_Image_Functions;