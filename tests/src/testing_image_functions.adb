with QOI;
with System.Storage_Elements; use System.Storage_Elements;
with Load_QOI;                use Load_QOI;
-- with Ada.Strings;             use Ada.Strings;
-- with Ada.Text_IO;             use Ada.Text_IO;

with Opencv_Ada.Colorspace;      use Opencv_Ada.Colorspace;
with Opencv_Ada.Edge_Detection;  use Opencv_Ada.Edge_Detection;
with Opencv_Ada.Hough_Transform; use Opencv_Ada.Hough_Transform;

procedure Testing_Image_Functions is
   -- Input_File_Name   : String := "src\images\" & "5x5BlackSquare.qoi";
   -- Input_File_Name  : String := "src\images\" & "hi144p.qoi";
   Input_File_Name  : constant String := "src\images\" & "lane1.qoi";
   Output_File_Name : constant String := "output.qoi";
   Input            : Input_Data;
begin
   Input := Get_QOI (Input_File_Name);

   ----------------- PRE-REQs ---------------- (Function Pre-requisites)
   -- Convert to grayscale first
   Convert_To_Grayscale (Input.Data.all, Input.Desc);

   -- Blur the image
   --  Blur_Image
   --    (Input.Data.all,
   --     Input.Desc.Width,
   --     Input.Desc.Height,
   --     Input.Desc.Channels);

   -- Apply Sobel edge detection
   Sobel_Edge_Detection
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels);

   -- Apply Canny edge detection
   Canny_Edge_Detection
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels,
      Low_Threshold  => 0.1,
      High_Threshold => 0.3);

   -- Apply Hough Transform to detect lines
   Hough_Line_Transform
     (Input.Data.all,
      Input.Desc.Width,
      Input.Desc.Height,
      Input.Desc.Channels);
   ------------------  END  ------------------

   declare
      Output      : Storage_Array (1 .. QOI.Encode_Worst_Case (Input.Desc));
      -- Output_Temp : Storage_Array (1 .. Input.Desc.Width * Input.Desc.Height * Input.Desc.Channels) := (others => 150);
      Output_Size : Storage_Count;

      ----------------- START ----------------- (Function Testing)

      -----------------  END  -----------------
   begin

      QOI.Encode (Input.Data.all, Input.Desc, Output, Output_Size);
      Write_To_File (Output_File_Name, Output, Output_Size);
   end;
end Testing_Image_Functions;