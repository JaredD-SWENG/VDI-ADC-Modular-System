with QOI;
with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package Acv is
   type Intensity_T is range 0 .. (2**8) - 1 with
     Size => 8;
   type Pixex_T is record
      R, G, B : Intensity_T;
   end record;
   type Image_T is array (Natural range <>, Natural range <>) of Pixex_T;
   type Image_Access is access all Image_T;

   function "=" (Left, Right : Image_T) return Boolean is
     (Left'Address = Right'Address);

   function Black_And_White_MT
     (Img : Image_Access; Threshold : Intensity_T) return Image_Access;
   function Gray (Img : Image_T) return Image_T;

   function Qoi_Desc
     (Directory : String; File_Name : String) return Qoi.QOI_Desc;
   function From_Qoi (Directory : String; File_Name : String) return Image_T;

   function To_QOI
     (Img : Image_T; Output_Size : out Storage_Count) return Storage_Array;
   function Imgs_Count_In_Dir (Dir : String) return Natural;

   procedure Write_To_File
     (Filename : String; D : Storage_Array; Size : Storage_Count);
end Acv;
