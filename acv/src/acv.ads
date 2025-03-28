with QOI;
with System; use System;

package Acv is
   type Intensity_T is range 0 .. (2 ** 8) - 1 with Size => 8;
   type Pixex_T is record
      R, G, B : Intensity_T;
   end record;
   type Image_T is array (Natural range <>, Natural range <>) of Pixex_T;
   type Image_Access is access all Image_T;

   function "=" (Left, Right : Image_T) return Boolean is
      (Left'Address = Right'Address);

   function Black_And_White (Img : Image_T; Threshold : Intensity_T) return Image_T;
   function Gray (Img : Image_T) return Image_T;

   function Qoi_Desc (Directory : String; File_Name : String) return Qoi.QOI_Desc;
   function From_Qoi (Directory : String; File_Name : String) return Image_T;

   procedure To_Qoi (Img : Image_T; Directory : String; File_Name : String);
   function Imgs_Count_In_Dir (Dir : String) return Natural;
end Acv;
