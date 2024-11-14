with System.Storage_Elements; use System.Storage_Elements;

package Img is

   -- Add these type declarations after the existing ones
   type Circle_Parameters is record
      X, Y  : Natural;  -- Center coordinates
      R     : Natural;  -- Radius
      Votes : Natural;       -- Number of votes
   end record;

   type Circle_Array is array (Positive range <>) of Circle_Parameters;
   type Circle_Array_Access is access Circle_Array;


   function Get_Pixel
     (Data                    : Storage_Array;
      X, Y                    : Natural;
      Width, Height, Channels : Natural;
      Offset                  : Natural := 0) return Storage_Element;

end;