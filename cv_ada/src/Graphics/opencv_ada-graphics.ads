with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Graphics is
   procedure Draw_Line
     (Data                    : in out Storage_Array;
      X1, Y1, X2, Y2          : Integer;
      Width, Height, Channels : Storage_Count;
      Color                   : Storage_Element := 255);

   function Get_Pixel
     (Data                    : Storage_Array;
      X, Y                    : Storage_Count;
      Width, Height, Channels : Storage_Count) return Storage_Element;
end OpenCV_Ada.Graphics;
