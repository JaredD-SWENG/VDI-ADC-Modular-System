with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Graphics is
   procedure Draw_Line
     (Data                    : in out Storage_Array;
      X1, Y1, X2, Y2          : Integer;
      Width, Height, Channels : Storage_Count;
      Color                   : Storage_Element := 255);
end OpenCV_Ada.Graphics;
