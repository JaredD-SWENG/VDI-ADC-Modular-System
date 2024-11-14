package Img.Alg is
   procedure Draw_Line
     (Data                    : in out Storage_Array;
      X1, Y1, X2, Y2          : Integer;
      Width, Height, Channels : Natural;
      Color                   : Storage_Element := 255);
end Img.Alg;