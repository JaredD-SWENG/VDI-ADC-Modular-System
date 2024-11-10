package body OpenCV_Ada.Graphics is
   procedure Draw_Line
     (Data                    : in out Storage_Array;
      X1, Y1, X2, Y2          : Integer;
      Width, Height, Channels : Storage_Count;
      Color                   : Storage_Element := 255)
   is
      DX     : constant Integer := abs (X2 - X1);
      DY     : constant Integer := abs (Y2 - Y1);
      Step_X : constant Integer := (if X1 < X2 then 1 else -1);
      Step_Y : constant Integer := (if Y1 < Y2 then 1 else -1);
      Error  : Integer := (if DX > DY then DX else -DY) / 2;
      X      : Integer := X1;
      Y      : Integer := Y1;
   begin
      loop
         -- Draw pixel if within bounds
         if X > 0 and X <= Integer (Width) and Y > 0 and Y <= Integer (Height)
         then
            -- Set all channels to the line color
            for C in 0 .. Integer (Channels) - 1 loop
               declare
                  Index : constant Storage_Offset :=
                    Storage_Offset
                      (((Y - 1) * Integer (Width) + (X - 1))
                       * Integer (Channels)
                       + C
                       + 1);
               begin
                  Data (Index) := Color;
               end;
            end loop;
         end if;

         exit when X = X2 and Y = Y2;

         declare
            Error2 : constant Integer := Error;
         begin
            if Error2 > -DX then
               Error := Error - DY;
               X := X + Step_X;
            end if;
            if Error2 < DY then
               Error := Error + DX;
               Y := Y + Step_Y;
            end if;
         end;
      end loop;
   end Draw_Line;
end OpenCV_Ada.Graphics;
