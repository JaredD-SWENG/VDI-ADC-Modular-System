with RC_Car_Display;
with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
   Update_Interval : constant Time_Span := Milliseconds(100);
   Next_Update : Time := Clock;
   Counter : Natural := 0;
begin
   RC_Car_Display.Initialize;

   loop
      RC_Car_Display.Update_Motor(Counter mod 100);
      RC_Car_Display.Update_Steering((Counter / 2) mod 180 - 90);
      RC_Car_Display.Update_Battery(12.4 + Float(Counter mod 10)/10.0);
      
      RC_Car_Display.Debug_Message("Test Line " & Integer'Image(Counter));

      Counter := Counter + 1;
      Next_Update := Next_Update + Update_Interval;
      delay until Next_Update;
   end loop;
end Main;
