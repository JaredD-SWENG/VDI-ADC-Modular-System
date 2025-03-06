with Uart;
with Motor;
-- with UI;

with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
   Period  : constant Time_Span := Milliseconds (200);
   Next_Release  : Time := Clock;
   Speed : Integer := 0;
begin
   Uart.Init;
   Motor.Init;
   loop
      Speed := (if Uart.Emergency_Stop then 0 else Uart.Get_Speed_Cmd);
      Motor.Set_Speed_Motor_1 (Speed);

      -- UI.Update (Encoder.Get_Speed_Motor_1);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Main;