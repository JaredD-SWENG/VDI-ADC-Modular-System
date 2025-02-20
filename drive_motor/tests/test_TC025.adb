with Coms_Uart;
with Drive_Motor;
with HAL; use HAL;
with Ada.Real_Time; use Ada.Real_Time;

procedure Test_TC025 is
   Motor : Drive_Motor.Motor;
   Input_Buffer : String(1 .. 50) := (others => ' ');
   Last_Char    : Natural := 0;
   Start_Time, Change_Time : Time;
   Delta_Time   : Time_Span;

begin
   Coms_Uart.Send_String_Newline("=== TC-025: Acceleration Response Test ===");
   Coms_Uart.Send_String_Newline("Connect scope to PB7. Press [y] to continue or [n] to abort...");

   Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);

   if Last_Char = 0 then
      Coms_Uart.Send_String_Newline("Aborting: No input");
      return;
   end if;

   if Input_Buffer(1) in 'y' | 'Y' then
      Drive_Motor.Initialize(Motor);
      Drive_Motor.Enable(Motor);

      -- Phase 1: Initial 5% duty cycle
      Coms_Uart.Send_String_Newline("Setting 5% speed...");
      Drive_Motor.Set_Speed(Motor, 5);
      Coms_Uart.Send_String_Newline("TAKE SCOPE CAPTURE NOW (5% Speed). Press y to continue...");
      
      Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
      if Input_Buffer(1) in 'y' | 'Y' then
         -- Phase 2: Ramp to 20%
         Start_Time := Clock;
         Drive_Motor.Set_Speed(Motor, 20);
         
         -- Wait for acceleration completion
         Change_Time := Clock;
         Delta_Time := Change_Time - Start_Time;

         Coms_Uart.Send_String("Acceleration time: ");
         Coms_Uart.Send_Time_Span(Delta_Time);
         Coms_Uart.Send_String_Newline(" - TAKE 20% CAPTURE NOW");
         
         -- Cleanup
         Coms_Uart.Send_String_Newline("Press ENTER to power down...");
         Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
         Drive_Motor.Stop(Motor);
         Drive_Motor.Disable(Motor);
      end if;
   end if;
end Test_TC025;
