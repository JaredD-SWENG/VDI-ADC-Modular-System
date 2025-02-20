with Coms_Uart;
with Drive_Motor;
with HAL; use HAL;
with Ada.Real_Time; use Ada.Real_Time;

procedure Test_TC026 is
   Motor : Drive_Motor.Motor;
   Input_Buffer : String(1 .. 50) := (others => ' ');
   Last_Char    : Natural := 0;
   Start_Time, Change_Time : Time;
   Delta_Time   : Time_Span;

begin
   Coms_Uart.Send_String_Newline("=== TC-026: Deceleration Response Test ===");
   Coms_Uart.Send_String_Newline("Connect scope to PB7. Press [y] to continue or [n] to abort...");

   Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);

   if Last_Char = 0 then
      Coms_Uart.Send_String_Newline("Aborting: No input");
      return;
   end if;

   if Input_Buffer(1) in 'y' | 'Y' then
      -- Initialize Motor
      Drive_Motor.Initialize(Motor);
      Drive_Motor.Enable(Motor);

      -- Phase 1: Initial 20% duty cycle
      Coms_Uart.Send_String_Newline("Setting 20% speed...");
      Drive_Motor.Set_Speed(Motor, 20);
      Coms_Uart.Send_String_Newline("TAKE SCOPE CAPTURE NOW (20% Speed). Press y to decelerate...");

      -- Wait for user confirmation to decelerate
      Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
      if Input_Buffer(1) in 'y' | 'Y' then
         -- Phase 2: Decelerate to 5%
         Start_Time := Clock;
         Drive_Motor.Set_Speed(Motor, 5);

         -- Wait for deceleration completion
         Change_Time := Clock;
         Delta_Time := Change_Time - Start_Time;

         -- Report timing
         Coms_Uart.Send_String("Deceleration time: ");
         Coms_Uart.Send_Time_Span(Delta_Time);
         Coms_Uart.Send_String_Newline(" - TAKE 5% CAPTURE NOW");

         -- Cleanup
         Coms_Uart.Send_String_Newline("Press y to power down...");
         Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
         if Input_Buffer(1) in 'y' | 'Y' then
            Drive_Motor.Stop(Motor);
            Drive_Motor.Disable(Motor);
         end if;
      else
         Coms_Uart.Send_String_Newline("Deceleration aborted.");
      end if;
   else
      Coms_Uart.Send_String_Newline("Test aborted.");
   end if;
end Test_TC026;
