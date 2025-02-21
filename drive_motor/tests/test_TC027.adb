with Coms_Uart;
with Drive_Motor;
with HAL; use HAL;
with Ada.Real_Time; use Ada.Real_Time;

procedure Test_TC027 is
   Motor : Drive_Motor.Motor;
   Input_Buffer : String(1 .. 50) := (others => ' ');
   Last_Char    : Natural := 0;
   Start_Time, Stop_Time : Time;
   Delta_Time   : Time_Span;

begin
   Coms_Uart.Send_String_Newline("=== TC-027: Emergency Stop Test ===");
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

      -- Phase 1: Start at 20% speed
      Coms_Uart.Send_String_Newline("Running at 20% speed...");
      Drive_Motor.Set_Speed(Motor, 20);
      Coms_Uart.Send_String_Newline("TAKE SCOPE CAPTURE NOW (20% Speed). Press y to trigger emergency stop...");

      -- Wait for user confirmation to trigger stop
      Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
      if Input_Buffer(1) in 'y' | 'Y' then
         -- Phase 2: Trigger Emergency Stop
         Start_Time := Clock;
         Drive_Motor.Emergency_Stop(Motor);
         Stop_Time := Clock;
         Delta_Time := Stop_Time - Start_Time;

         -- Report timing
         Coms_Uart.Send_String("Emergency stop time: ");
         Coms_Uart.Send_Time_Span(Delta_Time);
         Coms_Uart.Send_String_Newline(" - VERIFY MOTOR STOPPED IMMEDIATELY");

         -- Cleanup
         Coms_Uart.Send_String_Newline("Press y to power down...");
         Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);
         if Input_Buffer(1) in 'y' | 'Y' then
            Drive_Motor.Disable(Motor);
         end if;
      else
         Coms_Uart.Send_String_Newline("Emergency stop aborted.");
      end if;
   else
      Coms_Uart.Send_String_Newline("Test aborted.");
   end if;
end Test_TC027;
