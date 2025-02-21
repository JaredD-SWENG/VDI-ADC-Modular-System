with Coms_Uart;
with Drive_Motor;
with HAL; use HAL;

procedure Test_TC029 is
   Motor : Drive_Motor.Motor;
   Input_Buffer : String(1 .. 50) := (others => ' ');
   Last_Char    : Natural := 0;

   -- Define test speeds
   type Speed_Array is array (1 .. 5) of Integer;
   Test_Speeds : constant Speed_Array := (10, 30, 50, 70, 90);

   -- Helper to trim spaces from Integer'Image
   function Trim_Integer_Image (I : Integer) return String is
      Img : constant String := Integer'Image(I);
   begin
      return Img(Img'First + 1 .. Img'Last); -- Remove leading space
   end Trim_Integer_Image;

begin
   Coms_Uart.Send_String_Newline("=== TC-029: Frequency Stability Test ===");
   Coms_Uart.Send_String_Newline("Connect scope to PB7. Verify PWM frequency remains constant.");

   -- Initialize Motor
   Drive_Motor.Initialize(Motor);
   Drive_Motor.Enable(Motor);

   -- Loop through test speeds
   for I in Test_Speeds'Range loop
      -- Apply speed to the motor
      Coms_Uart.Send_String("Setting speed to " & Trim_Integer_Image(Test_Speeds(I)) & "%");
      Drive_Motor.Set_Speed(Motor, Test_Speeds(I));

      -- Prompt user to verify frequency stability
      Coms_Uart.Send_String_Newline("Verify PWM frequency remained constant.");
      Coms_Uart.Send_String_Newline("Did frequency remain stable? (y/n)");

      -- Wait for Pass/Fail input
      Coms_Uart.Receive_Line(Input_Buffer, Last_Char, Echo => True);

      if Input_Buffer(1) in 'y' | 'Y' then
         Coms_Uart.Send_String_Newline("Result: PASS");
      elsif Input_Buffer(1) in 'n' | 'N' then
         Coms_Uart.Send_String_Newline("Result: FAIL");
      else
         Coms_Uart.Send_String_Newline("Invalid input. Moving to next test.");
      end if;
   end loop;

   -- Cleanup
   Drive_Motor.Stop(Motor);
   Drive_Motor.Disable(Motor);
   Coms_Uart.Send_String_Newline("Test complete. Power off scope.");

   delay (25.0);
end Test_TC029;
