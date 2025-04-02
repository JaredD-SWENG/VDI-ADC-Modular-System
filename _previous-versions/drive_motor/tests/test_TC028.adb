with Coms_Uart;
with Drive_Motor;
with HAL; use HAL;

procedure Test_TC028 is
   Motor : Drive_Motor.Motor;
   Input_Buffer : String(1 .. 50) := (others => ' ');
   Last_Char    : Natural := 0;

   -- Define test speeds
   type Speed_Array is array (1 .. 7) of Integer;
   Test_Speeds : constant Speed_Array := (-5, 5, 15, 50, 90, 100, 110);

   -- Use access types for dynamic strings
   type String_Ptr is access constant String;
   Speed_Labels : constant array (1 .. 7) of String_Ptr :=
     (new String'("10% Below Bottom Limit"),
      new String'("Bottom Limit"),
      new String'("10% Above Bottom Limit"),
      new String'("Mid-range (50%)"),
      new String'("10% Below Upper Limit"),
      new String'("Upper Limit"),
      new String'("10% Above Upper Limit"));

begin
   Coms_Uart.Send_String_Newline("=== TC-028: Speed Limit Enforcement Test ===");
   Coms_Uart.Send_String_Newline("Connect scope to PB7. Follow prompts to verify results.");

   -- Initialize Motor
   Drive_Motor.Initialize(Motor);
   Drive_Motor.Enable(Motor);

   -- Loop through test speeds
   for I in Test_Speeds'Range loop
      -- Apply speed to the motor
      Coms_Uart.Send_String_Newline("Applying speed: " & Speed_Labels(I).all);
      Drive_Motor.Set_Speed(Motor, Test_Speeds(I));

      -- Prompt user to verify the result
      Coms_Uart.Send_String_Newline("Verify scope reflects expected result.");
      Coms_Uart.Send_String_Newline("Did the driver correctly handle this input? (y/n)");

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

   delay(25.0);
end Test_TC028;
