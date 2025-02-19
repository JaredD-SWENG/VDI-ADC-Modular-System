with STM32.Device; use STM32.Device;
with STM32.Board;  use STM32.Board;
with STM32.PWM;    use STM32.PWM;
with STM32.Timers; use STM32.Timers;
with STM32.GPIO;   use STM32.GPIO;
with Drive_Motor;
with Coms_Uart;

procedure Main is

   Motor : Drive_Motor.Motor;
   Delay_Time : constant Duration := 5.0;  -- Delay time for each step

   ----------------------------------------------------------------------------
   -- helper procedure to log status over UART.
   ----------------------------------------------------------------------------
   procedure Update_Output (Msg : String) is
   begin
      -- Send the string with a newline at the end.
      Coms_Uart.Send_String_Newline (Msg);
   end Update_Output;

begin
   ----------------------------------------------------------------------------
   -- Initialize motor
   ----------------------------------------------------------------------------
   Drive_Motor.Initialize (Motor);

   -- Log that the system is starting
   Update_Output ("System Init: Drive Motor Ready");

   -- delay
   delay Delay_Time;

   ----------------------------------------------------------------------------
   -- Main Loop
   ----------------------------------------------------------------------------
   loop
      -- Check if the user button is pressed
      if STM32.GPIO.Set (STM32.Board.User_Button_Point) then
         Update_Output ("Button Pressed");
         delay Delay_Time;

         -- Check PC8 state to determine whether to Enable or Disable the motor
         if STM32.GPIO.Set (PC8) then
            -- Disable motor PWM output
            Drive_Motor.Disable (Motor);
            Update_Output ("Motor Disabled");
         else
            -- Enable motor PWM output (and internally Calibrate)
            Drive_Motor.Enable (Motor);
            Update_Output ("Motor Enabled");
         end if;

         -- Wait for the button to be released.
         while STM32.GPIO.Set (STM32.Board.User_Button_Point) loop
            null;
            delay 0.1;
         end loop;
      end if;

      delay 0.1;

      -- Another check on PC8 to do a speed ramp if set
      if STM32.GPIO.Set (PC8) then
         -- Loop motor speed from 5% to 20%
         for I in 5 .. 20 loop
            Drive_Motor.Set_Speed (Motor, I);
            Update_Output ("Speed => " & Integer'Image (I));
            delay 1.0;
         end loop;

         -- Then from 20% back down to 5%
         for I in reverse 5 .. 20 loop
            Drive_Motor.Set_Speed (Motor, I);
            Update_Output ("Speed => " & Integer'Image (I));
            delay 1.0;
         end loop;

         -- Finally, disable motor PWM output
         Drive_Motor.Disable (Motor);
         Update_Output ("Motor Disabled");
      end if;
   end loop;

end Main;
