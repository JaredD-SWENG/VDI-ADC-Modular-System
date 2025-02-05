with Digital_Out;
with STM32.Device; use STM32.Device;
with STM32.Board; use STM32.Board;
with STM32.PWM; use STM32.PWM;
with STM32.Timers; use STM32.Timers;
with STM32.GPIO; use STM32.GPIO;
with Drive_Motor;
with LCD_Std_Out;

procedure Main is
   -- Declare a Drive_Motor instance
   Motor : Drive_Motor.Motor;

   Delay_Time : constant Duration := 5.0;  -- Delay time for each step

   -- Button Area (for user interaction feedback)
   Button_Area_X      : constant Natural := 10;
   Button_Area_Y      : constant Natural := 20;
   Button_Area_Text   : constant String  := "Drive Motor";

   -- Output Area (for showing signal state)
   Output_Area_X      : constant Natural := 10;
   Output_Area_Y      : constant Natural := 60;

      -- A procedure to draw the static button area text.
   procedure Draw_Button_Area is
   begin
      -- Draw the button label at the designated coordinates.
      LCD_Std_Out.Put (Button_Area_X, Button_Area_Y, Button_Area_Text);
   end Draw_Button_Area;

   -- A procedure to update the output area with a new message.
   procedure Update_Output (Msg : String) is
   begin
      -- Optional: Clear the output area by writing blank spaces.
      -- Adjust the width as needed. Here, we assume 20 characters is sufficient.
      LCD_Std_Out.Put (Output_Area_X, Output_Area_Y, "                    ");
      
      LCD_Std_Out.Put (Output_Area_X, Output_Area_Y, Msg);
   end Update_Output;   
   
begin
   -- Initialize the motor with specified configuration
   Drive_Motor.Initialize (Motor);

   -- Draw the button area
   Draw_Button_Area;


   Delay Delay_Time; 

   loop
      if STM32.GPIO.Set (STM32.Board.User_Button_Point) then

         Update_Output ("Button Pressed");
         Delay Delay_Time;
         
         if STM32.GPIO.Set(STM32.Device.PC8) then
            -- Disable motor PWM output
            Drive_Motor.Disable (Motor);
            Update_Output ("Disabled");

            LCD_Std_Out.Put (10,120,"               ");
            LCD_Std_Out.Put (10,150,"               ");
         else
            -- Enable motor PWM output
            Drive_Motor.Enable (Motor);
            Update_Output ("Enabled");
         end if;
         
         -- Wait for the button to be released.
         while STM32.GPIO.Set (STM32.Board.User_Button_Point) loop
            null;
            delay 0.1;
         end loop;
      end if;

      delay 0.1;

      if STM32.GPIO.Set(STM32.Device.PC8) then

            -- loop speed from 10 to 30
            for I in 5..20 loop
               Drive_Motor.Set_Speed (Motor, I);
               Update_Output ("Speed " & Integer'Image (I));
               Delay 1.0;
            end loop;

            -- loop speed from 30 to 10
            for I in reverse 5.. 20 loop
               Drive_Motor.Set_Speed (Motor, I);
               Update_Output ("Speed " & Integer'Image (I));
               Delay 1.0;
            end loop;

            -- Disable motor PWM output
            Drive_Motor.Disable (Motor);
            Update_Output ("Disabled");

            LCD_Std_Out.Put (10,120,"               ");
            LCD_Std_Out.Put (10,150,"               ");
      end if;

   end loop;

end Main;
