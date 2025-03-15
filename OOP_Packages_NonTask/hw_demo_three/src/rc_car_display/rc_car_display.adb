-- rc_car_display.adb
with HAL.Bitmap;
with STM32.Board;
with LCD_Std_Out;

package body RC_Car_Display is
   ----------------------------------------------------------------------------
   -- Initialize --
   -- Sets up the display hardware, clears the screen, and draws static labels 
   -- for motor, steering, and battery status.
   ----------------------------------------------------------------------------
   procedure Initialize is
   begin
      STM32.Board.Display.Initialize;
      STM32.Board.Display.Initialize_Layer(1, HAL.Bitmap.RGB_565);
      LCD_Std_Out.Current_Text_Color := HAL.Bitmap.White;
      LCD_Std_Out.Current_Background_Color := HAL.Bitmap.Black;
      LCD_Std_Out.Clear_Screen;

      -- Draw static labels
      LCD_Std_Out.Put(Status_Start_X, Motor_Y,    "Motor:   0%");
      LCD_Std_Out.Put(Status_Start_X, Steering_Y, "Steering: 0");
      LCD_Std_Out.Put(Status_Start_X, Battery_Y,  "Battery: 0.0V");
   end Initialize;

   ----------------------------------------------------------------------------
   -- Update_Motor --
   -- Updates the motor speed display with the given speed value.
   ----------------------------------------------------------------------------
   procedure Update_Motor(Speed : Natural) is
      Buffer : String := "Motor: " & Natural'Image(Speed) & "%  ";
   begin
      LCD_Std_Out.Put(Status_Start_X, Motor_Y, Buffer);
   end Update_Motor;

   ----------------------------------------------------------------------------
   -- Update_Steering --
   -- Updates the steering angle display with the given angle value.
   ----------------------------------------------------------------------------
   procedure Update_Steering(Angle : Integer) is
      Buffer : String := "Steering: " & Integer'Image(Angle) & " ";
   begin
      LCD_Std_Out.Put(Status_Start_X, Steering_Y, Buffer);
   end Update_Steering;

   ----------------------------------------------------------------------------
   -- Update_Battery --
   -- Updates the battery voltage display with the given voltage value.
   ----------------------------------------------------------------------------
   procedure Update_Battery(Voltage : Float) is
      Buffer : String := "Battery: " & Float'Image(Voltage)(1..4) & "V";
   begin
      LCD_Std_Out.Put(Status_Start_X, Battery_Y, Buffer);
   end Update_Battery;

   ----------------------------------------------------------------------------
   -- Debug_Message --
   -- Adds a new debug message to the debug area and shifts previous messages up.
   ----------------------------------------------------------------------------
   procedure Debug_Message(Msg : String) is
      Clean_Msg : String := Msg & "                              ";
   begin
      -- Shift previous messages up
      for I in reverse 1..Max_Debug_Lines-1 loop
         Debug_History(I+1) := Debug_History(I);
      end loop;

      -- Add new message
      Debug_History(1) := Clean_Msg(1..30);
      Current_Debug := Natural'Min(Current_Debug + 1, Max_Debug_Lines);

      -- Update debug area
      for I in 1..Max_Debug_Lines loop
         LCD_Std_Out.Put(Status_Start_X, Debug_Start_Y + (I-1)*Debug_Line_Step, 
                         Debug_History(I));
      end loop;
   end Debug_Message;

end RC_Car_Display;
