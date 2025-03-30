with Ada.Text_IO;
with Coms_Uart;
with Drive_Motor;
with Steering_Motor;
with RC_Car_Display;
with Ada.Strings.Fixed;

procedure Main is
   Motor : Drive_Motor.Motor;
   Steering : Steering_Motor.Steering;
   Motor_Calibrated : Boolean := False;

   procedure Initialize_Components is
   begin
      Drive_Motor.Initialize(Motor);
      Steering_Motor.Initialize(Steering);
      RC_Car_Display.Initialize;
      Coms_Uart.Output_Demo;
      Delay 0.5;
   end Initialize_Components;

   procedure Display_Menu is
   begin
      Coms_Uart.Clear_Screen;
      Coms_Uart.Flush_RX;
      Coms_Uart.Send_String_Newline(ASCII.CR & "RC Car UART Control Menu:");
      Coms_Uart.Send_String_Newline("1. Calibrate Motor");
      Coms_Uart.Send_String_Newline("2. Set Motor Speed");
      Coms_Uart.Send_String_Newline("3. Set Steering Angle");
      Coms_Uart.Send_String_Newline("4. Stop Motor");
      Coms_Uart.Send_String_Newline("5. Center Steering");
      Coms_Uart.Send_String_Newline("6. Display Status");
      Coms_Uart.Send_String_Newline("7. Exit");
      Coms_Uart.Send_String("Enter your choice: ");
   end Display_Menu;

   procedure Process_Menu_Choice(Choice : String) is
      Speed : Integer;
      Angle : Integer;
      Input : String(1..10);
      Last : Natural;
   begin

      if Choice = "1" then
         Coms_Uart.Send_String_Newline("Calibrating motor...");
         RC_Car_Display.Debug_Message("Calibrating motor...");
         Drive_Motor.Enable(Motor);
         Motor_Calibrated := True;
         RC_Car_Display.Debug_Message("Motor calibrated");
      elsif Choice = "2" then
         if not Motor_Calibrated then
            Coms_Uart.Send_String_Newline("Please calibrate the motor first (Option 1)");
         else
            Coms_Uart.Send_String("Enter motor speed (0-100%): ");
            Coms_Uart.Receive_Line(Input, Last, True);
            Speed := Integer'Value(Input(1..Last));
            Drive_Motor.Set_Speed(Motor, Speed);
            RC_Car_Display.Update_Motor(Speed);
            RC_Car_Display.Debug_Message("Motor speed set to " & Speed'Image & "%");
         end if;
      elsif Choice = "3" then
         Coms_Uart.Send_String("Enter steering angle (-30 to 30 degrees): ");
         Coms_Uart.Receive_Line(Input, Last, True);
         Angle := Integer'Value(Input(1..Last));
         Steering_Motor.Enable (Steering);
         Steering_Motor.Set_Angle(Steering, Angle);
         RC_Car_Display.Update_Steering(Angle);
         RC_Car_Display.Debug_Message("Steering angle set to " & Angle'Image & " degrees");
      elsif Choice = "4" then
         Drive_Motor.Stop(Motor);
         RC_Car_Display.Update_Motor(0);
         RC_Car_Display.Debug_Message("Motor stopped");
      elsif Choice = "5" then
         Steering_Motor.Center(Steering);
         RC_Car_Display.Update_Steering(0);
         RC_Car_Display.Debug_Message("Steering centered");
      elsif Choice = "6" then
         RC_Car_Display.Update_Battery(12.5); -- Example battery voltage
         RC_Car_Display.Debug_Message("Status updated on display");
      elsif Choice = "7" then
         Coms_Uart.Send_String_Newline("Exiting...");
         return;
      else
         Coms_Uart.Send_String_Newline("Invalid choice. Please try again.");
      end if;
   end Process_Menu_Choice;

   Choice : String(1..1);
   Last : Natural;
begin
   Initialize_Components;

   loop
      Display_Menu;
      Coms_Uart.Receive_Line(Choice, Last, True);
      Coms_Uart.Send_Newline;

      exit when Choice(1) = '7';

      Process_Menu_Choice(Choice(1..1));
      Coms_Uart.Send_Newline;
   end loop;

   -- Clean up
   Drive_Motor.Stop(Motor);
   Steering_Motor.Center(Steering);
   RC_Car_Display.Debug_Message("RC Car shutdown complete");
end Main;
