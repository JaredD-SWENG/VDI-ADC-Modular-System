with HAL; use HAL;
with STM32.GPIO;
with STM32.PWM;
with STM32.Timers;
with STM32.Device;
with Steering_Motor;
with Ada.Text_IO; use Ada.Text_IO;

procedure Main is

   -- Single instance of the steering motor.
   My_Steering : Steering_Motor.Steering;
   My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_2;
   
begin
   -- Initialize the steering motor instance.
   Steering_Motor.Initialize
     (My_Steering, 
      Timer => My_Timer'Access);

   Steering_Motor.Enable (My_Steering);

   -- Set the steering motor to the center position.
   Steering_Motor.Set_Duty_Cycle_Us (My_Steering, 1500);

   loop
      Delay 2.0;
   end loop;
end Main;
