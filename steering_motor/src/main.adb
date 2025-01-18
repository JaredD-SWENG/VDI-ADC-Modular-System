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
   
begin
   -- Initialize the steering motor instance.
   Steering_Motor.Initialize(My_Steering);

   loop

   end loop;
end Main;
