with System; use System;
with STM32; use STM32;
with STM32.Timers; use STM32.Timers;
with STM32.GPIO; use STM32.GPIO;
with STM32.PWM; use STM32.PWM;
with STM32.Device; use STM32.Device;

package System_Config is
   Uart_Priority  : constant Priority := Priority'First;
   Motor_Priority : constant Priority := Uart_Priority + 1;
   Steering_Priority : constant Priority := Motor_Priority + 1;

   Motor_Period : constant := 400;
   Steering_Period : constant := 400;
   Uart_Period  : constant := 200;

   -- Drive Motor/ESC Parameters
   Drive_Max_Speed : constant Integer := 20; -- Max speed in % range
   Drive_Min_Speed : constant Integer := 0; -- Min speed % range
   Drive_Frequency : constant STM32.PWM.Hertz := 50; -- 50 Hz for ESC
   Drive_Max_Duty_Cycle : constant STM32.PWM.Percentage := 10;
   Drive_Min_Duty_Cycle : constant STM32.PWM.Percentage := 5;
   ESC30A_Cal_Time : constant Standard.Duration := 2.0; -- seconds

   

end System_Config;