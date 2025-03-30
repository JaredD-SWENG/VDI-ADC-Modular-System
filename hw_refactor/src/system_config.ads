with System; use System;
with STM32; use STM32;
with STM32.Timers; use STM32.Timers;
with STM32.GPIO; use STM32.GPIO;
with STM32.PWM; use STM32.PWM;
with STM32.Device; use STM32.Device;

package System_Config is
   Motor_Priority : constant Priority := Priority'First;
   Steering_Priority : constant Priority := Motor_Priority + 1;
   Uart_Priority  : constant Priority := Steering_Priority + 1;

   --  Motor_Priority : constant Priority := 5;
   --  Steering_Priority : constant Priority := 5;
   --  Uart_Priority  : constant Priority := 4; 

   Motor_Period : constant := 1000;
   Steering_Period : constant := 1000;
   Uart_Period  : constant := 10;

end System_Config;