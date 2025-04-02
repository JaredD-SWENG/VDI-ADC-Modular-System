with System; use System;
with STM32; use STM32;
with STM32.Timers; use STM32.Timers;
with STM32.GPIO; use STM32.GPIO;
with STM32.PWM; use STM32.PWM;
with STM32.Device; use STM32.Device;

package System_Config is
   Motor_Priority : constant Priority := Priority'First + 2;
   Steering_Priority : constant Priority := Priority'First + 1;
   Uart_Priority  : constant Priority := Priority'First;

   Motor_Period : constant := 100;
   Steering_Period : constant := 100;
   Uart_Period  : constant := 200;

end System_Config;