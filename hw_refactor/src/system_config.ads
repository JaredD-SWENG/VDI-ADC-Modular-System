with System; use System;
package System_Config is
   Motor_Priority : constant Priority := Priority'First;
   Steering_Priority : constant Priority := Motor_Priority + 1;
   Uart_Priority  : constant Priority := Steering_Priority + 1;
   Motor_Period : constant := 1000;
   Steering_Period : constant := 1000;
   Uart_Period  : constant := 100;
end System_Config;