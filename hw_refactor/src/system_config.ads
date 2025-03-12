with System; use System;
package System_Config is
   Motor_Priority : constant Priority := Priority'First;
   Uart_Priority  : constant Priority := Motor_Priority + 1;
   Motor_Period : constant := 1000;
   Uart_Period  : constant := 100;
end System_Config;