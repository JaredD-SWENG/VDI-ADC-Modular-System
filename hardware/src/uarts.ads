with HAL;           use HAL;
with STM32.USARTs;  use STM32.USARTs;

package Uarts is
   Msg_Size: constant Integer := 20;
   procedure Initialize_UART_GPIO;
   procedure Initialize;
   procedure Await_Send_Ready (This : USART) with Inline;
   procedure Await_Data_Available (This : USART) with Inline;
   procedure Put_Blocking (This : in out USART;  Data : UInt16);
   procedure Get_Blocking (This : in out USART;  Data : out UInt16);
   procedure Put_Msg (This : in out USART;  Msg : String);
   procedure Get_Msg (This : in out USART;  Msg : out String);
end Uarts;