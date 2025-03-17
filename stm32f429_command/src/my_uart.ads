with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;   use STM32.Board;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.USARTs;   use STM32.USARTs;
with HAL.UART;      use HAL.UART;
with HAL;           use HAL;
with Ada.Streams;  use Ada.Streams;

package My_UART is
   procedure Initialize;
   task Uart_Task;
end My_UART;
