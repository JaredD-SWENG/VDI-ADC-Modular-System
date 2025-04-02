with Ada.Real_Time; use Ada.Real_Time;
with HAL;           use HAL;
with UARTS;         use UARTS;
with STM32.Device;  use STM32.Device;
with System_Config;
--  with STM32.Board;   use STM32.Board;

package body Uart is

   protected type Uart_Data_T is
      procedure Set_Command (C : Cmd);
      function Get_Command return Cmd;
   private
      Command : Cmd := undefined;
   end Uart_Data_T;

   protected body Uart_Data_T is
      procedure Set_Command (C : Cmd) is
      begin
         Command := C;
      end Set_Command;

      function Get_Command return Cmd is (Command);
   end Uart_Data_T;

   Uart_Data : Uart_Data_T;

   function Get_Command return Cmd is (Uart_Data.Get_Command);

   procedure Read_Uart is
      Rcv_Data    : HAL.UInt16;
   begin
      Uarts.Get_Blocking (STM32.Device.USART_1, Data => Rcv_Data);
      Uart_Data.Set_Command(Cmd'Enum_Val(Rcv_Data));
   end Read_Uart;

   task body Uart_Task is
   begin
      loop
         Read_Uart;
      end loop;
   end Uart_Task;
begin
   Uarts.Initialize;
end Uart;