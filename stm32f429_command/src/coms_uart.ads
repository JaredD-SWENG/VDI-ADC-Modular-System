with HAL; use HAL;
with STM32.Device;
with STM32.USARTs;
with STM32.GPIO;
with Interfaces; use Interfaces;
with STM32_SVD.RCC;
with HAL.UART;
with Ada.Real_Time;
with System;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Config;

package Coms_Uart is
   task UART_Task with Priority => Config.UART_Task_Priority;

   Exit_Flag : Boolean := False;

   ----------------------------------------------------------------------------
   -- Public subprograms
   ----------------------------------------------------------------------------
   procedure Initialize_Coms_Uart;
   procedure Send_String         (Data : String);
   procedure Send_String_Newline (Data : String);
   procedure Send_Newline;
   procedure Flush_RX;
   procedure Receive_Char (Output : out Character; Echo : Boolean := False);
   -- procedure Clear_Screen;


   -- Stop UART task
   procedure Request_Exit;
   function Exit_Requested return Boolean;


end Coms_Uart;
