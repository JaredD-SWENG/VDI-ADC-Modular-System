with HAL;
with STM32.Device;
with STM32.USARTs;
with STM32.GPIO;
with Interfaces;
with STM32_SVD.RCC;
with HAL.UART;
with Ada.Real_Time;
with System;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package Coms_Uart is

   task UART_Task;



   Exit_Flag : Boolean := False;



   ----------------------------------------------------------------------------
   -- Public subprograms (unchanged from your original)
   ----------------------------------------------------------------------------
   procedure Send_String         (Data : String);
   procedure Send_String_Newline (Data : String);
   procedure Send_Newline;
   procedure Receive_Line (Output : out String; Last : out Natural; Echo : Boolean := False);
   procedure Flush_RX;
   procedure Clear_Screen;
   procedure Newline;
   procedure Send_Time_Span (Span : Ada.Real_Time.Time_Span);
   procedure Handle_Drive_Command(Cmd : String);
   procedure Handle_Steering_Command(Cmd : String);

   -- Demo subprograms (unchanged from your original)
   procedure Process_Command (Command : String);
   procedure Output_Demo;
   procedure Input_Demo;
   procedure Initialize_Coms_Uart;
   procedure Request_Exit;
   function Exit_Requested return Boolean;

   ----------------------------------------------------------------------------
   -- Example: A library-level task that runs in parallel
   ----------------------------------------------------------------------------
   --  task UART_Task;

   ----------------------------------------------------------------------------
   -- Optionally, provide a way to stop the task from outside
   ----------------------------------------------------------------------------
   procedure Stop_UART_Task;
   function  Is_Exit_Requested return Boolean;

end Coms_Uart;
