------------------------------------------------------------------------------
-- main.adb
-- 
-- This is a driver program that sends a message to the UART.  The sample 
-- command turns the LED on and off.
------------------------------------------------------------------------------

with Coms_Uart;      use Coms_Uart;
with STM32.Board;    use STM32.Board;
with Ada.Real_Time;  use Ada.Real_Time;

procedure Main is
   Input_Buffer : String(1..80);
   Last_Char    : Natural;
begin
   Send_String_Newline ("UART Controller Ready");
   
   loop
      Send_String("CMD> ");
      Receive_Line(Output => Input_Buffer, 
                   Last   => Last_Char, 
                   Echo   => True);
      Process_Command(Input_Buffer(1..Last_Char));
   end loop;
end Main;
