-----------------------------------------------------------------------
--  uart_test_driver.adb
--
--  A simple driver program to demonstrate using the Coms_Uart package.
--  It sends a greeting message, prompts the user for input, and echoes
--  back what was typed, all via UART.
-----------------------------------------------------------------------
with Coms_Uart;
with Ada.Text_IO; use Ada.Text_IO;

procedure Uart_Test_Driver is
   -- Buffer for incoming data
   Input_Buffer : String(1 .. 80);
   Last_Char    : Natural;
begin
   -- Let the console user know we've started
   Coms_Uart.Send_String_Newline("UART Test Driver: Starting up...");

   -- Send out a greeting through UART
   Coms_Uart.Send_String_Newline("Hello from uart_test_driver!");

   -- Prompt the user for input via UART
   Coms_Uart.Send_String("Type something and press Enter: ");

   -- Read user input from UART
   Coms_Uart.Receive_Line(Output => Input_Buffer, Last => Last_Char, Echo => True);

   -- Echo the typed text
   if Last_Char > 0 then
      Coms_Uart.Send_String_Newline("You typed: " & Input_Buffer(1 .. Last_Char));
   else
      Coms_Uart.Send_String_Newline("No input received!");
   end if;

   Coms_Uart.Send_String_Newline("UART Test Driver: Finished.");
end Uart_Test_Driver;
