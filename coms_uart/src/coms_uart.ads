with HAL;
with STM32.Device;
with STM32.USARTs;
with STM32.GPIO;
with Interfaces;
with STM32_SVD.RCC;
with HAL.UART;

package Coms_Uart is
   ----------------------------------------------------------------------------
   -- Initialize
   -- Sets up the system clock, GPIO pins, and configures USART1 for
   -- 115200 baud (8N1).
   ----------------------------------------------------------------------------
   procedure Coms_Initialize;

   ----------------------------------------------------------------------------
   -- Run
   -- Demo loop: sends a default transmit buffer (e.g., "Hello VDI.\r\n")
   -- using the array-based Transmit call with a 1-second delay.
   ----------------------------------------------------------------------------
   procedure Run;

   ----------------------------------------------------------------------------
   -- Send_String
   -- Converts an Ada String into a UART_Data_8b array and transmits it.
   ----------------------------------------------------------------------------
   procedure Send_String (Data : String);

   ----------------------------------------------------------------------------
   -- Send_String_Newline
   -- Converts an Ada String into a UART_Data_8b array, appends a CR/LF, and
   -- transmits it.
   ----------------------------------------------------------------------------
   procedure Send_String_Newline (Data : String);

   ----------------------------------------------------------------------------
   -- Send_Newline
   -- Sends a carriage return and a line feed.
   ----------------------------------------------------------------------------
   procedure Send_Newline;

   ----------------------------------------------------------------------------
   -- Receive_Line
   -- Reads characters into Output until a CR or LF is encountered (or the
   -- buffer is full). Optionally echoes each character (except CR/LF).
   ----------------------------------------------------------------------------
   procedure Receive_Line
     (Output :    out String;
      Last   :    out Natural;
      Echo   :    Boolean := False);

   ----------------------------------------------------------------------------
   -- Flush_RX
   -- Flushes any residual data from the RX buffer.
   ----------------------------------------------------------------------------
   procedure Flush_RX;

   ----------------------------------------------------------------------------
   -- Clear_Screen
   -- Sends an escape sequence to clear the screen.
   ----------------------------------------------------------------------------
   procedure Clear_Screen;

   ----------------------------------------------------------------------------
   -- Newline
   -- Sends a carriage return and a line feed.
   ----------------------------------------------------------------------------
   procedure Newline;

   ----------------------------------------------------------------------------
   -- TO BE REMOVED IN FUTURE RELEASE -- FOR DEMO PURPOSES ONLY
   -- Process_Command
   -- Processes a command string currently to control the green built-in LED.
   ----------------------------------------------------------------------------
   procedure Process_Command (Command : String);
end Coms_Uart;
