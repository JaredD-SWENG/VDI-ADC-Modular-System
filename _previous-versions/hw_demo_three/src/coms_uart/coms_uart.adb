with HAL;           use HAL;
with STM32.Device;  use STM32.Device;
with STM32.USARTs;  use STM32.USARTs;
with STM32.GPIO;    use STM32.GPIO;
with Interfaces;    use Interfaces;
with STM32_SVD.RCC; use STM32_SVD.RCC;
with HAL.UART;      use HAL.UART;
with STM32.Board;   use STM32.Board;
with Ada.Strings, Ada.Strings.Fixed;

package body Coms_Uart is
   ----------------------------------------------------------------------------
   -- Define USART1 pins (TX on PA9 and RX on PA10)
   ----------------------------------------------------------------------------
   USART1_TX : constant GPIO_Point := PA9;
   USART1_RX : constant GPIO_Point := PA10;

   ----------------------------------------------------------------------------
   -- Configure_System_Clock_HSI_16MHz
   -- Enables the 16 MHz internal oscillator and selects it as the system clock
   -- with all prescalers set to 1.
   ----------------------------------------------------------------------------
   procedure Configure_System_Clock_HSI_16MHz is
   begin
      RCC_Periph.CR.HSION := True;
      loop
         exit when RCC_Periph.CR.HSIRDY;
      end loop;

      RCC_Periph.CFGR.SW := 0;  -- 0 means HSI is selected
      loop
         exit when RCC_Periph.CFGR.SWS = 0;
      end loop;

      RCC_Periph.CFGR.HPRE         := 0;
      RCC_Periph.CFGR.PPRE.Arr (1) := 0;
      RCC_Periph.CFGR.PPRE.Arr (2) := 0;
   end Configure_System_Clock_HSI_16MHz;

   ----------------------------------------------------------------------------
   -- Flush_RX
   -- Reads and discards any residual data from the RX buffer.
   ----------------------------------------------------------------------------
   procedure Flush_RX is
      Dummy : UInt9;
   begin
      while USART_1.Rx_Ready loop
         USART_1.Receive (Dummy);
      end loop;
   end Flush_RX;

   ----------------------------------------------------------------------------
   -- Send_String
   -- Converts an Ada String into a UART_Data_8b array and transmits it.
   ----------------------------------------------------------------------------
   procedure Send_String (Data : String) is
      Buffer : UART_Data_8b (Data'Range);
      Status : UART_Status;
   begin
      for I in Data'Range loop
         Buffer (I) := UInt8 (Character'Pos (Data (I)));
      end loop;
      USART_1.Transmit (Buffer, Status, Timeout => 1_000);
   end Send_String;

   ----------------------------------------------------------------------------
   -- Send_String_Newline
   -- Converts an Ada String into a UART_Data_8b array, appends a CR/LF, and transmits it.
   ----------------------------------------------------------------------------
   procedure Send_String_Newline (Data : String) is
      begin
         Send_String (Data & ASCII.CR & ASCII.LF);
      end Send_String_Newline;

   ----------------------------------------------------------------------------
   -- Send_Newline
   -- Sends a carriage return followed by a line feed.
   ----------------------------------------------------------------------------
   procedure Send_Newline is
   begin
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.CR)));
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.LF)));
   end Send_Newline;

   ----------------------------------------------------------------------------
   -- Receive_Line
   -- Reads characters into Output until a CR or LF is encountered (or the buffer is full).
   ----------------------------------------------------------------------------
   procedure Receive_Line
     (Output : out String; Last : out Natural; Echo : Boolean := False)
   is
      Char_Count : Natural := 0;
      Received   : UInt9;
   begin
      Last := 0;
      loop
         -- Wait until a byte is available.
         loop
            exit when USART_1.Rx_Ready;
         end loop;
         USART_1.Receive (Received);
         declare
            C : constant Character := Character'Val (Received);
         begin
            -- Only echo if not CR or LF.
            if Echo and then not (C = ASCII.CR or else C = ASCII.LF) then
               USART_1.Transmit (Received);
            end if;
            if C = ASCII.CR or else C = ASCII.LF then
               Flush_RX;
               Last := Char_Count;
               exit;
            else
               if Char_Count < Output'Length then
                  Char_Count          := Char_Count + 1;
                  Output (Char_Count) := C;
               else
                  Last := Char_Count;
                  exit;
               end if;
            end if;
         end;
      end loop;
   end Receive_Line;

   ----------------------------------------------------------------------------
   -- Clear_Screen
   -- Sends an escape sequence to clear the screen.
   ----------------------------------------------------------------------------
   procedure Clear_Screen is
   begin
      Send_String (ASCII.ESC & "[2J");
   end Clear_Screen;

   ----------------------------------------------------------------------------
   -- Newline
   -- Sends a carriage return and a line feed.
   ----------------------------------------------------------------------------
   procedure Newline is
   begin
      Send_String (ASCII.CR & ASCII.LF);
   end Newline;

   ----------------------------------------------------------------------------
   -- TO BE REMOVED IN FUTURE RELEASE -- FOR DEMO PURPOSES ONLY
   ----------------------------------------------------------------------------
   procedure Process_Command (Command : String) is
      use Ada.Strings, Ada.Strings.Fixed;
      CMD : constant String := Trim (Command, Both);
   begin
      if CMD = "LED ON" then
         Turn_On (Green_LED);
         Send_String_Newline (ASCII.CR & ASCII.LF & "Green LED Turned ON");
      elsif CMD = "LED OFF" then
         Turn_Off (Green_LED);
         Send_String_Newline (ASCII.CR & ASCII.LF & "Green LED Turned OFF");
      else
         Send_String_Newline
           (ASCII.CR & ASCII.LF & "ERR: Invalid command");
      end if;
   end Process_Command;

   ----------------------------------------------------------------------------
   -- TO BE REMOVED IN FUTURE RELEASE -- FOR DEMO PURPOSES ONLY
   ----------------------------------------------------------------------------
   procedure Output_Demo is
      Demo_Message : constant String := "Hello Ada from Penn State!";
   begin
      Clear_Screen;
      Send_String_Newline ("UART Controller Ready");
      Send_String_Newline (Demo_Message);
   end Output_Demo;

   ----------------------------------------------------------------------------
   -- TO BE REMOVED IN FUTURE RELEASE -- FOR DEMO PURPOSES ONLY
   ----------------------------------------------------------------------------
   procedure Input_Demo is
      Input_Buffer : String(1..80);
      Last_Char    : Natural;
   begin
      loop
         Send_String("CMD> ");
         Receive_Line(Output => Input_Buffer, 
                      Last   => Last_Char, 
                      Echo   => True);
         Process_Command(Input_Buffer(1..Last_Char));
      end loop;
   end Input_Demo;

-- package eloboration block
begin
   Configure_System_Clock_HSI_16MHz;
   Enable_Clock (GPIO_A);
   Enable_Clock (USART_1);

   Configure_IO
     (USART1_TX,
      (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating,
       AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));

   Configure_IO
     (USART1_RX,
     (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating,
       AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));

   USART_1.Set_Baud_Rate (115_200);
   USART_1.Set_Word_Length (Word_Length_8);
   USART_1.Set_Stop_Bits (Stopbits_1);
   USART_1.Set_Parity (No_Parity);
   USART_1.Set_Mode (Tx_Rx_Mode);
   USART_1.Set_Flow_Control (No_Flow_Control);
   USART_1.Set_Oversampling_Mode (Oversampling_By_16);

   USART_1.Enable;

   Initialize_LEDs;
   Flush_RX;

   Clear_Screen;
end Coms_Uart;


