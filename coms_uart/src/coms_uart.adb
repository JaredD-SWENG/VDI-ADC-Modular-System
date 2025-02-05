with HAL;                 use HAL;
with STM32.Device;       use STM32.Device;
with STM32.USARTs;       use STM32.USARTs;
with STM32.GPIO;         use STM32.GPIO;
with Interfaces;         use Interfaces;
with STM32_SVD.RCC;      use STM32_SVD.RCC;
with HAL.UART;           use HAL.UART;

package body Coms_Uart is

   ----------------------------------------------------------------------------
   -- Demo Transmit Buffer: "Hello VDI.\r\n"
   -- (This buffer is of type UART_Data_8b.)
   ----------------------------------------------------------------------------
   Tx_Buffer : constant UART_Data_8b :=
     (Character'Pos('H'), Character'Pos('e'), Character'Pos('l'),
      Character'Pos('l'), Character'Pos('o'), Character'Pos(' '),
      Character'Pos('V'), Character'Pos('D'), Character'Pos('I'),
      Character'Pos('.'), Character'Pos(ASCII.CR), Character'Pos(ASCII.LF));

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
   -- Initialize
   ----------------------------------------------------------------------------
   procedure Initialize is
   begin
      -- Configure system clock to 16 MHz HSI.
      Configure_System_Clock_HSI_16MHz;

      -- Enable clocks for GPIOA and USART1.
      Enable_Clock (GPIO_A);
      Enable_Clock (USART_1);

      -- Configure TX and RX pins.
      Configure_IO (USART1_TX,
                    (Mode           => Mode_AF,
                     AF             => GPIO_AF_USART1_7,
                     Resistors      => Floating,
                     AF_Output_Type => Push_Pull,
                     AF_Speed       => Speed_50MHz));

      Configure_IO (USART1_RX,
                    (Mode           => Mode_AF,
                     AF             => GPIO_AF_USART1_7,
                     Resistors      => Floating,
                     AF_Output_Type => Push_Pull,
                     AF_Speed       => Speed_50MHz));

      -- Configure USART1: 115200 baud, 8 data bits, no parity, 1 stop bit.
      USART_1.Set_Baud_Rate         (115_200);
      USART_1.Set_Word_Length       (Word_Length_8);
      USART_1.Set_Stop_Bits         (Stopbits_1);
      USART_1.Set_Parity            (No_Parity);
      USART_1.Set_Mode              (Tx_Rx_Mode);
      USART_1.Set_Flow_Control      (No_Flow_Control);
      USART_1.Set_Oversampling_Mode (Oversampling_By_16);

      -- Enable USART1.
      USART_1.Enable;
   end Initialize;

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
   -- Run
   -- Demo loop: sends Tx_Buffer (i.e. "Hello VDI.\r\n") using the array-based
   -- Transmit method (with a Timeout parameter), then delays 1 second.
   ----------------------------------------------------------------------------
   procedure Run is
      Status : UART_Status;
   begin
      while True loop
         -- The explicit type conversion forces Tx_Buffer to be of type UART_Data_8b.
         USART_1.Transmit (UART_Data_8b'(Tx_Buffer), Status, Timeout => 1000);
         delay 1.0;
      end loop;
   end Run;

   ----------------------------------------------------------------------------
   -- Send_String
   -- Converts an Ada String into a UART_Data_8b array and transmits it.
   ----------------------------------------------------------------------------
   procedure Send_String (Data : String) is
      Buffer : UART_Data_8b (Data'Range);
      Status : UART_Status;
   begin
      for I in Data'Range loop
         Buffer(I) := UInt8 (Character'Pos(Data(I)));
      end loop;
      USART_1.Transmit (Buffer, Status, Timeout => 1000);
   end Send_String;

   ----------------------------------------------------------------------------
   -- Send_Newline
   -- Sends a carriage return followed by a line feed.
   ----------------------------------------------------------------------------
   procedure Send_Newline is
   begin
      USART_1.Transmit (UInt9 (Character'Pos(ASCII.CR)));
      USART_1.Transmit (UInt9 (Character'Pos(ASCII.LF)));
   end Send_Newline;

   ----------------------------------------------------------------------------
   -- Receive_Line
   -- Reads characters into Output until a CR or LF is encountered (or the buffer is full).
   ----------------------------------------------------------------------------
   procedure Receive_Line
     (Output :    out String;
      Last   :    out Natural;
      Echo   :    Boolean := False)
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
            C : constant Character := Character'Val(Received);
         begin
            if Echo then
               USART_1.Transmit (Received);
            end if;
            if C = ASCII.CR or else C = ASCII.LF then
               Last := Char_Count;
               exit;
            else
               if Char_Count < Output'Length then
                  Char_Count := Char_Count + 1;
                  Output(Char_Count) := C;
               else
                  Last := Char_Count;
                  exit;
               end if;
            end if;
         end;
      end loop;
   end Receive_Line;

end Coms_Uart;
