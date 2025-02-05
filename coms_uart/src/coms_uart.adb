-- Test: https://psu.mediaspace.kaltura.com/media/2025-02-04+21-42-02_loopback-test/1_djezbmty

with HAL;                 use HAL;
with STM32.Device;       use STM32.Device;
with STM32.USARTs;       use STM32.USARTs;
with STM32.GPIO;         use STM32.GPIO;
with Interfaces;         use Interfaces;
with STM32_SVD.RCC;      use STM32_SVD.RCC;

procedure Coms_Uart is

   -----------------------------
   -- 1) Transmit Buffer
   -----------------------------
   Tx_Buffer : constant array (Positive range 1 .. 12) of UInt8 :=
     (Character'Pos('H'), Character'Pos('e'), Character'Pos('l'),
      Character'Pos('l'), Character'Pos('o'), Character'Pos(' '),
      Character'Pos('V'), Character'Pos('D'), Character'Pos('I'),
      Character'Pos('.'), Character'Pos(ASCII.CR), Character'Pos(ASCII.LF));

   -----------------------------
   -- 2) USART1 TX & RX Pins
   -----------------------------
   USART1_TX : constant GPIO_Point := PA9;   -- TX on PA9
   USART1_RX : constant GPIO_Point := PA10;  -- RX on PA10

   -----------------------------
   -- 3 System Clock Configuration
   -----------------------------
   procedure Configure_System_Clock_HSI_16MHz is
   begin
      -- Enable the 16 MHz internal oscillator (HSI)
      RCC_Periph.CR.HSION := True;
      loop
         exit when RCC_Periph.CR.HSIRDY;
      end loop;
      
      -- Select HSI as the system clock (SW = 0 means HSI)
      RCC_Periph.CFGR.SW := 0;
      loop
         exit when RCC_Periph.CFGR.SWS = 0;
      end loop;
      
      -- Set prescalers to 1 (HPRE, PPRE1, PPRE2)
      RCC_Periph.CFGR.HPRE         := 0;
      RCC_Periph.CFGR.PPRE.Arr (1) := 0;
      RCC_Periph.CFGR.PPRE.Arr (2) := 0;
   end Configure_System_Clock_HSI_16MHz;

begin
   -- Configure the system clock as in your C code.
   Configure_System_Clock_HSI_16MHz;

   -----------------------------
   -- 4) Enable Clocks and Configure GPIO for USART1
   -----------------------------
   Enable_Clock (GPIO_A);
   Enable_Clock (USART_1);

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

   -----------------------------
   -- 5) Configure USART1 for TX/RX
   -----------------------------
   USART_1.Set_Baud_Rate         (115_200);
   USART_1.Set_Word_Length       (Word_Length_8);
   USART_1.Set_Stop_Bits         (Stopbits_1);
   USART_1.Set_Parity            (No_Parity);
   USART_1.Set_Mode              (Tx_Rx_Mode);
   USART_1.Set_Flow_Control      (No_Flow_Control);
   USART_1.Set_Oversampling_Mode (Oversampling_By_16);
   USART_1.Enable;

   -----------------------------
   -- 6) Main Loop: Transmit and Receive
   -----------------------------
   while True loop
      -- Transmit the message one byte at a time.
      --  for I in Tx_Buffer'Range loop
      --     -- Convert each UInt8 to UInt9 and transmit.
      --     USART_1.Transmit (UInt9 (Tx_Buffer(I)));
      --  end loop;

      -- Check if a byte has been received.
      if USART_1.Rx_Ready then
         declare
            Received : UInt9;
         begin
            USART_1.Receive (Received);
            -- Echo the received byte back.
            USART_1.Transmit (Received);
         end;
      end if;
   end loop;
end Coms_Uart;
