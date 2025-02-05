with HAL; use HAL;
with HAL.UART;           use HAL.UART;
with STM32.Device;       use STM32.Device;
with STM32.USARTs;       use STM32.USARTs;
with STM32.GPIO;         use STM32.GPIO;
with Interfaces;         use Interfaces;
with STM32_SVD.RCC; use STM32_SVD.RCC;

procedure Main is

   procedure Configure_System_Clock_HSI_16MHz is
   begin
      -- 1) Enable HSI
      RCC_Periph.CR.HSION := True;
      loop
         exit when RCC_Periph.CR.HSIRDY;
      end loop;

      -- 2) Select HSI as System Clock
      --    SW = 0 in CFGR means “HSI selected as system clock.”
      RCC_Periph.CFGR.SW := 0;

      -- 3) Wait until HSI is really used as system clock
      loop
         exit when RCC_Periph.CFGR.SWS = 0;  -- SWS=0 means “HSI used as sysclk.”
      end loop;

      -- 4) Set all prescalers to DIV1
      --    HPRE = 0 => AHB prescaler = 1
      RCC_Periph.CFGR.HPRE := 0;

      -- PPRE1 => low part of CFGR.PPRE; PPRE2 => high part
      -- Typically done with these nested records/arrays:
      RCC_Periph.CFGR.PPRE.Arr (1) := 0;  -- APB1 = /1
      RCC_Periph.CFGR.PPRE.Arr (2) := 0;  -- APB2 = /1

      -- That is effectively the same as:
      --    RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
      --    RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV1;
      --    RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV1;
   end Configure_System_Clock_HSI_16MHz;

   -----------------------------
   -- 1) Transmit Buffer
   -----------------------------
   -- Build a “Hello VDI.\r\n” message.
   -- Depending on your HAL, you can do either:
   --    Tx_Buffer : constant UART_Data_8b := 
   --      (Character'Pos('H'), Character'Pos('e'), ...);
   --
   -- Or if your HAL.UART.Transmit allows a String, you can just do:
   --    Tx_String : constant String := "Hello VDI." & ASCII.LF & ASCII.CR;
   --
   Tx_Buffer : constant UART_Data_8b :=
     (Character'Pos('H'), Character'Pos('e'), Character'Pos('l'),
      Character'Pos('l'), Character'Pos('o'), Character'Pos(' '),
      Character'Pos('V'), Character'Pos('D'), Character'Pos('I'),
      Character'Pos('.'), Character'Pos(ASCII.CR), Character'Pos(ASCII.LF));

   -----------------------------
   -- 2) USART1 TX & RX Pins
   -----------------------------
   USART1_TX : constant GPIO_Point := PA9;   -- TX on PA9
   USART1_RX : constant GPIO_Point := PA10;  -- RX on PA10

begin
   -----------------------------
   -- (Optional) HAL Initialize
   -----------------------------
   -- If your run time does not do so automatically, you may need:
   -- HAL.Initialize;  -- This often sets up SysTick, etc.
   Configure_System_Clock_HSI_16MHz;

   -----------------------------
   -- 3) Enable Clocks
   -----------------------------
   Enable_Clock (GPIO_A);
   Enable_Clock (USART_1);

   -----------------------------
   -- 4) Configure GPIO for USART1
   -----------------------------
   Configure_IO
     (USART1_TX,
      (Mode           => Mode_AF,
       AF             => GPIO_AF_USART1_7,  -- AF7 for USART1 on STM32F4
       Resistors      => Floating,
       AF_Output_Type => Push_Pull,
       AF_Speed       => Speed_50MHz));

   Configure_IO
     (USART1_RX,
      (Mode           => Mode_AF,
       AF             => GPIO_AF_USART1_7,
       Resistors      => Floating,
       AF_Output_Type => Push_Pull,
       AF_Speed       => Speed_50MHz));

   -----------------------------
   -- 5) Configure USART1
   -----------------------------
   USART_1.Set_Baud_Rate         (115_200);
   USART_1.Set_Word_Length       (Word_Length_8);
   USART_1.Set_Stop_Bits         (Stopbits_1);
   USART_1.Set_Parity            (No_Parity);
   USART_1.Set_Mode              (Tx_Rx_Mode);
   USART_1.Set_Flow_Control      (No_Flow_Control);
   USART_1.Set_Oversampling_Mode (Oversampling_By_16);

   -- Finally enable USART1
   USART_1.Enable;

   -----------------------------
   -- 6) Send in a Loop
   -----------------------------
   while True loop
      declare
         Status : HAL.UART.UART_Status;
      begin
         USART_1.Transmit (Tx_Buffer, Status, Timeout => 1000);
         -- Optionally check the Status. If Status = Ok, all good.
         -- If needed, handle error states, etc.
      end;

      -- 1 second delay to match the C code's HAL_Delay(1000).
      delay 1.0;
   end loop;

end Main;