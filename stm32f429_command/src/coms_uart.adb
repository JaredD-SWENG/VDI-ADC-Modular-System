with HAL; use HAL;
with STM32.Device; use STM32.Device;
with STM32.USARTs; use STM32.USARTs;
with STM32.GPIO; use STM32.GPIO;
with Interfaces; use Interfaces;
with STM32_SVD.RCC; use STM32_SVD.RCC;
with HAL.UART; use HAL.UART;
with STM32.Board; use STM32.Board;

with Ada.Strings;
with Ada.Strings.Fixed;

package body Coms_Uart is

   procedure Request_Exit is begin Exit_Flag := True; end;
   function Exit_Requested return Boolean is begin return Exit_Flag; end;

   USART1_TX : constant GPIO_Point := PA9;
   USART1_RX : constant GPIO_Point := PA10;

   procedure Configure_System_Clock_HSI_16MHz is
   begin
      RCC_Periph.CR.HSION := True;
      loop exit when RCC_Periph.CR.HSIRDY; end loop;
      RCC_Periph.CFGR.SW := 0;
      loop exit when RCC_Periph.CFGR.SWS = 0; end loop;
      RCC_Periph.CFGR.HPRE := 0;
      RCC_Periph.CFGR.PPRE.Arr (1) := 0;
      RCC_Periph.CFGR.PPRE.Arr (2) := 0;
   end Configure_System_Clock_HSI_16MHz;

   procedure Initialize_Coms_Uart is
   begin
      -- Configure_System_Clock_HSI_16MHz;
      Enable_Clock (GPIO_A);
      Enable_Clock (USART_1);

      Configure_IO(USART1_TX, (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating, AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));
      Configure_IO(USART1_RX, (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating, AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));

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
   end Initialize_Coms_Uart;

   procedure Send_String (Data : String) is
      Buffer : UART_Data_8b (Data'Range);
      Status : UART_Status;
   begin
      for I in Data'Range loop
         Buffer (I) := UInt8 (Character'Pos (Data (I)));
      end loop;
      USART_1.Transmit (Buffer, Status, Timeout => 1_000);
   end Send_String;

   procedure Send_String_Newline (Data : String) is
   begin
      Send_String (Data & ASCII.CR & ASCII.LF);
   end Send_String_Newline;

   procedure Send_Newline is
   begin
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.CR)));
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.LF)));
   end Send_Newline;

   procedure Receive_Char (Output : out Character; Echo : Boolean := False) is
      Received : UInt9;
   begin
      loop exit when USART_1.Rx_Ready; end loop;
      USART_1.Receive (Received);
      Output := Character'Val (Received);
      if Echo and then not (Output in ASCII.LF | ASCII.CR) then
         USART_1.Transmit (Received);
      end if;
   end Receive_Char;

   procedure Flush_RX is
      Dummy : UInt9;
   begin
      while USART_1.Rx_Ready loop
         USART_1.Receive (Dummy);
      end loop;
   end Flush_RX;

   task body UART_Task is
      Input_Char : Character;
   begin
      Send_String_Newline ("[UART_Task] Started concurrency.");
      loop
         exit when Exit_Flag;
         Receive_Char (Output => Input_Char, Echo => True);
      end loop;
      Send_String_Newline ("[UART_Task] Exiting concurrency.");
   end UART_Task;

end Coms_Uart;
