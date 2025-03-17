package body My_UART is

   protected UART_Status is
      entry Wait_For_Transmit;
      entry Wait_For_Receive;
      procedure Signal_Tx_Complete;
      procedure Signal_Rx_Complete;
      private
         Tx_Complete : Boolean := False;
         Rx_Complete : Boolean := False;
   end UART_Status;

   protected body UART_Status is
      entry Wait_For_Transmit when Tx_Complete is
      begin
         Tx_Complete := False;
      end Wait_For_Transmit;

      entry Wait_For_Receive when Rx_Complete is
      begin
         Rx_Complete := False;
      end Wait_For_Receive;

      procedure Signal_Tx_Complete is
      begin
         Tx_Complete := True;
      end Signal_Tx_Complete;

      procedure Signal_Rx_Complete is
      begin
         Rx_Complete := True;
      end Signal_Rx_Complete;
   end UART_Status;


   procedure Initialize is
      USART1_TX : constant STM32.GPIO.GPIO_Point := PA9;
      USART1_RX : constant STM32.GPIO.GPIO_Point := PA10;
   begin
      STM32.Device.Enable_Clock(STM32.Device.USART_1); -- Enable the clock for UART1

      STM32.Device.USART_1.Set_Baud_Rate (115_200);
      STM32.Device.USART_1.Set_Word_Length (Word_Length_8);
      STM32.Device.USART_1.Set_Stop_Bits (Stopbits_1);
      STM32.Device.USART_1.Set_Parity (No_Parity);
      STM32.Device.USART_1.Set_Mode (Tx_Rx_Mode);
      STM32.Device.USART_1.Set_Flow_Control (No_Flow_Control);
      STM32.Device.USART_1.Set_Oversampling_Mode (Oversampling_By_16);

      STM32.Device.USART_1.Enable_DMA_Transmit_Requests; -- Enable DMA for transmit requests
      
      STM32.Device.Enable_Clock (STM32.Device.GPIO_A);
      STM32.Device.Enable_Clock (STM32.Device.USART_1);
      STM32.Device.Enable_Clock (STM32.Device.DMA_1);

      STM32.GPIO.Configure_IO
        (USART1_TX,
         (Mode           => STM32.GPIO.Mode_AF, AF => GPIO_AF_USART1_7,
          Resistors      => STM32.GPIO.Floating,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_50MHz));
      STM32.GPIO.Configure_IO
        (USART1_RX,
        (Mode            => STM32.GPIO.Mode_AF, AF => GPIO_AF_USART1_7,
          Resistors      => STM32.GPIO.Floating,
          AF_Output_Type => STM32.GPIO.Push_Pull,
          AF_Speed       => STM32.GPIO.Speed_50MHz));

      STM32.Device.USART_1.Enable;
   end Initialize;

   task body Uart_Task is

   Next_Release : Ada.Real_Time.Time := Ada.Real_Time.Clock;
   Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(100);

   begin
      Initialize;
      loop

         Next_Release := Next_Release + Period;

         USART_1.Transmit(UInt9(Character'Pos('H')));

         delay until Next_Release;
      end loop;
      
   end Uart_Task;
end My_UART;
