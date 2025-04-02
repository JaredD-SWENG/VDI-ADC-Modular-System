with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;

package body Uarts is

   TX_Pin : constant GPIO_Point := PA9;
   RX_Pin : constant GPIO_Point := PA10;

   --------------------------
   -- Initialize_UART_GPIO --
   --------------------------

   procedure Initialize_UART_GPIO is
   begin
      Enable_Clock (USART_1);
      Enable_Clock (RX_Pin & TX_Pin);

      Configure_IO
        (RX_Pin & TX_Pin,
         (Mode           => Mode_AF,
          AF             => GPIO_AF_USART1_7,
          Resistors      => Pull_Up,
          AF_Speed       => Speed_50MHz,
          AF_Output_Type => Push_Pull));
   end Initialize_UART_GPIO;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Initialize_UART_GPIO;

      Disable (USART_1);

      Set_Baud_Rate    (USART_1, 115_200);
      Set_Mode         (USART_1, Tx_Rx_Mode);
      Set_Stop_Bits    (USART_1, Stopbits_1);
      Set_Word_Length  (USART_1, Word_Length_8);
      Set_Parity       (USART_1, No_Parity);
      Set_Flow_Control (USART_1, No_Flow_Control);

      Enable (USART_1);
   end Initialize;

   ----------------------
   -- Await_Send_Ready --
   ----------------------

   procedure Await_Send_Ready (This : USART) is
   begin
      loop
         exit when Tx_Ready (This);
      end loop;
   end Await_Send_Ready;

   --------------------------
   -- Await_Data_Available --
   --------------------------

   procedure Await_Data_Available (This : USART) is
   begin
      loop
         exit when Rx_Ready (This);
      end loop;
   end Await_Data_Available;

   ------------------
   -- Put_Blocking --
   ------------------

   procedure Put_Blocking (This : in out USART;  Data : UInt16) is
   begin
      Await_Send_Ready (This);
      Transmit (This, UInt9 (Data));
   end Put_Blocking;

   ------------------
   -- Get_Blocking --
   ------------------

   procedure Get_Blocking (This : in out USART;  Data : out UInt16) is
   begin
      Await_Data_Available (This);
      Receive (This, UInt9 (Data));
   end Get_Blocking;

   -------------
   -- Put_Msg --
   -------------

   procedure Put_Msg (This : in out USART;  Msg : String) is
   begin
      for K in Msg'Range loop
         Await_Send_Ready (This);
         Transmit (This, Character'Pos (Msg (K)));
      end loop;
   end Put_Msg;


   -------------
   -- Get_Msg --
   -------------

   procedure Get_Msg (This : in out USART;  Msg : out String) is
      Received_Char : Character;
      Raw           : UInt9;
   begin
      Msg := (others => ASCII.NUL); --  Clear the string
      Receiving : for K in 1 .. Msg_Size loop
         Await_Data_Available (This);
         Receive (This, Raw);
         Received_Char := Character'Val (Raw);
         Msg (K) := Received_Char;

         exit Receiving when Received_Char = ASCII.NUL;
      end loop Receiving;
   end Get_Msg;

end Uarts;