with Ada.Real_Time; use Ada.Real_Time;
with HAL;           use HAL;
--  with HAL.UART; use HAL.UART;
with STM32.USARTs; use STM32.USARTs;
with STM32.GPIO;
with STM32.GPIO;
with STM32.Device; use STM32.Device;
with STM32.Board; use STM32.Board;

package body Uart is

   protected type Uart_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
      procedure Set_Emergency_Stop (S : Boolean);
      function Get_Emergency_Stop return Boolean;
   private
      Speed          : Integer := 0;
      Emergency_Stop : Boolean := False;
   end Uart_Data_T;

   protected body Uart_Data_T is
      procedure Set_Speed (S : Integer) is
      begin
         Speed := S;
      end Set_Speed;

      function Get_Speed return Integer is (Speed);

      procedure Set_Emergency_Stop (S : Boolean) is
      begin
         Emergency_Stop := S;
      end Set_Emergency_Stop;

      function Get_Emergency_Stop return Boolean is (Emergency_Stop);
   end Uart_Data_T;

   Uart1 : Uart_Data_T;

   function Get_Speed_Cmd return Integer is (Uart1.Get_Speed);

   function Emergency_Stop return Boolean is (Uart1.Get_Emergency_Stop);

   type Cmd_T is (Set_Angle, Emergency_Stop, Set_Speed, Undefined);
   for Cmd_T use
     (Set_Angle => Character'Pos ('A'), Emergency_Stop => Character'Pos ('E'),
      Set_Speed => Character'Pos ('S'), Undefined => Character'Pos ('U'));

   type Command (Cmd : Cmd_T) is record
      case Cmd is
         when Set_Angle =>
            Angle : Integer;
         when Set_Speed =>
            Speed : Integer;
         when Emergency_Stop =>
            State : Boolean;
         when Undefined =>
            null;
      end case;
   end record;

   function Read_Uart return Command is
      Received : UInt9;
      C        : Character with
        Address => Received'Address;
      Msg      : String (1 .. 120);
      Pos      : Natural := Msg'First;
   begin
      loop
         USART_1.Receive (Received);
         USART_1.Transmit (Received);
         exit when C = ASCII.NUL;
         Msg (Pos) := C;
         Pos       := Pos + 1;
      end loop;
      if Msg (1) = 'E' then
         return (Cmd => Emergency_Stop, State => True);
      elsif Msg (1) = 'S' then
         return
           (Cmd => Set_Speed, Speed => Integer'Value (Msg (2 .. Msg'Last)));
      else
         return (Cmd => Undefined);
      end if;
   end Read_Uart;

   procedure Init is
      USART1_TX : constant STM32.GPIO.GPIO_Point := PA9;
      USART1_RX : constant STM32.GPIO.GPIO_Point := PA10;
   begin
      STM32.Device.Enable_Clock (STM32.Device.GPIO_A);
      STM32.Device.Enable_Clock (STM32.Device.USART_1);

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

      USART_1.Set_Baud_Rate (115_200);
      USART_1.Enable;

      --  USART_1.Set_Word_Length (Word_Length_8);
      --  USART_1.Set_Stop_Bits (Stopbits_1);
      --  USART_1.Set_Parity (No_Parity);
      --  USART_1.Set_Mode (Tx_Rx_Mode);
      --  USART_1.Set_Flow_Control (No_Flow_Control);
      --  USART_1.Set_Oversampling_Mode (Oversampling_By_16);
      --  USART_1.Enable;
      --  HAL.Init (USART_1);
      --  Configure (USART_1, Baud_Rate => 115_200);
      USART_1.Transmit (UInt9 (Character'Pos ('H')));
      USART_1.Transmit (UInt9 (Character'Pos ('E')));
      USART_1.Transmit (UInt9 (Character'Pos ('L')));
      USART_1.Transmit (UInt9 (Character'Pos ('L')));
      USART_1.Transmit (UInt9 (Character'Pos ('O')));
      USART_1.Transmit (UInt9 (Character'Pos (' ')));
      USART_1.Transmit (UInt9 (Character'Pos ('W')));
      USART_1.Transmit (UInt9 (Character'Pos ('O')));    
      USART_1.Transmit (UInt9 (Character'Pos ('R')));
      USART_1.Transmit (UInt9 (Character'Pos ('L')));
      USART_1.Transmit (UInt9 (Character'Pos ('D')));
      USART_1.Transmit (UInt9 (Character'Pos ('!')));
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.CR)));
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.LF)));
   end Init;

   task body Uart_Task is
      Next_Release : Time := Clock;
   begin
      Init;
      loop
         --  declare
         --     Cmd : Command := Read_Uart;
         --  begin
         --     if Cmd.Cmd = Set_Speed then
         --        Uart1.Set_Speed (Cmd.Speed);
         --     elsif Cmd.Cmd = Emergency_Stop then
         --        Uart1.Set_Emergency_Stop (Cmd.State);
         --     end if;
         --  end;
         STM32.Board.Turn_On (Green_LED);

         Next_Release := Next_Release + Milliseconds (100);
         delay until Next_Release;


         STM32.Board.Turn_Off (Green_LED);
         Next_Release := Next_Release + Milliseconds (100);
         delay until Next_Release;


      end loop;
   end Uart_Task;

end Uart;
