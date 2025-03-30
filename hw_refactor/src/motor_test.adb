with Ada.Real_Time; use Ada.Real_Time;
with Motor;         use Motor;
with Uarts;         use Uarts;
with System;        use System;
with STM32.Board;
with STM32.Device; use STM32.Device;

procedure Motor_Test is

   ------------------------------------------------------------------
   -- Helper routine to set speed and wait N seconds, printing info.
   ------------------------------------------------------------------
   procedure Set_And_Wait (Speed_Percent : Integer; Seconds : Duration) is
   begin
      Set_Speed_Motor_1 (Speed_Percent);
      Uarts.Put_Msg (STM32.Device.USART_1, "Set Speed to: ");
      Uarts.Put_Msg (STM32.Device.USART_1, Integer'Image (Speed_Percent));
      Uarts.Put_Msg (STM32.Device.USART_1, "%");
      Uarts.Put_Msg (STM32.Device.USART_1, ASCII.LF & ASCII.CR);
      delay Seconds;
   end Set_And_Wait;

begin

   Uarts.Initialize;

   Uarts.Put_Msg (STM32.Device.USART_1, "Initializing Motor..." & ASCII.LF & ASCII.CR);

   Motor.Init;

   Uarts.Put_Msg (STM32.Device.USART_1, "Motor Initialized." & ASCII.LF & ASCII.CR);

   loop
      Set_And_Wait (5,  1.0);
      Set_And_Wait (15, 1.0);
      Set_And_Wait (25, 1.0);
      Set_And_Wait (50, 1.0);
      Set_And_Wait (75, 1.0);
      Set_And_Wait (90, 1.0);

      Uarts.Put_Msg (STM32.Device.USART_1, "Looping..." & ASCII.LF & ASCII.CR);
   end loop;

end Motor_Test;
