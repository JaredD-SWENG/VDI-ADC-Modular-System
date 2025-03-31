with Uart; use Uart;
with Motor;
--with Steering;
with STM32.Board;

with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
   Period  : constant Time_Span := Milliseconds (10);
   Next_Release  : Time := Clock;
   C : Uart.Cmd;
   GoSpeed : Integer := 25;
   StopSpeed : Integer := 0;
begin
   STM32.Board.Initialize_LEDs;
   Motor.Init;
   loop
      C := Uart.Get_Command;
      case (C) is
         when center | left | right => 
            --  Steering.Set_Command(Cmd);
            null;
         when go =>
            STM32.Board.Turn_Off (STM32.Board.Red_LED);
            STM32.Board.Turn_On (STM32.Board.Green_LED);
            Motor.Set_Speed_Motor_1 (25);

         when stop =>
            STM32.Board.Turn_On (STM32.Board.Red_LED);
            STM32.Board.Turn_Off (STM32.Board.Green_LED);
            Motor.Set_Speed_Motor_1 (0);
     
         when undefined => 
            null;
      end case;
      
      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Main;