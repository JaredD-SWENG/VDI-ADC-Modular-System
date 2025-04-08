with System_Config;
with Uart; use Uart;
with Motor;
with Steering;
with STM32.Board;

with Ada.Real_Time; use Ada.Real_Time;

procedure Main is
   Period  : constant Time_Span := Milliseconds (10);
   Next_Release  : Time := Clock;
   C : Uart.Cmd;
begin
   STM32.Board.Initialize_LEDs;
   loop
      C := Uart.Get_Command;
      case (C) is
         when center => 
            Steering.Set_Angle_Steering1(0);
            null;
         when left =>
            Steering.Set_Angle_Steering1(-60);
            null;
         when right => 
            Steering.Set_Angle_Steering1(60);
            null;
         when go =>
            STM32.Board.Turn_Off (STM32.Board.Red_LED);
            STM32.Board.Turn_On (STM32.Board.Green_LED);
            Motor.Set_Speed_Drive (System_Config.Drive_Max_Speed);
         when stop =>
            STM32.Board.Turn_On (STM32.Board.Red_LED);
            STM32.Board.Turn_Off (STM32.Board.Green_LED);
            Motor.Set_Speed_Drive (System_Config.Drive_Min_Speed);
         when undefined => 
            null;
      end case;

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Main;