with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board;

package body Motor is
   Period  : constant Time_Span := Milliseconds (System_Config.Motor_Period);

   protected type Motor_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
   private
      Speed : Integer := 0;
   end Motor_Data_T;

   protected body Motor_Data_T is
      procedure Set_Speed (S : Integer) is
      begin
         Speed := S;
      end Set_Speed;

      function Get_Speed return Integer is
      begin
         return Speed;
      end Get_Speed;
   end Motor_Data_T;

   Motor1 : Motor_Data_T;

   function Get_Speed_Motor_1 return Integer is
   begin
      return Motor1.Get_Speed;
   end Get_Speed_Motor_1;

   procedure Set_Speed_Motor_1 (S : Integer) is
   begin
      Motor1.Set_Speed (S);
   end Set_Speed_Motor_1;


   task body Motor_Task is
      Next_Release  : Time := Clock;
   begin
      --STM32.Board.Initialize_LEDs;
      
      loop

         --STM32.Board.Turn_On (STM32.Board.Red_LED);
         
         --PWM_Control.Set (Get_Speed_Motor_1.Get_Speed);
         --  Next_Release := Next_Release + Period;
         --  delay until Next_Release;

         --STM32.Board.Turn_Off (STM32.Board.Red_LED);
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Motor_Task;

   procedure Init is
      Next_Release  : Time := Clock;
   begin
      null;
   end Init;

end Motor;