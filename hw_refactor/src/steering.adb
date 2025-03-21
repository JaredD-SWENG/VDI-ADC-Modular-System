with Ada.Real_Time; use Ada.Real_Time;
with System_Config;
with STM32.Device;  use STM32.Device;
with STM32.GPIO;    use STM32.GPIO;
with STM32.Timers;  use STM32.Timers;
with STM32.Board;

package body Steering is
   Period : constant Time_Span := Milliseconds(System_Config.Steering_Period);

   protected type Steering_Data_T is
      procedure Set_Angle (A : Integer);
      function Get_Angle return Integer;
   private
      Angle : Integer := 0;
   end Steering_Data_T;

   protected body Steering_Data_T is
      procedure Set_Angle (A: Integer) is
      begin
         Angle := A;
      end Set_Angle;

      function Get_Angle return Integer is
      begin
         return Angle;
      end Get_Angle;
   end Steering_Data_T;

   Steering1 : Steering_Data_T;

   function Get_Angle_Steering1 return Integer is
   begin
      return Steering1.Get_Angle;
   end Get_Angle_Steering1;

   procedure Set_Angle_Steering1 (A: Integer) is
   begin
      Steering1.Set_Angle(A);
   end Set_Angle_Steering1;
   
   task body Steering_Task is 
      Next_Release : Time := Clock;
   begin
      loop
         --
         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Steering_Task;

   procedure Init is 
   begin
      null;
   end Init;

end Steering;