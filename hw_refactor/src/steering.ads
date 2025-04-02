with System_Config;
with STM32.Board; -- LED init and named constants
with STM32.PWM; -- PWM control
with STM32.Timers; -- Timer control
with STM32.GPIO; -- GPIO control
with STM32.Device; -- Device specific constants

package Steering is

   procedure Init;
   function Get_Angle_Steering1 return Integer;
   procedure Set_Angle_Steering1 (A : Integer);

   private
   task Steering_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Steering_Priority;
end Steering;