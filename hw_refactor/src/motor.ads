with System_Config;
with STM32.Board; -- LED init and named constants
with STM32.PWM; -- PWM control
with STM32.Timers; -- Timer control
with STM32.GPIO; -- GPIO control
with STM32.Device; -- Device specific constants

package Motor is

   procedure Init;
   function Get_Speed_Motor_1 return Integer;
   procedure Set_Speed_Motor_1 (S : Integer);

   private
   task Motor_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Motor_Priority;
end Motor;