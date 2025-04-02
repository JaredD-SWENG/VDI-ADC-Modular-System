with System_Config;
with STM32.Board; -- LED init and named constants
with STM32.PWM; -- PWM control
with STM32.Timers; -- Timer control
with STM32.GPIO; -- GPIO control
with STM32.Device; -- Device specific constants

package Motor is

   procedure Init;
   procedure Calibrate_Esc_30A;

   function Get_Speed_Drive return Integer;
   procedure Set_Speed_Drive (S : Integer);
   
   private
   task Motor_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Motor_Priority;
end Motor;