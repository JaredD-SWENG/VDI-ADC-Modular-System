with System_Config;

package Motor is

   procedure Init;
   function Get_Speed_Motor_1 return Integer;
   procedure Set_Speed_Motor_1 (S : Integer);

   private
   task Motor_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Uart_Priority;
end Motor;