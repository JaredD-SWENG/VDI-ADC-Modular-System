with System_Config;

package Uart is

   procedure Init;
   function Get_Speed_Cmd return Integer;
   function Emergency_Stop return Boolean;

private
   task Uart_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Uart_Priority;
end Uart;