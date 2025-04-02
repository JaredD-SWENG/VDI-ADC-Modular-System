with System_Config;

package Uart is
   type Cmd is (center, go, left, right, stop, undefined);
   for Cmd use (center => Character'Pos('C'),
               go => Character'Pos('G'),
               left => Character'Pos('L'),
               right => Character'Pos('R'),
               stop => Character'Pos('S'),
               undefined => Character'Pos('U'));

   function Get_Command return Cmd;

private
   task Uart_Task with
      Storage_Size => 1 * 1024,
      Priority     => System_Config.Uart_Priority;
end Uart;