with Command_Queue;
with Commands;

package UART_Task is
   task type UART_Handler;
   procedure Start;
private
   Task_Instance : UART_Handler;
end UART_Task;
