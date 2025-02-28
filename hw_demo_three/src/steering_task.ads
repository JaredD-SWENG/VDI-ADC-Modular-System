with Command_Queue;
with Commands;

package Steering_Task is
   task type Steering_Handler;
   procedure Start;
private
   Task_Instance : Steering_Handler;
end Steering_Task;
