-- tasks.ads (Declare the task type)
pragma Profile (Ravenscar);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);
pragma Locking_Policy (Ceiling_Locking);
with Config;

with Ada.Real_Time; use Ada.Real_Time;
with STM32.Board; use STM32.Board;
package Tasks is
   task Simple_Task with Priority => Config.Simple_Task_Priority;
   task Another_Task with Priority => Config.Another_Task_Priority;
end Tasks;
