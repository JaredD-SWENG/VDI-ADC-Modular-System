with Drive_Motor;
with System;

package Motor_Task is
   task Motor_Handler with Priority => System.Default_Priority;
   procedure Start (Motor_Instance : in out Drive_Motor.Motor);
   procedure Set_Motor_Instance (Motor_Instance : access Drive_Motor.Motor);
private
   --  Motor_Instance_Ptr : access Drive_Motor.Motor := null;
end Motor_Task;
