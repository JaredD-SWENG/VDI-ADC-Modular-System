with Digital_Out;
with STM32.PWM;
with STM32.Timers;

package Drive_Motor is

   -- 1) Public subprograms for high-level control
   procedure Initialize;    -- Call once, sets up pins/timer, starts Motor_Task
   procedure Set_Speed (Speed_Percentage : Integer);
   procedure Stop;
   procedure Emergency_Stop;
   procedure Calibrate;

   -- 2) Returns whether motor is currently enabled or not
   function Is_Enabled return Boolean;

   -- 3) Motor control task
   task Motor_Task;

private
   -- Shared state for the motor's requested speed, or special commands
   type Motor_Command_Type is (No_Command, Calibrate_Requested, Stop_Requested, Emergency_Stop_Requested);

   -- We store speed, plus the current pending command
   Motor_Speed : Integer := 0;  
   Motor_Command : Motor_Command_Type := No_Command;
   Motor_Enabled : Boolean := False;

end Drive_Motor;
