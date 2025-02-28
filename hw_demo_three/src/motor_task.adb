with Command_Queue; use Command_Queue;
with Commands; use Commands;
with Coms_Uart; use Coms_Uart;
with Drive_Motor; use Drive_Motor;
with System; use System;

package body Motor_Task is

   -- Pointer to the motor instance
   Motor_Instance_Ptr : access Drive_Motor.Motor := null;

   procedure Set_Motor_Instance (Motor_Instance : access Drive_Motor.Motor) is
   begin
      Motor_Instance_Ptr := Motor_Instance;
   end Set_Motor_Instance;

   task body Motor_Handler is
      Cmd   : Commands.Command_Type;
      Param : Commands.Command_Param;
   begin
      Send_String_Newline("Motor_Task: Starting...");

      -- Ensure Motor_Instance_Ptr is set before proceeding
      while Motor_Instance_Ptr = null loop
         delay 0.01;
      end loop;

      -- Initialize using the passed Motor instance
      Initialize(Motor_Instance_Ptr.all);
      Send_String_Newline("Motor_Task: Initialized Motor, entering command loop");

      loop
         Command_Queue.Main_Queue.Get(Cmd, Param);
         Send_String_Newline("Debug motor_task command received");

         case Cmd is
            when Calibrate_Motor =>
               Enable(Motor_Instance_Ptr.all);

            when Set_Motor_Speed =>
               Set_Speed(Motor_Instance_Ptr.all, Param.Speed);

            when Emergency_Stop =>
               Emergency_Stop(Motor_Instance_Ptr.all);

            when Motor_Stop =>
               Stop(Motor_Instance_Ptr.all);

            when Exit_Command =>
               exit;

            when others =>
               null;
         end case;
      end loop;

      Send_String_Newline("[Motor_Task] Exiting concurrency.");
      Stop(Motor_Instance_Ptr.all);
      Disable(Motor_Instance_Ptr.all);
   end Motor_Handler;

   procedure Start (Motor_Instance : access Drive_Motor.Motor) is
   begin
      Set_Motor_Instance(Motor_Instance);
   end Start;

end Motor_Task;
