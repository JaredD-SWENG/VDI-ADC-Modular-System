with Steering_Motor; use Steering_Motor;
with Command_Queue; use Command_Queue;
with Commands; use Commands;

package body Steering_Task is

   task body Steering_Handler is
      Steering : Steering_Motor.Steering;
      Cmd      : Commands.Command_Type;
      Param    : Commands.Command_Param;
   begin
      Steering_Motor.Initialize(Steering);

      loop
         Command_Queue.Main_Queue.Get(Cmd, Param);

         case Cmd is
            when Set_Steering_Angle =>
               Steering_Motor.Set_Angle(Steering, Param.Speed);
            when Center_Steering =>
               Steering_Motor.Center(Steering);
            when Exit_Command =>
               exit;
            when others =>
               null;
         end case;
      end loop;

      Steering_Motor.Center(Steering);
      Steering_Motor.Disable(Steering);
   end Steering_Handler;

   procedure Start is null;
end Steering_Task;
