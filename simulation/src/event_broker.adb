with Ada.Text_IO;   use Ada.Text_IO;
with Event_Queue;   use Event_Queue;
with Path_Planning; use Path_Planning;

package body Event_Broker is
   task body Event_Broker_Task is
      Processing_Active : Boolean := False;
   begin
      accept Start;
      Put_Line ("Event Broker Started");

      loop
         if Queue_Manager.Has_Signal_Event and Processing_Active then
            Path_Planning.Path_Planning_Task.Kill;
            Processing_Active := False;
         end if;

         if not Processing_Active then
            Processing_Active := True;
            Path_Planning.Path_Planning_Task.Process;
         end if;

         delay 0.1;
      end loop;
   end Event_Broker_Task;
end Event_Broker;
