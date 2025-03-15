with Ada.Text_IO;   use Ada.Text_IO;
with Event_Queue;   use Event_Queue;
with Event_Types;   use Event_Types;
with Path_Planning; use Path_Planning;

with Ada.Text_IO;   use Ada.Text_IO;
with Event_Queue;   use Event_Queue;
with Path_Planning; use Path_Planning;

package body Event_Broker is
   task body Event_Broker_Task is
      Path_Processing : Boolean := False;
   begin
      accept Start;
      Put_Line ("Event Broker Started");

      loop
         declare
            Queue_Empty               : Boolean := Queue_Manager.Is_Empty;
            Queue_Has_Higher_Priority : Boolean :=
              Queue_Manager.Has_Higher_Priority_Event;
         begin
            -- Start Path Planning if there are events and it's not already processing
            if not Path_Processing and not Queue_Empty then
               Path_Planning_Task.Process;
               Path_Processing := True;
               Put_Line ("Event Broker: Started Path Planning");
            end if;

            -- Kill current processing if higher priority event arrives
            if Path_Processing and Queue_Has_Higher_Priority then
               Path_Planning_Task.Kill;
               Path_Processing := False; -- Will restart on next iteration
               Put_Line
                 ("Event Broker: Killed Path Planning for higher priority event");
            end if;

            -- Reset state when queue becomes empty
            if Path_Processing and Queue_Empty then
               Path_Processing := False;
               Put_Line
                 ("Event Broker: Path Planning completed (queue empty)");
            end if;
         end;

         delay 0.05; -- Small delay to prevent busy waiting
      end loop;
   end Event_Broker_Task;
end Event_Broker;

--  package body Event_Broker is
--     task body Event_Broker_Task is
--        Processing_Active : Boolean := False;
--     begin
--        accept Start;
--        Put_Line ("Event Broker Started");

--        loop
--           if Queue_Manager.Has_Signal_Event and Processing_Active then
--              Path_Planning.Path_Planning_Task.Kill;
--              Processing_Active := False;
--           end if;

--           if not Processing_Active then
--              Processing_Active := True;
--              Path_Planning.Path_Planning_Task.Process;
--           end if;

--           delay 0.1;
--        end loop;
--     end Event_Broker_Task;
--  end Event_Broker;
--
