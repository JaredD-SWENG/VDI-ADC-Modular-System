with Ada.Text_IO; use Ada.Text_IO;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;

--  with Host_Serial; use Host_Serial;

with GUI_Function;

package body Path_Planning is
   task body Path_Planning_Task is
      Current_Event : Event_Access;
      Processing    : Boolean := False;
   begin
      accept Start;
      Put_Line ("Path Planning Started");

      loop
         select
            accept Kill do
               Processing := False;
               --  Put_Line ("Path Planning: Processing killed for higher priority event");
               --  GUI_Function.AddConsoleText ("Path Planning: Processing killed for higher priority event");
            end Kill;
         or
            accept Process do
               Processing := True;
            end Process;
         end select;

         while Processing loop
            select
               accept Kill do
                  Processing := False;
                  --  Put_Line ("Path Planning: Processing killed for higher priority event");
                  --  GUI_Function.AddConsoleText ("Path Planning: Processing killed for higher priority event");
               end Kill;
            else
               if Queue_Manager.Is_Empty then
                  --  Put_Line ("Path Planning: No events to process");
                  --  GUI_Function.AddConsoleText ("Path Planning: No events to process");

                  Processing := False;
               else
                  Queue_Manager.Dequeue (Current_Event);
                  if Current_Event.all in Signal_State'Class then
                     declare
                        Signal :
                          Signal_State renames
                          Signal_State (Current_Event.all);
                     begin
                        --  Put_Line
                        --    ("Processing Event: Signal State - " &
                        --     Signal_Color'Image (Signal.Color));
                        case Signal.Color is
                           when Red =>
                              null;
                              --  Put_Line ("Path Planning: Stop");
                              --  GUI_Function.AddConsoleText ("Path Planning: Stop");
                              --  Send_Command(stop);
                           when Yellow =>
                              null;
                              --  Put_Line ("Path Planning: Slow");
                              --  GUI_Function.AddConsoleText ("Path Planning: Slow");
                              --  Send_Command(stop);
                           when Green =>
                              null;
                              --  Put_Line ("Path Planning: Go");
                              GUI_Function.AddConsoleText ("Path Planning: Go");
                              --  Send_Command(go);
                        end case;
                        delay 0.1;
                     end;
                  elsif Current_Event.all in Offset'Class then
                     declare
                        Lane_Offset :
                          Offset renames Offset (Current_Event.all);
                     begin
                        Put_Line
                          ("Processing Event: Offset - Value: " &
                           Float'Image (Lane_Offset.Value));
                        --  GUI_Function.AddConsoleText ("Processing Event: Offset - Value: " & Float'Image (Lane_Offset.Value));
                        if Lane_Offset.Value < 0.0 then
                           --  Put_Line ("Path Planning: Turn Right");
                           GUI_Function.RightSignalOn;
                           GUI_Function.LeftSignalOff;
                           --  GUI_Function.AddConsoleText ("Path Planning: Turn Right");
                           --  Send_Command(right);
                        elsif Lane_Offset.Value > 0.0 then
                           --  Put_Line ("Path Planning: Turn Left");
                           GUI_Function.LeftSignalOn;
                           GUI_Function.RightSignalOff;
                           --  GUI_Function.AddConsoleText ("Path Planning: Turn Left");
                           --  Send_Command(left);
                        else
                           --  Put_Line ("Path Planning: Center");
                           GUI_Function.LeftSignalOff;
                           GUI_Function.RightSignalOff;
                           --  GUI_Function.AddConsoleText ("Path Planning: Center");
                           --  Send_Command(center);
                        end if;
                        delay 0.1;
                     end;
                  end if;
               end if;
            end select;
         end loop;
      end loop;
   end Path_Planning_Task;
end Path_Planning;