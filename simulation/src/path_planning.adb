--  with Ada.Text_IO;                use Ada.Text_IO;
--  with Event_Types;                use Event_Types;
--  with Event_Queue;                use Event_Queue;
--  with Ada.Streams;                use Ada.Streams;
--  with Ada.Finalization;           use Ada.Finalization;
--  with GNAT.Serial_Communications; use GNAT.Serial_Communications;

--  package body Path_Planning is
--     Port : aliased Serial_Port;

--     procedure Close_Port;

--     procedure Send_Command (Command : Stream_Element) is
--     begin
--        String'Write (Port'Access, (1 => Character'Val (Command)));
--     end Send_Command;

--     task body Path_Planning_Task is
--        Current_Event : Event_Access;
--        Processing    : Boolean := False;
--     begin
--        -- Initialize UART communication
--        Port.Open (Name => "/dev/ttyUSB0");  -- Adjust port name as needed
--        Port.Set
--          (Rate => B115200, Bits => CS8, Stop_Bits => One, Parity => None);

--        accept Start;

--        loop
--           select
--              accept Kill do
--                 Processing := False;
--                 Put_Line
--                   ("Path Planning: Processing killed for higher priority event");
--              end Kill;
--           or
--              accept Process do
--                 Processing := True;
--              end Process;
--           end select;

--           while Processing loop
--              select
--                 accept Kill do
--                    Processing := False;
--                    Put_Line
--                      ("Path Planning: Processing killed for higher priority event");
--                 end Kill;
--              else
--                 if Queue_Manager.Is_Empty then
--                    Processing := False;
--                 else
--                    Queue_Manager.Dequeue (Current_Event);
--                    if Current_Event.all in Signal_State'Class then
--                       declare
--                          Signal :
--                            Signal_State renames
--                            Signal_State (Current_Event.all);
--                       begin
--                          Put_Line
--                            ("Processing Event: Signal State - " &
--                             Signal_Color'Image (Signal.Color));
--                          case Signal.Color is
--                             when Red =>
--                                Put_Line ("Path Planning: Stop");
--                                Send_Command ("D0000");
--                             when Yellow =>
--                                Put_Line ("Path Planning: Slow");
--                                Send_Command ("D0500");
--                             when Green =>
--                                Put_Line ("Path Planning: Go");
--                                Send_Command ("D1000");
--                          end case;
--                       end;
--                    elsif Current_Event.all in Offset'Class then
--                       declare
--                          Lane_Offset :
--                            Offset renames Offset (Current_Event.all);
--                       begin
--                          Put_Line
--                            ("Processing Event: Offset - Value: " &
--                             Float'Image (Lane_Offset.Value));
--                          if Lane_Offset.Value < 0.0 then
--                             Put_Line ("Path Planning: Turn Right");
--                             Send_Command ("S1000");
--                          elsif Lane_Offset.Value > 0.0 then
--                             Put_Line ("Path Planning: Turn Left");
--                             Send_Command ("S0000");
--                          else
--                             Put_Line ("Path Planning: Center");
--                             Send_Command ("S0500");
--                          end if;
--                          delay 2.0;
--                       end;
--                    end if;
--                 end if;
--              end select;
--           end loop;
--        end loop;
--     exception
--        when others =>
--           -- Make sure to close the port if an exception occurs
--           Close_Port;
--           raise;
--     end Path_Planning_Task;

--     procedure Close_Port is
--     begin
--        Port.Close;
--     exception
--        when Serial_Error =>
--           null;  -- Port was already closed
--     end Close_Port;

--     -- Use a controlled type for proper finalization
--     type Port_Controller is new Limited_Controlled with null record;

--     procedure Finalize (Object : in out Port_Controller) is
--     begin
--        Close_Port;
--     end Finalize;

--     Controller : Port_Controller;
--  end Path_Planning;

with Ada.Text_IO; use Ada.Text_IO;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;

package body Path_Planning is
   task body Path_Planning_Task is
      Current_Event : Event_Access;
      Processing    : Boolean := False;
   begin
      accept Start;

      loop
         select
            accept Kill do
               Processing := False;
               Put_Line
                 ("Path Planning: Processing killed for higher priority event");
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
                  Put_Line
                    ("Path Planning: Processing killed for higher priority event");
               end Kill;
            else
               if Queue_Manager.Is_Empty then
                  Processing := False;
               else
                  Queue_Manager.Dequeue (Current_Event);
                  if Current_Event.all in Signal_State'Class then
                     declare
                        Signal :
                          Signal_State renames
                          Signal_State (Current_Event.all);
                     begin
                        Put_Line
                          ("Processing Event: Signal State - " &
                           Signal_Color'Image (Signal.Color));
                        case Signal.Color is
                           when Red =>
                              Put_Line ("Path Planning: Stop");
                           when Yellow =>
                              Put_Line ("Path Planning: Slow");
                           when Green =>
                              Put_Line ("Path Planning: Go");
                        end case;
                     end;
                  elsif Current_Event.all in Offset'Class then
                     declare
                        Lane_Offset :
                          Offset renames Offset (Current_Event.all);
                     begin
                        Put_Line
                          ("Processing Event: Offset - Value: " &
                           Float'Image (Lane_Offset.Value));
                        if Lane_Offset.Value < 0.0 then
                           Put_Line ("Path Planning: Turn Right");
                        elsif Lane_Offset.Value > 0.0 then
                           Put_Line ("Path Planning: Turn Left");
                        else
                           Put_Line ("Path Planning: Center");
                        end if;
                        delay 2.0;
                     end;
                  end if;
               end if;
            end select;
         end loop;
      end loop;
   end Path_Planning_Task;
end Path_Planning;
