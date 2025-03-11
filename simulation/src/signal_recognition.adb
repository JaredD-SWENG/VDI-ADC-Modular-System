with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Event_Types; use Event_Types;
with Event_Queue; use Event_Queue;

package body Signal_Recognition is
   package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
   S_Gen : Signal_Generator.Generator;

   task body Signal_Recognition_Task is
      Signal_Value : Signal_Color;
   begin
      accept Start;
      Put_Line ("Signal State Started");

      Signal_Generator.Reset (S_Gen);
      loop
         Signal_Value := Signal_Generator.Random (S_Gen);
         Queue_Manager.Enqueue
           (new Signal_State'
              (Event_Kind => Signal_Event, Color => Signal_Value));
         Put_Line
           ("Signal Recognition State: " & Signal_Color'Image (Signal_Value));
         delay 1.0;
      end loop;
   end Signal_Recognition_Task;
end Signal_Recognition;
