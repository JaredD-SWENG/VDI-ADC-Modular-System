with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Event_Types; use Event_Types;
with Shared_Queue; use Shared_Queue;

package body Lane_Detection is
   F_Gen : Ada.Numerics.Float_Random.Generator;

   task body Lane_Detection_Task is
      Offset_Value : Float;
   begin
      accept Start;

      Ada.Numerics.Float_Random.Reset(F_Gen);
      loop
         Offset_Value := (Ada.Numerics.Float_Random.Random(F_Gen) * 2.0) - 1.0;
         Offset_Value := Float'Rounding(Offset_Value * 100.0) / 100.0;
         Queue_Manager.Enqueue(new Offset'(Event_Kind => Offset_Event, 
                                         Value => Offset_Value));
         Put_Line("Lane Detection Offset: " & Float'Image(Offset_Value));
         delay 1.0;
      end loop;
   end Lane_Detection_Task;
end Lane_Detection;
