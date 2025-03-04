with Ada.Real_Time; use Ada.Real_Time;

-- Define the task behavior)
package body Tasks is
   task body Simple_Task is
      Period : constant Time_Span := Milliseconds(100);  -- 10Hz cycle
      Next_Time : Time := Clock;
   begin
      Initialize_LEDs;

      loop
         -- Your task logic here
         STM32.Board.Toggle (Red_LED);

         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Simple_Task;

      task body Another_Task is
      Period : constant Time_Span := Milliseconds(200);
      Next_Time : Time := Clock;
   begin
      loop
         -- Task logic for Another_Task
         STM32.Board.Toggle (Green_LED);
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Another_Task;

end Tasks;
