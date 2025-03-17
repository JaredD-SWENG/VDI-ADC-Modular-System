with Ada.Real_Time; use Ada.Real_Time;
with my_uart;

procedure Main is
   Period  : constant Time_Span := Milliseconds (100);
   Next_Release  : Time := Clock;
   
begin

   loop

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end loop;
end Main;