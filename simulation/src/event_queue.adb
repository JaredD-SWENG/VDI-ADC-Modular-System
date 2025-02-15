package body Event_Queue is
   protected body Queue_Manager is
      entry Enqueue(Item : Event_Access) when Count < Queue'Length is
         Existing_Index : Integer;
      begin
         Existing_Index := Find_Event_Index(Item.Event_Kind);
         if Existing_Index /= 0 then
            Queue(Existing_Index) := Item;
         else
            Tail := Tail mod Queue'Length + 1;
            Queue(Tail) := Item;
            Count := Count + 1;
         end if;
      end Enqueue;

      entry Dequeue(Item : out Event_Access) when Count > 0 is
         Signal_Index : Integer := 0;
      begin
         for I in 1 .. Count loop
            declare
               Index : Integer := (Head + I - 1) mod Queue'Length + 1;
            begin
               if Queue(Index).Event_Kind = Signal_Event then
                  Signal_Index := Index;
                  exit;
               end if;
            end;
         end loop;

         if Signal_Index /= 0 then
            Item := Queue(Signal_Index);
            for I in Signal_Index .. Tail - 1 loop
               Queue(I) := Queue(I + 1);
            end loop;
            Tail := Tail - 1;
         else
            Head := Head mod Queue'Length + 1;
            Item := Queue(Head);
         end if;
         Count := Count - 1;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      function Has_Signal_Event return Boolean is
      begin
         for I in 1 .. Count loop
            declare
               Index : Integer := (Head + I - 1) mod Queue'Length + 1;
            begin
               if Queue(Index).Event_Kind = Signal_Event then
                  return True;
               end if;
            end;
         end loop;
         return False;
      end Has_Signal_Event;

      procedure Remove_Old_Events(Event_Kind : Event_Type) is
         I : Integer := 1;
      begin
         while I <= Count loop
            declare
               Index : Integer := (Head + I - 1) mod Queue'Length + 1;
            begin
               if Queue(Index).Event_Kind = Event_Kind then
                  for J in Index .. Tail - 1 loop
                     Queue(J) := Queue(J + 1);
                  end loop;
                  Tail := Tail - 1;
                  Count := Count - 1;
               else
                  I := I + 1;
               end if;
            end;
         end loop;
      end Remove_Old_Events;

      function Find_Event_Index(Event_Kind : Event_Type) return Integer is
      begin
         for I in 1 .. Count loop
            declare
               Index : Integer := (Head + I - 1) mod Queue'Length + 1;
            begin
               if Queue(Index).Event_Kind = Event_Kind then
                  return Index;
               end if;
            end;
         end loop;
         return 0;
      end Find_Event_Index;
   end Queue_Manager;
end Event_Queue;