with Ada.Text_IO; use Ada.Text_IO;
with GUI_Functions;

package body Event_Queue is
   protected body Queue_Manager is
      entry Enqueue (Item : Event_Access) when Count < Queue_Size is
         Current     : Queue_Index := Head;
         Found       : Boolean     := False;
         Found_Index : Queue_Index := Head;
      begin
         -- Check if an event of the same type already exists in the queue
         for I in 1 .. Count loop
            if Queue (Current).Event_Kind = Item.Event_Kind then
               Found       := True;
               Found_Index := Current;
               exit;
            end if;

            -- Move to next position with wrap-around
            if Current = Queue_Index'Last then
               Current := Queue_Index'First;
            else
               Current := Current + 1;
            end if;
         end loop;

         if Found then
            -- Replace existing event of the same type
            Queue (Found_Index) := Item;
            GUI_Functions.AddConsoleText ("Replaced existing " & Event_Type'Image (Item.Event_Kind) & " event");
         else
            -- Add new event to the end of the queue
            Queue (Tail) := Item;

            -- Update tail position with wrap-around
            if Tail = Queue_Index'Last then
               Tail := Queue_Index'First;
            else
               Tail := Tail + 1;
            end if;

            Count := Count + 1;
         end if;
      end Enqueue;

      entry Dequeue (Item : out Event_Access) when Count > 0 is
         Highest_Priority : Priority_Level := Priority_Level'First;
         Highest_Index    : Queue_Index    := Head;
         Current          : Queue_Index    := Head;
      begin
         -- Find highest priority event in the queue
         for I in 1 .. Count loop
            if Queue (Current).Priority > Highest_Priority then
               Highest_Priority := Queue (Current).Priority;
               Highest_Index    := Current;
            end if;

            -- Move to next position with wrap-around
            if Current = Queue_Index'Last then
               Current := Queue_Index'First;
            else
               Current := Current + 1;
            end if;
         end loop;

         -- Save the highest priority event
         Item                   := Queue (Highest_Index);
         Last_Dequeued_Priority := Item.Priority;

         -- Remove the item by shifting elements
         if Highest_Index /= Head then
            -- If not at head, move the item to remove to the head position
            Queue (Highest_Index) := Queue (Head);
         end if;

         -- Remove from head
         if Head = Queue_Index'Last then
            Head := Queue_Index'First;
         else
            Head := Head + 1;
         end if;

         Count := Count - 1;
      end Dequeue;

      function Is_Empty return Boolean is
      begin
         return Count = 0;
      end Is_Empty;

      function Has_Higher_Priority_Event return Boolean is
         Highest_Priority : Priority_Level := Priority_Level'First;
         Current          : Queue_Index    := Head;
      begin
         if Count = 0 then
            return False;
         end if;

         -- Find highest priority in queue
         for I in 1 .. Count loop
            if Queue (Current).Priority > Highest_Priority then
               Highest_Priority := Queue (Current).Priority;
            end if;

            if Current = Queue_Index'Last then
               Current := Queue_Index'First;
            else
               Current := Current + 1;
            end if;
         end loop;

         -- Compare with last dequeued priority
         return Highest_Priority > Last_Dequeued_Priority;
      end Has_Higher_Priority_Event;

      procedure PrintQueue is
         Current : Queue_Index := Head;
      begin
         if Count = 0 then
            GUI_Functions.AddConsoleText ("Queue is empty");
         else
            GUI_Functions.AddConsoleText ("Queue contents:");
            for I in 1 .. Count loop
               GUI_Functions.AddConsoleText
                 ("Item " & Natural'Image (I) & " - Type: " &
                  Event_Type'Image (Queue (Current).Event_Kind) &
                  " - Priority: " &
                  Priority_Level'Image (Queue (Current).Priority));

               if Current = Queue_Index'Last then
                  Current := Queue_Index'First;
               else
                  Current := Current + 1;
               end if;
            end loop;
         end if;
      end PrintQueue;
   end Queue_Manager;
end Event_Queue;