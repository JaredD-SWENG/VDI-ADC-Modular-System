with Event_Types; use Event_Types;

package Event_Queue is
   Queue_Size : constant := 100;
   type Queue_Index is range 1 .. Queue_Size;
   type Event_Array is array (Queue_Index) of Event_Access;

   protected Queue_Manager is
      entry Enqueue (Item : Event_Access);
      entry Dequeue (Item : out Event_Access);
      function Is_Empty return Boolean;
      function Has_Higher_Priority_Event return Boolean;
      procedure PrintQueue;
   private
      Queue                  : Event_Array;
      Head                   : Queue_Index    := Queue_Index'First;
      Tail                   : Queue_Index    := Queue_Index'First;
      Count                  : Natural        := 0;
      Last_Dequeued_Priority : Priority_Level := Priority_Level'First;
   end Queue_Manager;
end Event_Queue;