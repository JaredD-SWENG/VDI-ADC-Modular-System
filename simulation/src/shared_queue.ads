with Event_Types; use Event_Types;

package Shared_Queue is
   type Queue_Array is array (1 .. 10) of Event_Access;

   protected Queue_Manager is
      entry Enqueue(Item : Event_Access);
      entry Dequeue(Item : out Event_Access);
      function Is_Empty return Boolean;
      function Has_Signal_Event return Boolean;
      procedure Remove_Old_Events(Event_Kind : Event_Type);
   private
      Queue : Queue_Array;
      Head, Tail : Integer := 0;
      Count : Integer := 0;
      function Find_Event_Index(Event_Kind : Event_Type) return Integer;
   end Queue_Manager;
end Shared_Queue;
