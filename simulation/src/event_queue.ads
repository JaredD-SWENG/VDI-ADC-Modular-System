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

--  with Event_Types; use Event_Types;

--  package Event_Queue is
--     type Queue_Array is array (1 .. 10) of Event_Access;

--     -- PRINT EVENT QUEUE

--     protected Queue_Manager is
--        entry Enqueue(Item : Event_Access);
--        entry Dequeue(Item : out Event_Access);
--        function Is_Empty return Boolean;
--        function Has_Signal_Event return Boolean;
--        procedure Remove_Old_Events(Event_Kind : Event_Type);
--     private
--        Queue : Queue_Array;
--        Head, Tail : Integer := 0;
--        Count : Integer := 0;
--        function Find_Event_Index(Event_Kind : Event_Type) return Integer;
--     end Queue_Manager;
--  end Event_Queue;
