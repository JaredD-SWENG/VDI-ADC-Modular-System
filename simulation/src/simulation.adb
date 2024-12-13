--  with Ada.Text_IO; use Ada.Text_IO;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Containers.Unbounded_Synchronized_Queues;
--  with Ada.Containers.Synchronized_Queue_Interfaces;

--  procedure Simulation is
--     -- Define the queue interface for integers
--     package Integer_Queue_Interfaces is new
--       Ada.Containers.Synchronized_Queue_Interfaces (Element_Type => Integer);

--     -- Instantiate the queue package
--     package Integer_Queues is new
--       Ada.Containers.Unbounded_Synchronized_Queues
--         (Queue_Interfaces => Integer_Queue_Interfaces);

--     Shared_Queue : Integer_Queues.Queue;

--     -- Random number generator setup
--     subtype Random_Range is Integer range 1 .. 3;
--     package Random_Generator is new Ada.Numerics.Discrete_Random (Random_Range);
--     Gen : Random_Generator.Generator;

--     -- Producer task declaration
--     task Producer;

--     -- Event Broker task declaration
--     task Event_Broker;

--     -- Consumer task type declaration with an entry for processing
--     task type Consumer (ID : Positive) is
--        entry Process;
--     end Consumer;

--     -- Producer task body implementation
--     task body Producer is
--        Next_Number : Integer;
--     begin
--        Random_Generator.Reset (Gen);
--        loop
--           Next_Number := Random_Generator.Random (Gen);
--           Shared_Queue.Enqueue (Next_Number);
--           Put_Line ("Producer: Pushed " & Integer'Image (Next_Number));
--           delay 1.0;
--        end loop;
--     end Producer;

--     -- Event Broker task body implementation
--     task body Event_Broker is
--        Consumer_1 : Consumer (1);
--        Consumer_2 : Consumer (2);
--        Consumer_3 : Consumer (3);
--        Item       : Integer;
--     begin
--        loop
--           Shared_Queue.Dequeue (Item);
--           Put_Line ("Event Broker: Received " & Integer'Image (Item));
--           case Item is
--              when 1 =>
--                 Consumer_1.Process;

--              when 2 =>
--                 Consumer_2.Process;

--              when 3 =>
--                 Consumer_3.Process;

--              when others =>
--                 null;
--           end case;
--        end loop;
--     end Event_Broker;

--     -- Consumer task body implementation
--     task body Consumer is
--        Item : Integer;
--     begin
--        loop
--           accept Process do
--              Put_Line ("Consumer" & Integer'Image (ID) & ": Processing");
--              delay 3.0;  -- Simulate processing time
--              Put_Line ("Consumer" & Integer'Image (ID) & ": Done");
--           end Process;
--        end loop;
--     end Consumer;

--  begin
--     null;  -- Main procedure does not need to do anything else.
--  end Simulation;

-- VERSION BELOW DEALS WITH PRIORITY QUEUE 

--  with Ada.Text_IO;   use Ada.Text_IO;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Real_Time; use Ada.Real_Time;
--  with System;        use System;

--  procedure Simulation is

--     -- Define a named array type for the queue
--     type Queue_Array is array (1 .. 10) of Integer;

--     -- Protected object for managing the queue with priority
--     protected type Priority_Queue is
--        entry Enqueue (Item : Integer);
--        entry Dequeue (Item : out Integer);
--     private
--        Queue : Queue_Array := (others => 0);
--        Head, Tail : Integer := 0;
--        Count : Integer := 0;
--     end Priority_Queue;

--     protected body Priority_Queue is

--        entry Enqueue (Item : Integer) when Count < Queue'Length is
--        begin
--           Tail := Tail mod Queue'Length + 1;
--           Queue (Tail) := Item;
--           Count := Count + 1;
--           -- No requeue needed here, simply enqueue and manage count
--        end Enqueue;

--        entry Dequeue (Item : out Integer) when Count > 0 is
--        begin
--           Head := Head mod Queue'Length + 1;
--           Item := Queue (Head);
--           Count := Count - 1;
--        end Dequeue;

--     end Priority_Queue;

--     Shared_Queue : Priority_Queue;

--     -- Random number generator setup
--     subtype Random_Range is Integer range 1 .. 3;
--     package Random_Generator is new Ada.Numerics.Discrete_Random (Random_Range);
--     Gen : Random_Generator.Generator;

--     -- Producer task declaration
--     task Producer;

--     -- Event Broker task declaration
--     task Event_Broker;

--     -- Consumer task type declaration with an entry for processing
--     task type Consumer
--       (ID             : Positive;
--        Priority_Level : System.Priority)
--     is
--        pragma Priority (Priority_Level);
--        entry Process;
--     end Consumer;

--     -- Producer task body implementation
--     task body Producer is
--        Next_Number : Integer;
--     begin
--        Random_Generator.Reset (Gen);
--        loop
--           Next_Number := Random_Generator.Random (Gen);
--           Shared_Queue.Enqueue (Next_Number);
--           Put_Line ("Producer: Pushed " & Integer'Image (Next_Number));
--           delay 5.0;
--        end loop;
--     end Producer;

--     -- Event Broker task body implementation
--     task body Event_Broker is
--        Consumer_1 : Consumer (1, System.Priority'First + 10);
--        Consumer_2 : Consumer (2, System.Priority'First + 20);
--        Consumer_3 : Consumer (3, System.Priority'First + 30);
--        Item       : Integer;
--     begin
--        loop
--           Shared_Queue.Dequeue (Item);
--           Put_Line ("Event Broker: Received " & Integer'Image (Item));
--           case Item is
--              when 1 =>
--                 Consumer_1.Process;

--              when 2 =>
--                 Consumer_2.Process;

--              when 3 =>
--                 Consumer_3.Process;

--              when others =>
--                 null;
--           end case;
--        end loop;
--     end Event_Broker;

--     -- Consumer task body implementation
--     task body Consumer is
--     begin
--        loop
--           accept Process do
--              Put_Line ("Consumer" & Integer'Image (ID) & ": Processing");
--              delay until
--                Clock
--                + Milliseconds
--                    (8000);  -- Simulate processing time with Real_Time delay for better control over timing.
--              Put_Line ("Consumer" & Integer'Image (ID) & ": Done");
--           end Process;
--        end loop;
--     end Consumer;

--  begin
--     null;  -- Main procedure does not need to do anything else.
--  end Simulation;

-- VERSION BELOW DEALS WITH PRIORITY QUEUE AND PREEMPTION

with Ada.Text_IO;   use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time; use Ada.Real_Time;
with System;        use System;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;

procedure Simulation is

   -- Define a named array type for the queue
   type Queue_Array is array (1 .. 10) of Integer;

   -- Protected object for managing the queue with priority
   protected type Priority_Queue is
      entry Enqueue (Item : Integer);
      entry Dequeue (Item : out Integer);
   private
      Queue : Queue_Array := (others => 0);
      Head, Tail : Integer := 0;
      Count : Integer := 0;
   end Priority_Queue;

   protected body Priority_Queue is

      entry Enqueue (Item : Integer) when Count < Queue'Length is
      begin
         Tail := Tail mod Queue'Length + 1;
         Queue (Tail) := Item;
         Count := Count + 1;
         -- No requeue needed here, simply enqueue and manage count
      end Enqueue;

      entry Dequeue (Item : out Integer) when Count > 0 is
      begin
         Head := Head mod Queue'Length + 1;
         Item := Queue (Head);
         Count := Count - 1;
      end Dequeue;

   end Priority_Queue;

   Shared_Queue : Priority_Queue;

   -- Random number generator setup
   subtype Random_Range is Integer range 1 .. 3;
   package Random_Generator is new Ada.Numerics.Discrete_Random (Random_Range);
   Gen : Random_Generator.Generator;

   -- Producer task declaration
   task Producer;

   -- Event Broker task declaration
   task Event_Broker;

   -- Consumer task type declaration with an entry for processing
   task type Consumer
     (ID             : Positive;
      Priority_Level : System.Priority)
   is
      pragma Priority (Priority_Level);
      entry Process;
   end Consumer;

   -- Producer task body implementation
   task body Producer is
      Next_Number : Integer;
   begin
      Random_Generator.Reset (Gen);
      loop
         Next_Number := Random_Generator.Random (Gen);
         Shared_Queue.Enqueue (Next_Number);
         Put_Line ("Producer: Pushed " & Integer'Image (Next_Number));
         delay 10.0;
      end loop;
   end Producer;

   -- Event Broker task body implementation
   task body Event_Broker is
      Consumer_1 : Consumer (1, System.Priority'First + 10);
      Consumer_2 : Consumer (2, System.Priority'First + 20);
      Consumer_3 : Consumer (3, System.Priority'First + 30);
      Item       : Integer;

      procedure Preempt_Consumer
        (Consumer_Task_Id : Ada.Task_Identification.Task_Id;
         New_Priority     : System.Priority) is
      begin
         Ada.Dynamic_Priorities.Set_Priority (New_Priority, Consumer_Task_Id);
         Put_Line ("Event Broker: Preempting with higher priority event.");
         delay until
           Clock + Milliseconds (100); -- Ensure time for preemption effect.
      end Preempt_Consumer;

   begin
      loop
         Shared_Queue.Dequeue (Item);
         Put_Line ("Event Broker: Received " & Integer'Image (Item));

         case Item is
            when 1 =>
               Preempt_Consumer
                 (Consumer_1'Identity, System.Priority'First + 10);
               Consumer_1.Process;

            when 2 =>
               Preempt_Consumer
                 (Consumer_2'Identity, System.Priority'First + 20);
               Consumer_2.Process;

            when 3 =>
               Preempt_Consumer
                 (Consumer_3'Identity, System.Priority'First + 30);
               Consumer_3.Process;

            when others =>
               null;
         end case;

         delay until
           Clock + Milliseconds (500); -- Allow some time between processing.
      end loop;
   end Event_Broker;

   -- Consumer task body implementation
   task body Consumer is
   begin
      loop
         accept Process do
            Put_Line ("Consumer" & Integer'Image (ID) & ": Processing");
            delay until
              Clock
              + Milliseconds
                  (20000);  -- Simulate processing time with Real_Time delay for better control over timing.
            Put_Line ("Consumer" & Integer'Image (ID) & ": Done");
         end Process;
      end loop;
   end Consumer;

begin
   null;  -- Main procedure does not need to do anything else.
end Simulation;
