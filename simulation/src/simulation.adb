with Ada.Text_IO;
with Simulated_Camera;
with Signal_Recognition;
with Lane_Detection;
with Path_Planning;
with Event_Broker;
with Camera; use Camera;
with CV_Ada; use CV_Ada;

procedure Simulation is
   CAMERA_PATH : constant String := "..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder";
begin
   Simulated_Camera.Start (CAMERA_PATH);
   Lane_Detection.Lane_Detection_Task.Start (Priority => 1);
   Signal_Recognition.Signal_Recognition_Task.Start (Priority => 2);
   Path_Planning.Path_Planning_Task.Start;
   Event_Broker.Event_Broker_Task.Start;
end Simulation;

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

--  with Ada.Text_IO;   use Ada.Text_IO;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Real_Time; use Ada.Real_Time;
--  with System;        use System;
--  with Ada.Dynamic_Priorities;
--  with Ada.Task_Identification;

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
--           delay 10.0;
--        end loop;
--     end Producer;

--     -- Event Broker task body implementation
--     task body Event_Broker is
--        Consumer_1 : Consumer (1, System.Priority'First + 10);
--        Consumer_2 : Consumer (2, System.Priority'First + 20);
--        Consumer_3 : Consumer (3, System.Priority'First + 30);
--        Item       : Integer;

--        procedure Preempt_Consumer
--          (Consumer_Task_Id : Ada.Task_Identification.Task_Id;
--           New_Priority     : System.Priority) is
--        begin
--           Ada.Dynamic_Priorities.Set_Priority (New_Priority, Consumer_Task_Id);
--           Put_Line ("Event Broker: Preempting with higher priority event.");
--           delay until
--             Clock + Milliseconds (100); -- Ensure time for preemption effect.
--        end Preempt_Consumer;

--     begin
--        loop
--           Shared_Queue.Dequeue (Item);
--           Put_Line ("Event Broker: Received " & Integer'Image (Item));

--           case Item is
--              when 1 =>
--                 Preempt_Consumer
--                   (Consumer_1'Identity, System.Priority'First + 10);
--                 Consumer_1.Process;

--              when 2 =>
--                 Preempt_Consumer
--                   (Consumer_2'Identity, System.Priority'First + 20);
--                 Consumer_2.Process;

--              when 3 =>
--                 Preempt_Consumer
--                   (Consumer_3'Identity, System.Priority'First + 30);
--                 Consumer_3.Process;

--              when others =>
--                 null;
--           end case;

--           delay until
--             Clock + Milliseconds (500); -- Allow some time between processing.
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
--                    (20000);  -- Simulate processing time with Real_Time delay for better control over timing.
--              Put_Line ("Consumer" & Integer'Image (ID) & ": Done");
--           end Process;
--        end loop;
--     end Consumer;

--  begin
--     null;  -- Main procedure does not need to do anything else.
--  end Simulation;

-- FLESHED OUT VERSION OF THE SIMULATION CODE

--  with Ada.Text_IO;             use Ada.Text_IO;
--  with Ada.Numerics.Float_Random;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Real_Time;           use Ada.Real_Time;
--  with System;                  use System;
--  with Ada.Task_Identification; use Ada.Task_Identification;
--  with Ada.Task_Termination;

--  procedure Simulation is
--     -- Event type definitions
--     type Event_Type is (Signal_Event, Offset_Event);

--     type Base_Event is tagged record
--        Event_Kind : Event_Type;
--     end record;

--     type Offset is new Base_Event with record
--        Value : Float;
--     end record;

--     type Signal_Color is (Red, Yellow, Green);
--     type Signal_State is new Base_Event with record
--        Color : Signal_Color;
--     end record;

--     -- Queue definitions
--     type Event_Access is access Base_Event'Class;
--     type Queue_Array is array (1 .. 10) of Event_Access;

--     protected type Priority_Queue is
--        entry Enqueue (Item : Event_Access);
--        entry Dequeue (Item : out Event_Access);
--        function Is_Empty return Boolean;
--        function Has_Signal_Event return Boolean;
--        procedure Remove_Old_Events (Event_Kind : Event_Type);
--     private
--        Queue : Queue_Array;
--        Head, Tail : Integer := 0;
--        Count : Integer := 0;

--        -- Helper function to find an event of a specific type in the queue
--        function Find_Event_Index (Event_Kind : Event_Type) return Integer;
--     end Priority_Queue;

--     protected body Priority_Queue is
--        entry Enqueue (Item : Event_Access) when Count < Queue'Length is
--           Existing_Index : Integer;
--        begin
--           -- Check if an event of the same type already exists in the queue
--           Existing_Index := Find_Event_Index (Item.Event_Kind);

--           if Existing_Index /= 0 then
--              -- Replace the existing event with the new one
--              Queue (Existing_Index) := Item;
--           else
--              -- Add the new event to the queue
--              Tail := Tail mod Queue'Length + 1;
--              Queue (Tail) := Item;
--              Count := Count + 1;
--           end if;
--        end Enqueue;

--        entry Dequeue (Item : out Event_Access) when Count > 0 is
--           Signal_Index : Integer := 0;
--        begin
--           -- First check for signal events
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Signal_Event then
--                    Signal_Index := Index;
--                    exit;
--                 end if;
--              end;
--           end loop;

--           if Signal_Index /= 0 then
--              -- Return signal event and reorganize queue
--              Item := Queue (Signal_Index);
--              for I in Signal_Index .. Tail - 1 loop
--                 Queue (I) := Queue (I + 1);
--              end loop;
--              Tail := Tail - 1; -- Adjust tail after removing an element

--           else
--              -- No signal events, return next event in FIFO order
--              Head := Head mod Queue'Length + 1;
--              Item := Queue (Head);
--           end if;

--           Count := Count - 1; -- Decrement count after dequeueing an item
--        end Dequeue;

--        function Is_Empty return Boolean is
--        begin
--           return Count = 0;
--        end Is_Empty;

--        function Has_Signal_Event return Boolean is
--        begin
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Signal_Event then
--                    return True;
--                 end if;
--              end;
--           end loop;
--           return False;
--        end Has_Signal_Event;

--        procedure Remove_Old_Events (Event_Kind : Event_Type) is
--           I : Integer := 1;
--        begin
--           while I <= Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Event_Kind then
--                    -- Remove this event by shifting subsequent elements leftward
--                    for J in Index .. Tail - 1 loop
--                       Queue (J) := Queue (J + 1);
--                    end loop;
--                    Tail := Tail - 1;
--                    Count := Count - 1;
--                 else
--                    I := I + 1;
--                 end if;
--              end;
--           end loop;
--        end Remove_Old_Events;

--        function Find_Event_Index (Event_Kind : Event_Type) return Integer is
--        begin
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Event_Kind then
--                    return Index;
--                 end if;
--              end;
--           end loop;

--           return 0; -- Return 0 if no matching event is found
--        end Find_Event_Index;
--     end Priority_Queue;

--     Shared_Queue : Priority_Queue;

--     -- Random generators and tasks remain unchanged...

--     Path_Planning_ID : Task_Id;

--     -- Random generators
--     F_Gen : Ada.Numerics.Float_Random.Generator;

--     package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
--     S_Gen : Signal_Generator.Generator;

--     -- Producer tasks
--     task Lane_Detection;
--     task Signal_Recognition;

--     -- Consumer task
--     task Path_Planning;

--     task body Lane_Detection is
--        Offset_Value : Float;
--     begin
--        Ada.Numerics.Float_Random.Reset (F_Gen);
--        loop
--           Offset_Value :=
--             (Ada.Numerics.Float_Random.Random (F_Gen) * 2.0) - 1.0;
--           Offset_Value := Float'Rounding (Offset_Value * 100.0) / 100.0;

--           Shared_Queue.Enqueue
--             (new Offset'(Event_Kind => Offset_Event, Value => Offset_Value));
--           Put_Line ("Lane Detection Offset: " & Float'Image (Offset_Value));
--           delay 1.0;
--        end loop;
--     end Lane_Detection;

--     task body Signal_Recognition is
--        Signal_Value : Signal_Color;
--     begin
--        Signal_Generator.Reset (S_Gen);
--        loop
--           Signal_Value := Signal_Generator.Random (S_Gen);
--           Shared_Queue.Enqueue
--             (new Signal_State'
--                (Event_Kind => Signal_Event, Color => Signal_Value));
--           Put_Line
--             ("Signal Recognition State: " & Signal_Color'Image (Signal_Value));
--           delay 3.0;
--        end loop;
--     end Signal_Recognition;

--     task body Path_Planning is
--        Current_Event : Event_Access;
--     begin
--        Path_Planning_ID := Current_Task;
--        loop
--           Shared_Queue.Dequeue (Current_Event);

--           -- Print the details of the event being processed
--           if Current_Event.all in Signal_State'Class then
--              declare
--                 Signal : Signal_State renames Signal_State (Current_Event.all);
--              begin
--                 Put_Line
--                   ("Processing Event: Signal State - "
--                    & Signal_Color'Image (Signal.Color));
--                 case Signal.Color is
--                    when Red =>
--                       Put_Line ("Path Planning: Stop");

--                    when Yellow =>
--                       Put_Line ("Path Planning: Slow");

--                    when Green =>
--                       Put_Line ("Path Planning: Go");
--                 end case;
--              end;
--           elsif Current_Event.all in Offset'Class then
--              declare
--                 Lane_Offset : Offset renames Offset (Current_Event.all);
--              begin
--                 Put_Line
--                   ("Processing Event: Offset - Value: "
--                    & Float'Image (Lane_Offset.Value));
--                 if Lane_Offset.Value < 0.0 then
--                    Put_Line ("Path Planning: Turn Right");
--                 elsif Lane_Offset.Value > 0.0 then
--                    Put_Line ("Path Planning: Turn Left");
--                 else
--                    Put_Line ("Path Planning: Center");
--                 end if;

--                 -- Check if there's a signal event while processing offset
--                 if Shared_Queue.Has_Signal_Event then
--                    Put_Line ("Interrupting offset processing for signal event");
--                    goto Continue;
--                 end if;

--                 delay 5.0;
--              end;
--           end if;

--           <<Continue>>
--        end loop;
--     end Path_Planning;

--  begin
--     null;
--  end Simulation;

-- VERSION BELOW HAS EVENT BROKER

--  with Ada.Text_IO;             use Ada.Text_IO;
--  with Ada.Numerics.Float_Random;
--  with Ada.Numerics.Discrete_Random;
--  with Ada.Real_Time;           use Ada.Real_Time;
--  with System;                  use System;
--  with Ada.Task_Identification; use Ada.Task_Identification;
--  with Ada.Dynamic_Priorities;

--  procedure Simulation is
--     -- Event type definitions (unchanged)
--     type Event_Type is (Signal_Event, Offset_Event);

--     type Base_Event is tagged record
--        Event_Kind : Event_Type;
--     end record;

--     type Offset is new Base_Event with record
--        Value : Float;
--     end record;

--     type Signal_Color is (Red, Yellow, Green);
--     type Signal_State is new Base_Event with record
--        Color : Signal_Color;
--     end record;

--     -- Queue definitions (unchanged)
--     type Event_Access is access Base_Event'Class;
--     type Queue_Array is array (1 .. 10) of Event_Access;

--     -- Priority Queue protected object (unchanged)
--     protected type Priority_Queue is
--        entry Enqueue (Item : Event_Access);
--        entry Dequeue (Item : out Event_Access);
--        function Is_Empty return Boolean;
--        function Has_Signal_Event return Boolean;
--        procedure Remove_Old_Events (Event_Kind : Event_Type);
--     private
--        Queue : Queue_Array;
--        Head, Tail : Integer := 0;
--        Count : Integer := 0;
--        function Find_Event_Index (Event_Kind : Event_Type) return Integer;
--     end Priority_Queue;

--     -- Priority Queue implementation remains unchanged
--     protected body Priority_Queue is
--        -- Previous implementation remains the same
--        entry Enqueue (Item : Event_Access) when Count < Queue'Length is
--           Existing_Index : Integer;
--        begin
--           -- Check if an event of the same type already exists in the queue
--           Existing_Index := Find_Event_Index (Item.Event_Kind);

--           if Existing_Index /= 0 then
--              -- Replace the existing event with the new one
--              Queue (Existing_Index) := Item;
--           else
--              -- Add the new event to the queue
--              Tail := Tail mod Queue'Length + 1;
--              Queue (Tail) := Item;
--              Count := Count + 1;
--           end if;
--        end Enqueue;

--        entry Dequeue (Item : out Event_Access) when Count > 0 is
--           Signal_Index : Integer := 0;
--        begin
--           -- First check for signal events
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Signal_Event then
--                    Signal_Index := Index;
--                    exit;
--                 end if;
--              end;
--           end loop;

--           if Signal_Index /= 0 then
--              -- Return signal event and reorganize queue
--              Item := Queue (Signal_Index);
--              for I in Signal_Index .. Tail - 1 loop
--                 Queue (I) := Queue (I + 1);
--              end loop;
--              Tail := Tail - 1; -- Adjust tail after removing an element

--           else
--              -- No signal events, return next event in FIFO order
--              Head := Head mod Queue'Length + 1;
--              Item := Queue (Head);
--           end if;

--           Count := Count - 1; -- Decrement count after dequeueing an item
--        end Dequeue;

--        function Is_Empty return Boolean is
--        begin
--           return Count = 0;
--        end Is_Empty;

--        function Has_Signal_Event return Boolean is
--        begin
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Signal_Event then
--                    return True;
--                 end if;
--              end;
--           end loop;
--           return False;
--        end Has_Signal_Event;

--        procedure Remove_Old_Events (Event_Kind : Event_Type) is
--           I : Integer := 1;
--        begin
--           while I <= Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Event_Kind then
--                    -- Remove this event by shifting subsequent elements leftward
--                    for J in Index .. Tail - 1 loop
--                       Queue (J) := Queue (J + 1);
--                    end loop;
--                    Tail := Tail - 1;
--                    Count := Count - 1;
--                 else
--                    I := I + 1;
--                 end if;
--              end;
--           end loop;
--        end Remove_Old_Events;

--        function Find_Event_Index (Event_Kind : Event_Type) return Integer is
--        begin
--           for I in 1 .. Count loop
--              declare
--                 Index : Integer := (Head + I - 1) mod Queue'Length + 1;
--              begin
--                 if Queue (Index).Event_Kind = Event_Kind then
--                    return Index;
--                 end if;
--              end;
--           end loop;

--           return 0; -- Return 0 if no matching event is found
--        end Find_Event_Index;
--     end Priority_Queue;

--     Shared_Queue : Priority_Queue;

--     -- Random generators
--     F_Gen : Ada.Numerics.Float_Random.Generator;
--     package Signal_Generator is new Ada.Numerics.Discrete_Random (Signal_Color);
--     S_Gen : Signal_Generator.Generator;

--     -- Producer tasks
--     task Lane_Detection;
--     task Signal_Recognition;

--     -- Event Broker task
--     task Event_Broker;

--     -- Consumer task with entry point
--     task Path_Planning is
--        entry Process;
--        entry Kill;
--     end Path_Planning;

--     -- Producer implementations (unchanged)
--     task body Lane_Detection is
--        Offset_Value : Float;
--     begin
--        Ada.Numerics.Float_Random.Reset (F_Gen);
--        loop
--           Offset_Value :=
--             (Ada.Numerics.Float_Random.Random (F_Gen) * 2.0) - 1.0;
--           Offset_Value := Float'Rounding (Offset_Value * 100.0) / 100.0;
--           Shared_Queue.Enqueue
--             (new Offset'(Event_Kind => Offset_Event, Value => Offset_Value));
--           Put_Line ("Lane Detection Offset: " & Float'Image (Offset_Value));
--           delay 1.0;
--        end loop;
--     end Lane_Detection;

--     task body Signal_Recognition is
--        Signal_Value : Signal_Color;
--     begin
--        Signal_Generator.Reset (S_Gen);
--        loop
--           Signal_Value := Signal_Generator.Random (S_Gen);
--           Shared_Queue.Enqueue
--             (new Signal_State'
--                (Event_Kind => Signal_Event, Color => Signal_Value));
--           Put_Line
--             ("Signal Recognition State: " & Signal_Color'Image (Signal_Value));
--           delay 3.0;
--        end loop;
--     end Signal_Recognition;

--     task body Event_Broker is
--        Processing_Active : Boolean := False;
--     begin
--        loop
--           if Shared_Queue.Has_Signal_Event and Processing_Active then
--              Path_Planning.Kill;
--              Processing_Active := False;
--           end if;

--           if not Processing_Active then
--              Processing_Active := True;
--              Path_Planning.Process;
--           end if;

--           delay 0.1;  -- Small delay to prevent busy waiting
--        end loop;
--     end Event_Broker;

--     task body Path_Planning is
--        Current_Event : Event_Access;
--        Processing    : Boolean := False;
--     begin
--        loop
--           select
--              accept Kill do
--                 Processing := False;
--                 Put_Line
--                   ("Path Planning: Processing killed for higher priority event");
--              end Kill;
--           or
--              accept Process do
--                 Processing := True;
--              end Process;
--           end select;

--           while Processing loop
--              select
--                 accept Kill do
--                    Processing := False;
--                    Put_Line
--                      ("Path Planning: Processing killed for higher priority event");
--                 end Kill;
--              else
--                 if Shared_Queue.Is_Empty then
--                    Processing := False;
--                 else
--                    Shared_Queue.Dequeue (Current_Event);

--                    if Current_Event.all in Signal_State'Class then
--                       declare
--                          Signal : Signal_State
--                            renames Signal_State (Current_Event.all);
--                       begin
--                          Put_Line
--                            ("Processing Event: Signal State - "
--                             & Signal_Color'Image (Signal.Color));
--                          case Signal.Color is
--                             when Red =>
--                                Put_Line ("Path Planning: Stop");

--                             when Yellow =>
--                                Put_Line ("Path Planning: Slow");

--                             when Green =>
--                                Put_Line ("Path Planning: Go");
--                          end case;
--                       end;
--                    elsif Current_Event.all in Offset'Class then
--                       declare
--                          Lane_Offset : Offset
--                            renames Offset (Current_Event.all);
--                       begin
--                          Put_Line
--                            ("Processing Event: Offset - Value: "
--                             & Float'Image (Lane_Offset.Value));
--                          if Lane_Offset.Value < 0.0 then
--                             Put_Line ("Path Planning: Turn Right");
--                          elsif Lane_Offset.Value > 0.0 then
--                             Put_Line ("Path Planning: Turn Left");
--                          else
--                             Put_Line ("Path Planning: Center");
--                          end if;
--                          delay
--                            5.0;  -- Simulate longer processing time for offset
--                       end;
--                    end if;
--                 end if;
--              end select;
--           end loop;
--        end loop;
--     end Path_Planning;

--  begin
--     null;
--  end Simulation;
