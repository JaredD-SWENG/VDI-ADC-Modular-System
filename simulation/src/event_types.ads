package Event_Types is
   type Event_Type is (Signal_Event, Offset_Event);

   type Priority_Level is
     new Integer range 1 .. 10;  -- 1 is lowest, 10 is highest

   type Base_Event is tagged record
      Event_Kind : Event_Type;
      Priority   : Priority_Level := 1;  -- Default to lowest priority
   end record;

   type Offset is new Base_Event with record
      Value : Float;
   end record;

   type Signal_Color is (Red, Yellow, Green);
   type Signal_State is new Base_Event with record
      Color : Signal_Color;
   end record;

   type Event_Access is access Base_Event'Class;
end Event_Types;

--  package Event_Types is
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

--     type Event_Access is access Base_Event'Class;
--  end Event_Types;
