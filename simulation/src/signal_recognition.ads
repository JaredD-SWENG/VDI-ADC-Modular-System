with Event_Types; use Event_Types;

package Signal_Recognition is
   Event_Priority : Event_Types.Priority_Level;

   task Signal_Recognition_Task is
      entry Start (Priority : Event_Types.Priority_Level := 1);
   end Signal_Recognition_Task;
end Signal_Recognition;
