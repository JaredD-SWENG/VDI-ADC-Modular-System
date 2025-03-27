with Event_Types; use Event_Types;

package Lane_Detection is
   Event_Priority : Event_Types.Priority_Level;
   
   task Lane_Detection_Task is
      entry Start (Priority : Event_Types.Priority_Level := 1);
   end Lane_Detection_Task;
end Lane_Detection;
