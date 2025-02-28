with Commands;
with Coms_Uart;
package Command_Queue is
   -- Define named array types
   type Command_Array is array (Positive range <>) of Commands.Command_Type;
   type Param_Array is array (Positive range <>) of Commands.Command_Param;

   protected type Queue is
      entry Put(Cmd : Commands.Command_Type; Param : Commands.Command_Param);
      entry Get(Cmd : out Commands.Command_Type; Param : out Commands.Command_Param);
   private
      -- Use named array types
      Cmd_Buffer  : Command_Array(1..10);
      Param_Buffer : Param_Array(1..10);
      Head : Positive := 1;
      Tail : Positive := 1;
      Count : Natural := 0;
   end Queue;

   Main_Queue : aliased Queue;
end Command_Queue;

