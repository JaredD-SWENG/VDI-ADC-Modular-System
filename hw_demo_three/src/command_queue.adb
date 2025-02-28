

package body Command_Queue is
   protected body Queue is
      entry Put(Cmd : Commands.Command_Type; Param : Commands.Command_Param) when Count < Cmd_Buffer'Length is
      begin
         Cmd_Buffer(Tail) := Cmd;
         Param_Buffer(Tail) := Param;
         Tail := (Tail mod Cmd_Buffer'Length) + 1;
         Count := Count + 1;
      end Put;

      entry Get(Cmd : out Commands.Command_Type; Param : out Commands.Command_Param) when Count > 0 is
      begin
         Cmd := Cmd_Buffer(Head);
         Param := Param_Buffer(Head);
         Head := (Head mod Cmd_Buffer'Length) + 1;
         Count := Count - 1;
      end Get;

      function Is_Empty return Boolean is (Count = 0);
   end Queue;
end Command_Queue;
