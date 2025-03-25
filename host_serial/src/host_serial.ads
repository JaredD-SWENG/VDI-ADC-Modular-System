package Host_Serial is

   type Cmd is (center, go, left, right, stop);
   for Cmd use (center => Character'Pos('C'),
               go => Character'Pos('G'),
               left => Character'Pos('L'),
               right => Character'Pos('R'),
               stop => Character'Pos('S'));

   procedure Send_Command (C : Cmd);

end Host_Serial;