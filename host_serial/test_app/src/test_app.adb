with Host_Serial; use Host_Serial;

procedure Test_App is
begin
   Send_Command (stop);
   delay 3.0;
   Send_Command (go);
   delay 3.0;
   Send_Command (stop);
end Test_App;
