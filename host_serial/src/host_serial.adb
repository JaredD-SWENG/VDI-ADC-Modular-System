with Ada.Streams;                use Ada.Streams;
with GNAT.IO;                    use GNAT.IO;
with GNAT.Serial_Communications; use GNAT.Serial_Communications;

package body Host_Serial is

   procedure Send_Command (C : Cmd) is
      COM  : aliased Serial_Port;
      COM6 : constant Port_Name := Name (6);
      Cmd_Data : Stream_Element_Array(1 .. 1) := [Cmd'Enum_Rep(C)];
   begin
      delay 0.01;
      Open (COM, COM6);
   
      Set (COM, B115200, CS8, One, Even);
   
      Write (COM, Cmd_Data);
   
      Close (COM);  
   end Send_Command;

end Host_Serial;

--  procedure Test_App is
--     COM  : aliased Serial_Port;
--     COM3 : constant Port_Name := Name (3);

--     Outgoing : String (1 .. 1024); -- arbitrary
--     Last     : Natural;
--  begin
--     COM.Open (COM3);
--     COM.Set (Rate => B115200, Block => False);

--     loop
--        Put ("> ");
--        Get_Line (Outgoing, Last);
--        exit when Last = Outgoing'First - 1;

--        Put_Line ("Sending: '" & Outgoing (1 .. Last) & "'");

--        String'Output (COM'Access, Outgoing (1 .. Last));

--        declare
--           Incoming : constant String := String'Input (COM'Access);
--        begin
--           Put_Line ("From board: " & Incoming);
--        end;
--     end loop;

--     COM.Close;
--  end Test_App;
