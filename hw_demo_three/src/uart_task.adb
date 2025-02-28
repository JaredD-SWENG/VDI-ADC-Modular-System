with Coms_Uart; use Coms_Uart;
with Command_Queue; use Command_Queue;
with Commands; use Commands;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body UART_Task is

   task body UART_Handler is
      Input_Buffer : String(1..50); -- Increased length to handle longer commands
      Last         : Natural;
   begin
      Send_String_Newline ("[UART_Task] Started concurrency.");

      loop
         Send_String ("CMD> ");
         Receive_Line(Input_Buffer, Last, Echo => True);

         if Last = 0 then
            -- Ignore empty input
            Send_String_Newline("No input received.");
            null;
         end if;

         declare
            Choice : constant String := Trim(Input_Buffer(1..Last), Ada.Strings.Both);
         begin
            Send_String_Newline("Processing Command => '" & Choice & "'");

            -- MOTOR COMMANDS
            if Choice'Length > 5 and then Choice(1..5) = "MOTOR" then
               declare
                  Subcommand : constant String := Trim(Choice(7..Choice'Last), Ada.Strings.Both);
               begin
                  if Subcommand = "CALIBRATE" then
                     Send_String_Newline("Sending CALIBRATE to Queue");
                     Command_Queue.Main_Queue.Put(Calibrate_Motor, (Speed => 0));
                  elsif Subcommand = "STOP" then
                     Send_String_Newline("Sending STOP to Queue");
                     Command_Queue.Main_Queue.Put(Motor_Stop, (Speed => 0));
                  elsif Index(Subcommand, "SPEED ") = 1 then
                     declare
                        Speed : constant Integer := Integer'Value(Subcommand(7..Subcommand'Last));
                     begin
                        Send_String_Newline("Sending SPEED " & Integer'Image(Speed) & " to Queue");
                        Command_Queue.Main_Queue.Put(Set_Motor_Speed, (Speed => Speed));
                     end;
                  else
                     Send_String_Newline("INVALID MOTOR COMMAND: " & Subcommand);
                  end if;
               end;

            -- STEERING COMMANDS
            elsif Choice'Length > 8 and then Choice(1..8) = "STEERING" then
               declare
                  Subcommand : constant String := Trim(Choice(10..Choice'Last), Ada.Strings.Both);
               begin
                  if Subcommand = "CENTER" then
                     Send_String_Newline("Sending CENTER to Queue");
                     Command_Queue.Main_Queue.Put(Center_Steering, (Speed => 0));
                  elsif Index(Subcommand, "ANGLE ") = 1 then
                     declare
                        Angle : constant Integer := Integer'Value(Subcommand(7..Subcommand'Last));
                     begin
                        Send_String_Newline("Sending ANGLE " & Integer'Image(Angle) & " to Queue");
                        Command_Queue.Main_Queue.Put(Set_Steering_Angle, (Speed => Angle));
                     end;
                  else
                     Send_String_Newline("INVALID STEERING COMMAND: " & Subcommand);
                  end if;
               end;

            -- EXIT AND STOP
            elsif Choice = "EXIT" then
               Send_String_Newline ("[UART_Task] Exiting concurrency.");
               Command_Queue.Main_Queue.Put(Exit_Command, (Speed => 0));
               exit;
            elsif Choice = "STOP" then
               Send_String_Newline("Sending EMERGENCY STOP to Queue");
               Command_Queue.Main_Queue.Put(Emergency_Stop, (Speed => 0));

            -- HELP MESSAGE
            elsif Choice = "HELP" then
               Send_String_Newline("Available Commands:");
               Send_String_Newline("MOTOR CALIBRATE | MOTOR STOP | MOTOR SPEED <value>");
               Send_String_Newline("STEERING CENTER | STEERING ANGLE <value>");
               Send_String_Newline("STOP | EXIT | HELP");

            -- DEMO COMMANDS
            elsif Choice = "DEMO" then
               Send_String_Newline("Running Output Demo...");
               Output_Demo;
               Input_Demo;
            elsif Choice = "TEST" then
               Send_String_Newline("TEST MODE ACTIVE...");
               Send_String_Newline("MOTOR SPEED 50");
               Send_String_Newline("STEERING ANGLE 30");
               Send_String_Newline("STEERING CENTER");
               Send_String_Newline("MOTOR SPEED 0");
               Send_String_Newline("EXIT");
               Command_Queue.Main_Queue.Put(Exit_Command, (Speed => 0));

            -- INVALID COMMAND HANDLING
            else
               Send_String_Newline("INVALID COMMAND: " & Choice);
            end if;
         end;
      end loop;
   end UART_Handler;

   procedure Start is
   begin
      Send_String_Newline("[UART_Task] Start procedure called.");
   end Start;

end UART_Task;
