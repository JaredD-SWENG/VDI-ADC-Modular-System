with Coms_Uart;    use Coms_Uart;
with Drive_Motor;
with STM32.Device;
with STM32.PWM;
with STM32.Timers;
with HAL;          use HAL;

procedure Main is

   ---------------------------------------------------------------------------
   -- Motor instance for testing
   ---------------------------------------------------------------------------
   My_Motor : Drive_Motor.Motor;

   ---------------------------------------------------------------------------
   -- Track whether motor is enabled or disabled
   ---------------------------------------------------------------------------
   Motor_Enabled : Boolean := False;

   ---------------------------------------------------------------------------
   -- Command buffer for user input
   ---------------------------------------------------------------------------
   Command_Buffer : String (1 .. 50) := (others => ' ');
   Last_Char      : Natural         := 0;

   ---------------------------------------------------------------------------
   -- Helper procedure to print the main menu
   ---------------------------------------------------------------------------
   procedure Show_Menu is
   begin
      Send_String_Newline ("");
      Send_String_Newline ("========== UART Test Menu ==========");
      Send_String_Newline ("1) Enable Motor");
      Send_String_Newline ("2) Disable Motor");
      Send_String_Newline ("3) Set Motor Frequency  (Hz)");
      Send_String_Newline ("4) Set Duty Cycle       (%)");
      Send_String_Newline ("5) Set Duty Time        (us)");
      Send_String_Newline ("Q) Quit");
      Send_String      ("Enter choice: "); -- No newline, just a prompt
   end Show_Menu;

   ---------------------------------------------------------------------------
   -- Helper function to prompt user for numeric input and return as Integer
   -- If user input is invalid or blank, it returns 0 (or a default).
   ---------------------------------------------------------------------------
   function Get_Integer_Input (Prompt : String) return Integer is
      Local_Buffer : String (1 .. 50) := (others => ' ');
      Local_Last   : Natural          := 0;
   begin
      -- Ask user for the numeric value
      Send_String (Prompt);
      Receive_Line (Output => Local_Buffer,
                    Last   => Local_Last,
                    Echo   => True);

      if Local_Last = 0 then
         Send_String_Newline ("No input, returning 0 as default.");
         return 0;
      else
         -- Attempt to convert the typed string to an Integer
         declare
            Value_String : constant String := Local_Buffer (1 .. Local_Last);
            Value        : Integer;
         begin
            Value := Integer'Value (Value_String);
            return Value;
         exception
            when others =>
               Send_String_Newline ("Invalid numeric input. Returning 0.");
               return 0;
         end;
      end if;
   end Get_Integer_Input;

begin
   -- Clear screen (if terminal supports ANSI codes)
   Clear_Screen;

   -- Initialize the motor. By default, it's Disabled.
   Drive_Motor.Initialize (This           => My_Motor,
                           Timer          => STM32.Device.Timer_4'Access,
                           PWM_Pin        => STM32.Device.PB7,
                           Channel        => STM32.Timers.Channel_2,
                           GPIO_AF        => STM32.Device.GPIO_AF_TIM4_2,
                           Frequency      => 50,
                           Max_Duty_Cycle => 2000,
                           Min_Duty_Cycle => 1000);

   Send_String_Newline ("UART Test Menu with Motor Controls");
   Send_String_Newline ("Motor is initially DISABLED.");
   Send_String_Newline ("Type a number or 'Q' to interact.");

   ---------------------------------------------------------------------------
   -- Main menu loop
   ---------------------------------------------------------------------------
   loop
      Show_Menu;

      Receive_Line (Output => Command_Buffer,
                    Last   => Last_Char,
                    Echo   => True);

      if Last_Char = 0 then
         -- User just pressed ENTER with no input
         Send_String_Newline ("No menu selection received.");
      else
         declare
            User_Input : constant String := Command_Buffer (1 .. Last_Char);
         begin
            case User_Input (1) is

               when '1' =>
                  -- Enable Motor
                  Drive_Motor.Enable (My_Motor);
                  Motor_Enabled := True;
                  Send_String_Newline ("Motor Enabled.");

               when '2' =>
                  -- Disable Motor
                  Drive_Motor.Disable (My_Motor);
                  Motor_Enabled := False;
                  Send_String_Newline ("Motor Disabled.");

               when '3' =>
                  -- SET MOTOR FREQUENCY
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        New_Frequency : Integer := Get_Integer_Input ("Enter Frequency (Hz): ");
                     begin
                        if New_Frequency > 0 then
                           Drive_Motor.Set_Frequency
                             (This => My_Motor,
                              Frequency => STM32.PWM.Hertz (New_Frequency));
                           Send_String_Newline ("Frequency set to " & Integer'Image (New_Frequency) & " Hz.");
                        else
                           Send_String_Newline ("Frequency NOT changed (invalid input).");
                        end if;
                     end;
                  end if;

               when '4' =>
                  -- SET DUTY CYCLE PERCENTAGE
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        New_Percent : Integer := Get_Integer_Input ("Enter Duty Cycle % (5..100): ");
                     begin
                        if New_Percent >= 5 and then New_Percent <= 100 then
                           Drive_Motor.Set_Duty_Cycle_Percentage (My_Motor,
                             STM32.PWM.Percentage (New_Percent));
                           Send_String_Newline ("Duty Cycle set to " & Integer'Image (New_Percent) & "%.");
                        else
                           Send_String_Newline ("Invalid duty cycle. Must be between 5 and 100.");
                        end if;
                     end;
                  end if;

               when '5' =>
                  -- SET DUTY TIME (microseconds)
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        New_Us : Integer := Get_Integer_Input ("Enter Duty Time (us): ");
                     begin
                        if New_Us > 0 then
                           Drive_Motor.Set_Duty_Cycle_Us (My_Motor,
                             STM32.PWM.Microseconds (New_Us));
                           Send_String_Newline ("Duty Time set to " & Integer'Image (New_Us) & " us.");
                        else
                           Send_String_Newline ("Duty Time NOT changed (invalid input).");
                        end if;
                     end;
                  end if;

               when 'Q' | 'q' =>
                  Send_String_Newline ("Exiting menu. Goodbye!");
                  exit; -- Leave the loop

               when others =>
                  Send_String_Newline ("Invalid selection: " & User_Input);
            end case;
         end;
      end if;
   end loop;
end Main;
