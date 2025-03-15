with Coms_Uart;    use Coms_Uart;
with Drive_Motor;
with HAL;          use HAL;
with STM32.Device;
with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;

procedure Main is

   My_Motor      : Drive_Motor.Motor;
   Motor_Enabled : Boolean := False;

   Command_Buffer : String (1 .. 50) := (others => ' ');
   Last_Char      : Natural         := 0;

   ---------------------------------------------------------------------------
   -- Print the main menu
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
      Send_String_Newline ("6) Set Speed (Percentage)");
      Send_String_Newline ("7) Normal Stop  (Min Duty)");
      Send_String_Newline ("8) Emergency Stop (Full Cut!)");
      Send_String_Newline ("Q) Quit");
      Send_String      ("Enter choice: "); -- Prompt
   end Show_Menu;

   ---------------------------------------------------------------------------
   -- Helper to prompt for an Integer
   ---------------------------------------------------------------------------
   function Get_Integer_Input (Prompt : String) return Integer is
      Local_Buffer : String (1 .. 50) := (others => ' ');
      Local_Last   : Natural          := 0;
   begin
      Send_String (Prompt);
      Receive_Line (Output => Local_Buffer,
                    Last   => Local_Last,
                    Echo   => True);

      if Local_Last = 0 then
         Send_String_Newline ("No input, returning 0 as default.");
         return 0;
      else
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
   -- Clear screen
   Clear_Screen;

   -- Initialize the motor (disabled by default)
   Drive_Motor.Initialize
     (This           => My_Motor,
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

   loop
      Show_Menu;

      Receive_Line (Output => Command_Buffer,
                    Last   => Last_Char,
                    Echo   => True);

      if Last_Char = 0 then
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

               when '2' =>
                  -- Disable Motor
                  Drive_Motor.Disable (My_Motor);
                  Motor_Enabled := False;

               when '3' =>
                  -- Set Frequency
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        Freq : Integer := Get_Integer_Input ("Enter Frequency (Hz): ");
                     begin
                        if Freq > 0 then
                           Drive_Motor.Set_Frequency (My_Motor,
                             STM32.PWM.Hertz (Freq));
                           Send_String_Newline ("Frequency set to " & Integer'Image (Freq) & " Hz.");
                        else
                           Send_String_Newline ("Invalid frequency!");
                        end if;
                     end;
                  end if;

               when '4' =>
                  -- Set Duty Cycle %
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        DC : Integer := Get_Integer_Input ("Enter Duty Cycle % (5..100): ");
                     begin
                        if DC >= 5 and then DC <= 100 then
                           Drive_Motor.Set_Duty_Cycle_Percentage (My_Motor,
                             STM32.PWM.Percentage (DC));
                           Send_String_Newline ("Duty Cycle set to " & Integer'Image (DC) & "%.");
                        else
                           Send_String_Newline ("Out of range (5..100) or invalid.");
                        end if;
                     end;
                  end if;

               when '5' =>
                  -- Set Duty Time (microseconds)
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        Us : Integer := Get_Integer_Input ("Enter Duty Time (us): ");
                     begin
                        if Us > 0 then
                           Drive_Motor.Set_Duty_Cycle_Us (My_Motor,
                             STM32.PWM.Microseconds (Us));
                           Send_String_Newline ("Duty Time set to " & Integer'Image (Us) & " us.");
                        else
                           Send_String_Newline ("Invalid duty time!");
                        end if;
                     end;
                  end if;

               when '6' =>
                  -- Set Speed (Percentage)
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED. Enable first!");
                  else
                     declare
                        Spd : Integer := Get_Integer_Input ("Enter Speed % (5..100): ");
                     begin
                        Drive_Motor.Set_Speed (My_Motor, Spd);
                     end;
                  end if;

               when '7' =>
                  -- Normal Stop
                  if not Motor_Enabled then
                     Send_String_Newline ("Motor is DISABLED; already stopped.");
                  else
                     Drive_Motor.Stop (My_Motor);
                     Send_String_Newline ("Motor Stop invoked.");
                  end if;

               when '8' =>
                  -- Emergency Stop
                  -- We allow this any time (whether enabled or disabled).
                  Drive_Motor.Emergency_Stop (My_Motor);
                  Motor_Enabled := False;  -- definitely disabled now

               when 'Q' | 'q' =>
                  Send_String_Newline ("Exiting menu. Goodbye!");
                  exit;

               when others =>
                  Send_String_Newline ("Invalid selection: " & User_Input);
            end case;
         end;
      end if;
   end loop;
end Main;
