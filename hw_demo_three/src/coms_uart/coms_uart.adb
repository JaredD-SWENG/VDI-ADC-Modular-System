with HAL;            use HAL;
with STM32.Device;   use STM32.Device;
with STM32.USARTs;   use STM32.USARTs;
with STM32.GPIO;     use STM32.GPIO;
with Interfaces;     use Interfaces;
with STM32_SVD.RCC;  use STM32_SVD.RCC;
with HAL.UART;       use HAL.UART;
with STM32.Board;    use STM32.Board;
with Drive_Motor;    use Drive_Motor;
with Commands;       use Commands;
with Steering_Motor;   use Steering_Motor;
with Command_Queue; use Command_Queue;

with Ada.Strings; 
with Ada.Strings.Fixed;

package body Coms_Uart is

      procedure Request_Exit is begin Exit_Flag := True; end;
      function Exit_Requested return Boolean is begin return Exit_Flag; end;



   USART1_TX : constant GPIO_Point := PA9;
   USART1_RX : constant GPIO_Point := PA10;

   procedure Configure_System_Clock_HSI_16MHz is
   begin
      RCC_Periph.CR.HSION := True;
      loop
         exit when RCC_Periph.CR.HSIRDY;
      end loop;

      RCC_Periph.CFGR.SW := 0;  -- 0 => HSI selected
      loop
         exit when RCC_Periph.CFGR.SWS = 0;
      end loop;

      RCC_Periph.CFGR.HPRE         := 0;
      RCC_Periph.CFGR.PPRE.Arr (1) := 0;
      RCC_Periph.CFGR.PPRE.Arr (2) := 0;
   end Configure_System_Clock_HSI_16MHz;

   procedure Flush_RX is
      Dummy : UInt9;
   begin
      while USART_1.Rx_Ready loop
         USART_1.Receive (Dummy);
      end loop;
   end Flush_RX;

   procedure Initialize_Coms_Uart is
   begin
      -- System clock
      Configure_System_Clock_HSI_16MHz;
      Enable_Clock (GPIO_A);
      Enable_Clock (USART_1);

      -- Pin setup
      Configure_IO
        (USART1_TX,
         (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating,
          AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));

      Configure_IO
        (USART1_RX,
         (Mode => Mode_AF, AF => GPIO_AF_USART1_7, Resistors => Floating,
          AF_Output_Type => Push_Pull, AF_Speed => Speed_50MHz));

      USART_1.Set_Baud_Rate (115_200);
      USART_1.Set_Word_Length (Word_Length_8);
      USART_1.Set_Stop_Bits (Stopbits_1);
      USART_1.Set_Parity (No_Parity);
      USART_1.Set_Mode (Tx_Rx_Mode);
      USART_1.Set_Flow_Control (No_Flow_Control);
      USART_1.Set_Oversampling_Mode (Oversampling_By_16);
      USART_1.Enable;

      Initialize_LEDs;
      Flush_RX;
   end Initialize_Coms_Uart;

   procedure Send_String (Data : String) is
      Buffer : UART_Data_8b (Data'Range);
      Status : UART_Status;
   begin
      for I in Data'Range loop
         Buffer (I) := UInt8 (Character'Pos (Data (I)));
      end loop;
      USART_1.Transmit (Buffer, Status, Timeout => 1_000);
   end Send_String;

   procedure Send_String_Newline (Data : String) is
   begin
      Send_String (Data & ASCII.CR & ASCII.LF);
   end Send_String_Newline;

   procedure Send_Newline is
   begin
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.CR)));
      USART_1.Transmit (UInt9 (Character'Pos (ASCII.LF)));
   end Send_Newline;

   procedure Receive_Line
     (Output : out String; 
      Last   : out Natural; 
      Echo   : Boolean := False)
   is
      Char_Count : Natural := 0;
      Received   : UInt9;
   begin
      Last := 0;
      loop
         -- Wait for a byte
         loop
            exit when USART_1.Rx_Ready;
         end loop;

         USART_1.Receive (Received);
         declare
            C : constant Character := Character'Val (Received);
         begin
            if Echo and then not (C in ASCII.LF | ASCII.CR) then
               USART_1.Transmit (Received);
            end if;

            if C in ASCII.CR | ASCII.LF then
               Flush_RX;
               Last := Char_Count;
               exit;
            else
               if Char_Count < Output'Length then
                  Char_Count := Char_Count + 1;
                  Output (Char_Count) := C;
               else
                  Last := Char_Count;
                  exit;
               end if;
            end if;
         end;
      end loop;
   end Receive_Line;

   procedure Clear_Screen is
   begin
      Send_String (ASCII.ESC & "[2J");
   end Clear_Screen;

   procedure Newline is
   begin
      Send_String (ASCII.CR & ASCII.LF);
   end Newline;

   procedure Send_Time_Span (Span : Ada.Real_Time.Time_Span) is
      use Ada.Real_Time;
      Seconds : Seconds_Count;
      Subspan : Time_Span;
   begin
      Split (Time_Of(0, Span), Seconds, Subspan);
      Send_String ("Time: " & Seconds_Count'Image(Seconds) & "s");
      Send_String (" " & Integer'Image(Integer(To_Duration(Subspan) * 1E9)) & "ns");
   end Send_Time_Span;

procedure Process_Command(Command : String) is
   use Ada.Strings, Ada.Strings.Fixed;
   CMD : constant String := Trim(Command, Both);
begin
   Send_String_Newline("Debug: Processing Command => " & CMD);


   if CMD'Length > 5 and then CMD(1..5) = "MOTOR" then
      Handle_Drive_Command(Trim(CMD(6..CMD'Last), Both));
   elsif CMD'Length > 8 and then CMD(1..8) = "STEERING" then
      Handle_Steering_Command(Trim(CMD(9..CMD'Last), Both));
   else
      Send_String_Newline ("Debug: Invalid Command");
   end if;
end Process_Command;

procedure Handle_Drive_Command(Cmd : String) is
   Space_Pos : constant Natural := Index(Cmd, " ");
begin
   Send_String_Newline ("Debug: Handle_Drive_Command received '" & Cmd & "'");

   if Cmd = "STOP" then
      Send_String_Newline ("Debug: Handle_Drive_Command Matched STOP");
      Main_Queue.Put(Cmd => Motor_Stop, Param => (Speed => 0));
   elsif Cmd = "CALIBRATE" then
      Send_String_Newline ("Debug: Handle_Drive_Command Matched CALIBRATE");
      Main_Queue.Put(Cmd => Calibrate_Motor, Param => (Speed => 0));
   elsif Cmd = "EMERGENCY" then
      Send_String_Newline ("Debug: Handle_Drive_Command Matched EMERGENCY");
      Main_Queue.Put(Cmd => Emergency_Stop, Param => (Speed => 0));
   elsif Cmd = "EXIT" then
      Send_String_Newline ("Debug: Handle_Drive_Command Matched EXIT");
      Main_Queue.Put(Cmd => Exit_Command, Param => (Speed => 0));
      Exit_Flag := True;
   elsif Space_Pos > 0 then
      declare
         Action : constant String := Cmd(Cmd'First..Space_Pos-1);
         Value  : constant Integer := Integer'Value(Cmd(Space_Pos+1..Cmd'Last));
      begin
         Send_String_Newline ("Debug: Handle_Drive_Command Parsed Action => '" & Action & "', Value => " & Integer'Image(Value));

         if Action = "SPEED" then
            Send_String_Newline ("Debug: Handle_Drive_Command Matched SPEED");
            Main_Queue.Put(Cmd => Set_Motor_Speed, Param => (Speed => Value));
         end if;
      end;
   else
      Send_String_Newline ("Debug: No Match in Handle_Drive_Command");
   end if;
end Handle_Drive_Command;


procedure Handle_Steering_Command(Cmd : String) is
   Space_Pos : constant Natural := Index(Cmd, " ");
begin
   Send_String_Newline("Debug: Extracting Steering Command: '" & Cmd & "'");

   if Cmd = "CENTER" then
      Send_String_Newline("Debug: Matched CENTER");
      Main_Queue.Put(Cmd => Center_Steering, Param => (Speed => 0));
   elsif Cmd = "EXIT" then
      Send_String_Newline("Debug: Matched EXIT");
      Main_Queue.Put(Cmd => Exit_Command, Param => (Speed => 0));
   elsif Space_Pos > 0 then
      declare
         Action : constant String := Cmd(Cmd'First..Space_Pos-1);
         Value  : constant Integer := Integer'Value(Trim(Cmd(Space_Pos+1..Cmd'Last), Ada.Strings.Both));
      begin
         Send_String_Newline("Debug: Parsed Action => '" & Action & "', Value => " & Integer'Image(Value));

         if Action = "ANGLE" then
            Send_String_Newline("Debug: Matched ANGLE");
            Main_Queue.Put(Cmd => Set_Steering_Angle, Param => (Speed => Value));
         else
            Send_String_Newline("Debug: Unknown Action '" & Action & "'");
         end if;
      end;
   else
      Send_String_Newline("Debug: No Match in Handle_Steering_Command");
   end if;
exception
   when others =>
      Send_String_Newline("INVALID STEERING COMMAND");
end Handle_Steering_Command;


   procedure Output_Demo is
      Demo_Message : constant String := "Hello VDI from Penn State!";
   begin
      Clear_Screen;
      Send_String_Newline ("UART Controller Ready");
      Send_String_Newline (Demo_Message);
   end Output_Demo;

   procedure Input_Demo is
      Input_Buffer : String (1 .. 80);
      Last_Char    : Natural;
   begin
      loop
         Send_String ("CMD> ");
         Receive_Line (Output => Input_Buffer, Last => Last_Char, Echo => True);
         Process_Command (Input_Buffer(1 .. Last_Char));
      end loop;
   end Input_Demo;

   ----------------------------------------------------------------------------
   -- Library-level Task: UART_Task
   -- Example: waits for commands in a loop, calls Input_Demo logic, etc.
   ----------------------------------------------------------------------------
   procedure Stop_UART_Task is
   begin
      Exit_Flag := True;
   end Stop_UART_Task;

   function Is_Exit_Requested return Boolean is
   begin
      return Exit_Flag;
   end Is_Exit_Requested;
   task body UART_Task is
      Input_Buffer : String (1 .. 80);
      Last_Char    : Natural;
   begin
      -- Example: an infinite loop reading user commands
      Send_String_Newline ("[UART_Task] Started concurrency.");

      loop
         exit when Exit_Flag;

         Send_String ("CMD> ");
         Receive_Line (Output => Input_Buffer, Last => Last_Char, Echo => True);

         if Exit_Flag then
            exit;
         end if;

         if Last_Char > 0 then
            Process_Command (Input_Buffer(1 .. Last_Char));
         else
            -- If user hits Enter with no input, do nothing or some default
            null;
         end if;
      end loop;

      Send_String_Newline ("[UART_Task] Exiting concurrency.");
   end UART_Task;

-- begin
   --  Initialize_Coms_Uart;

   --  Send_String_Newline("System Booting...");

   -- Clear_Screen;
end Coms_Uart;
