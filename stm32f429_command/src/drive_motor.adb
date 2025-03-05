with STM32.Device;   use STM32.Device;
with STM32.GPIO;     use STM32.GPIO;
with STM32.Board;    use STM32.Board;
with STM32.PWM;      use STM32.PWM;
with Ada.Real_Time;  use Ada.Real_Time;
with HAL;            use HAL;
with Digital_Out;    use Digital_Out;

package body Drive_Motor is

   ---------------------------
   -- Internal Declarations --
   ---------------------------
   Motor_Pin : Digital_Pin;  -- for power
   PWM_Mod   : STM32.PWM.PWM_Modulator;

   -- For calibrating & controlling the motor
   Generator      : access STM32.Timers.Timer := Timer_4'Access;
   Max_Duty_Cycle : STM32.PWM.Microseconds := 2000;
   Min_Duty_Cycle : STM32.PWM.Microseconds := 1000;
   Min_Percentage : constant Integer         := 5;
   Max_Percentage : constant Integer         := 100;

   ----------------------------------
   -- Public Procedure Definitions --
   ----------------------------------
   procedure Initialize is
   begin
      ----------------------
      -- Setup Timer/PWM --
      ----------------------
      Enable_Clock (USART_1);   -- Example: might or might not be needed
      STM32.PWM.Configure_PWM_Timer (Generator, 50);  -- 50 Hz default
      PWM_Mod.Attach_PWM_Channel
        (Generator,
         Channel    => STM32.Timers.Channel_2,
         Pin        => PB7,
         AF         => GPIO_AF_TIM4_2);

      ------------------
      -- Setup Output --
      ------------------
      Initialize (Motor_Pin, PC8);  -- or whichever you prefer
      PWM_Mod.Disable_Output;
      Motor_Enabled := False;

      -- Set speed & command to defaults
      Motor_Speed := 0;
      Motor_Command := No_Command;

      Send_Debug_String ("[Drive_Motor] Initialization complete.");
   end Initialize;

   procedure Set_Speed (Speed_Percentage : Integer) is
   begin
      -- Constrain Speed to [Min_Percentage..Max_Percentage]
      if Speed_Percentage < Min_Percentage then
         Speed_Percentage := Min_Percentage;
      elsif Speed_Percentage > Max_Percentage then
         Speed_Percentage := Max_Percentage;
      end if;

      Motor_Speed := Speed_Percentage;
      Motor_Command := No_Command;  -- Just normal operation
   end Set_Speed;

   procedure Stop is
   begin
      Motor_Command := Stop_Requested;
   end Stop;

   procedure Emergency_Stop is
   begin
      Motor_Command := Emergency_Stop_Requested;
   end Emergency_Stop;

   procedure Calibrate is
   begin
      Motor_Command := Calibrate_Requested;
   end Calibrate;

   function Is_Enabled return Boolean is
   begin
      return Motor_Enabled;
   end Is_Enabled;

   -----------------------------------
   -- Utility to Print Debug Strings --
   -----------------------------------
   procedure Send_Debug_String (Msg : String) is
   begin
      -- Replace with your debug method, e.g. Coms_Uart.Send_String_Newline(Msg);
      -- or do nothing if you prefer
      null;
   end Send_Debug_String;

   ----------------------
   -- Helper Routines --
   ----------------------
   procedure Apply_Speed (Speed_Pct : Integer) is
      Adjusted_Duty : STM32.PWM.Microseconds;
   begin
      Adjusted_Duty :=
         Min_Duty_Cycle
         + STM32.PWM.Microseconds
            ((Speed_Pct - Min_Percentage)
             * (Integer (Max_Duty_Cycle) - Integer (Min_Duty_Cycle))
             / (Max_Percentage - Min_Percentage));
      PWM_Mod.Set_Duty_Time (Adjusted_Duty);
   end Apply_Speed;

   procedure Perform_Calibration is
      Calibrate_Time : constant Duration := 0.5;
   begin
      Send_Debug_String ("[Drive_Motor] Calibrating...");
      -- e.g. set to max
      Apply_Speed (10);
      delay Calibrate_Time;

      -- Then set to min
      Apply_Speed (5);
      delay Calibrate_Time;

      Send_Debug_String ("[Drive_Motor] Calibration done.");
   end Perform_Calibration;

   ------------------------
   -- Library-level Task --
   ------------------------
   task body Motor_Task is
      Period    : constant Time_Span := Milliseconds(100);
      Next_Time : Time := Clock;
   begin
      -- At startup, assume user calls Drive_Motor.Initialize from outside
      -- to set up pins. We can't rely on that if the user forgets, but for now we do.

      -- Force power on initially, or wait for user to do so
      Digital_Out.Enable (Motor_Pin);
      PWM_Mod.Enable_Output;
      Motor_Enabled := True;

      -- Possibly do an initial calibration
      Perform_Calibration;

      loop
         -- In a normal loop, read current command
         case Motor_Command is
            when Calibrate_Requested =>
               Perform_Calibration;
               Motor_Command := No_Command;

            when Stop_Requested =>
               Apply_Speed (5);   -- min speed?
               Send_Debug_String ("[Drive_Motor] Stop command applied.");
               Motor_Command := No_Command;

            when Emergency_Stop_Requested =>
               -- Immediately power off
               Motor_Enabled := False;
               PWM_Mod.Disable_Output;
               Digital_Out.Disable (Motor_Pin);
               Send_Debug_String ("[Drive_Motor] EMERGENCY STOP!");
               -- remain off until user calls Initialize or something
               Motor_Command := No_Command;

            when No_Command =>
               -- Normal operation, apply Motor_Speed
               if Motor_Enabled then
                  Apply_Speed (Motor_Speed);
               end if;
         end case;

         -- For debug: Toggle an LED or do something
         STM32.Board.Toggle (Blue_LED);

         -- Delay for determinism
         Next_Time := Next_Time + Period;
         delay until Next_Time;
      end loop;
   end Motor_Task;

end Drive_Motor;
