with STM32.GPIO;
with STM32.Device;
with STM32.Board;
with Coms_Uart;
with Digital_Out;
with HAL; use HAL;


package body Drive_Motor is

   ----------------------------------------------------------------------------
   -- Initialize
   ----------------------------------------------------------------------------
   procedure Initialize
     (This           : in out Motor; 
      Timer          : not null access STM32.Timers.Timer := STM32.Device.Timer_4'Access;
      PWM_Pin        : STM32.GPIO.GPIO_Point := STM32.Device.PB7; 
      Channel        : STM32.Timers.Timer_Channel := STM32.Timers.Channel_2;
      GPIO_AF        : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM4_2;
      Frequency      : STM32.PWM.Hertz := 50;
      Max_Duty_Cycle : STM32.PWM.Microseconds := 2000;
      Min_Duty_Cycle : STM32.PWM.Microseconds := 1000)

   is
   begin
      -- Store the timer generator.
      This.Generator := Timer;

      -- Configure the timer for PWM.
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency);
      
      -- Attach the PWM channel.
      This.PWM_Mod.Attach_PWM_Channel (This.Generator, Channel, PWM_Pin, GPIO_AF);

      -- Store duty cycle range.
      This.Max_Duty_Cycle := Max_Duty_Cycle;
      This.Min_Duty_Cycle := Min_Duty_Cycle;

      -- Initialize the Power_Pin.
      Digital_Out.Initialize (This.Power_Pin, Digital_Out.Power_Pin);

      -- Disable PWM output initially.
      This.PWM_Mod.Disable_Output;
      Coms_Uart.Send_String_Newline("Motor Initialized.");
   end Initialize;

   ----------------------------------------------------------------------------
   -- Enable
   ----------------------------------------------------------------------------
   procedure Enable (This : in out Motor) is
   begin
      This.PWM_Mod.Enable_Output;

      -- Enable Mosfet for BLDC controller + motor power.
      Digital_Out.Enable (This.Power_Pin);
      Coms_Uart.Send_String_Newline("Motor Enabled (via Enable procedure).");

      Calibrate (This);
   end Enable;

   ----------------------------------------------------------------------------
   -- Disable
   ----------------------------------------------------------------------------
   procedure Disable (This : in out Motor) is
   begin
      -- Disable Mosfet for BLDC controller and motor power.
      Digital_Out.Disable (This.Power_Pin);

      This.PWM_Mod.Disable_Output;
      Coms_Uart.Send_String_Newline("Motor Disabled (via Disable procedure).");
   end Disable;

   ----------------------------------------------------------------------------
   -- Set_Frequency
   ----------------------------------------------------------------------------
   procedure Set_Frequency (This : in out Motor; Frequency : STM32.PWM.Hertz) is
   begin
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency);
   end Set_Frequency;

   ----------------------------------------------------------------------------
   -- Set_Duty_Cycle_Us
   ----------------------------------------------------------------------------
   procedure Set_Duty_Cycle_Us (This : in out Motor; Time_Us : STM32.PWM.Microseconds) is
   begin
      This.PWM_Mod.Set_Duty_Time (Time_Us);
   end Set_Duty_Cycle_Us;

   ----------------------------------------------------------------------------
   -- Set_Duty_Cycle_Percentage
   ----------------------------------------------------------------------------
   procedure Set_Duty_Cycle_Percentage
     (This : in out Motor; Percentage : STM32.PWM.Percentage) is
   begin
      This.PWM_Mod.Set_Duty_Cycle (Percentage);
   end Set_Duty_Cycle_Percentage;

   ----------------------------------------------------------------------------
   -- Calibrate
   ----------------------------------------------------------------------------
   procedure Calibrate (This : in out Motor) is
      Min_Percentage : constant Integer := 5;
      Max_Percentage : constant Integer := 10;
      Calibrate_Time : constant Duration := 0.5;

   begin
      Digital_Out.Enable (This.Power_Pin);

      if Digital_Out.Is_Enabled (This.Power_Pin) then
         Coms_Uart.Send_String_Newline("Motor Power On - Calibrating...");


         Coms_Uart.Send_String_Newline("Setting to max duty cycle...");
         This.Set_Duty_Cycle_Percentage (Max_Percentage);

         delay Calibrate_Time;

         Coms_Uart.Send_String_Newline("Setting to min duty cycle...");
         This.Set_Duty_Cycle_Percentage (Min_Percentage);

         delay Calibrate_Time;

         Coms_Uart.Send_String_Newline("Calibrated");
      else
         Coms_Uart.Send_String_Newline("Power OFF");
      end if;
   end Calibrate;

   ----------------------------------------------------------------------------
   -- Set_Speed
   ----------------------------------------------------------------------------
   procedure Set_Speed (This : in out Motor; Speed_Percentage : Integer) is
      Adjusted_Percentage : Integer;
      Adjusted_Duty       : STM32.PWM.Microseconds;
      Min_Percentage      : constant Integer := 5;
      Max_Percentage      : constant Integer := 100;
   begin
      -- Constrain Speed_Percentage to range
      Adjusted_Percentage := Integer'Max(Min_Percentage, Integer'Min(Max_Percentage, Speed_Percentage));

      -- Calculate duty cycle based on constrained percentage.
      Adjusted_Duty :=
      This.Min_Duty_Cycle +
      STM32.PWM.Microseconds
         ((Adjusted_Percentage - Min_Percentage) * (Integer (This.Max_Duty_Cycle) - Integer (This.Min_Duty_Cycle)) / (Max_Percentage - Min_Percentage));

      -- Apply the calculated duty cycle.
      Set_Duty_Cycle_Us (This, Adjusted_Duty);
   end Set_Speed;

   ----------------------------------------------------------------------------
   -- Stop
   ----------------------------------------------------------------------------
   procedure Stop (This : in out Motor) is
   begin
      Set_Duty_Cycle_Us (This, This.Min_Duty_Cycle); -- Set to minimum duty cycle (stopping point).
      Coms_Uart.Send_String_Newline("Motor Stop invoked (min duty).");
   end Stop;

   procedure Emergency_Stop (This : in out Motor) is
   begin
      -- Immediately set speed to 0
      Set_Speed (This, 0);
      delay 0.1;

      -- Disable Mosfet for BLDC controller and motor power.
      Digital_Out.Disable (This.Power_Pin);

      -- Disable PWM output.
      This.PWM_Mod.Disable_Output;

      Coms_Uart.Send_String_Newline("EMERGENCY STOP - Motor Power Cut!");
   end Emergency_Stop;
   
end Drive_Motor;
