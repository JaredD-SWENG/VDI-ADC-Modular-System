with LCD_Std_Out;
with STM32.GPIO;
with STM32.Device;
with STM32.Board;
with HAL.Framebuffer;

package body Drive_Motor is

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
   end Initialize;

   procedure Enable (This : in out Motor) is
   begin
      This.PWM_Mod.Enable_Output;

      -- Enable Mosfet for BLDC controller and motor power.
      Digital_Out.Enable (This.Power_Pin);

      Calibrate (This);
   end Enable;

   procedure Disable (This : in out Motor) is
   begin
      -- Disable Mosfet for BLDC controller and motor power.
      Digital_Out.Disable (This.Power_Pin);

      This.PWM_Mod.Disable_Output;
   end Disable;

   procedure Set_Frequency (This : in out Motor; Frequency : STM32.PWM.Hertz) is
   begin
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency);
   end Set_Frequency;

   procedure Set_Duty_Cycle_Us (This : in out Motor; Time_Us : STM32.PWM.Microseconds) is
   begin
      This.PWM_Mod.Set_Duty_Time (Time_Us);
   end Set_Duty_Cycle_Us;

   procedure Set_Duty_Cycle_Percentage
     (This : in out Motor; Percentage : STM32.PWM.Percentage) is
   begin
      This.PWM_Mod.Set_Duty_Cycle (Percentage);
   end Set_Duty_Cycle_Percentage;

   procedure Calibrate (This : in out Motor) is
   begin
      Digital_Out.Disable (This.Power_Pin); -- Turn off power to motor.

      Delay 3.0;  -- Wait a short delay to ensure proper power-down

      Digital_Out.Enable (This.Power_Pin);  -- Power on the motor.

      -- Calibrate the motor.
      if Digital_Out.Is_Enabled (This.Power_Pin) then
         LCD_Std_Out.Put (10,150,"               ");
         LCD_Std_Out.Put (10,120,"Power On");
      
         Delay 0.5;  -- Wait a short delay to ensure proper power-up before calibration begins.
         
         LCD_Std_Out.Put (10,150,"               ");
         LCD_Std_Out.Put (10,150,"Calibrating...");
         This.Set_Duty_Cycle_Percentage (10);

         delay 3.0;

         This.Set_Duty_Cycle_Percentage (5);

         delay 5.0;
         LCD_Std_Out.Put (10,150,"               ");
         LCD_Std_Out.Put (10,150,"Calibrated");
      else
         LCD_Std_Out.Put (10,150,"               ");
         LCD_Std_Out.Put (10,150,"Power OFF");
      end if;

   end Calibrate;

   procedure Set_Speed (This : in out Motor; Speed_Percentage : Integer) is
      Adjusted_Percentage : Integer;
      Adjusted_Duty       : STM32.PWM.Microseconds;
      Min_Percentage      : constant Integer := 5;
      Max_Percentage      : constant Integer := 100;
   begin
      -- Constrain Speed_Percentage to range [5%, 10%].
      Adjusted_Percentage := Integer'Max(Min_Percentage, Integer'Min(Max_Percentage, Speed_Percentage));

      -- Calculate duty cycle based on constrained percentage.
      Adjusted_Duty :=
      This.Min_Duty_Cycle +
      STM32.PWM.Microseconds
         ((Adjusted_Percentage - Min_Percentage) * (Integer (This.Max_Duty_Cycle) - Integer (This.Min_Duty_Cycle)) / (Max_Percentage - Min_Percentage));

      -- Apply the calculated duty cycle.
      Set_Duty_Cycle_Us (This, Adjusted_Duty);
   end Set_Speed;

   procedure Stop (This : in out Motor) is
   begin
      Set_Duty_Cycle_Us (This, This.Min_Duty_Cycle); -- Set to minimum duty cycle (stopping point).
   end Stop;

end Drive_Motor;
