with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;
-- with Ada.Text_IO; use Ada.Text_IO;  -- Uncomment for debugging output if needed

package body Steering_Motor is

   procedure Initialize
     (This      : in out Steering; 
      Timer     : not null access STM32.Timers.Timer;
      Pin       : STM32.GPIO.GPIO_Point := STM32.Device.PA5; 
      Channel   : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
      GPIO_AF   : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1; 
      Frequency : STM32.PWM.Hertz := 50;
      Center_Duty_Cycle : STM32.PWM.Microseconds := 1500) is
   begin
      -- Store the timer generator.
      This.Generator := Timer;

      -- Set up the timer generator with the desired frequency.
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency * 2);

      -- Attach the PWM channel to the timer generator.
      This.PWM_Mod.Attach_PWM_Channel (This.Generator, Channel, Pin, GPIO_AF);

      -- Initially disable the PWM output.
      This.PWM_Mod.Disable_Output;
   end Initialize;

   procedure Set_Frequency (This : in out Steering; Frequency : STM32.PWM.Hertz) is
   begin
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency * 2);
   end Set_Frequency;

   procedure Set_Duty_Cycle_Us (This : in out Steering; Time_Us : STM32.PWM.Microseconds) is
   begin
      This.PWM_Mod.Set_Duty_Time (Time_Us / 2);
   end Set_Duty_Cycle_Us;

   procedure Set_Duty_Cycle_Percentage (This : in out Steering; Percentage : STM32.PWM.Percentage) is
   begin
      This.PWM_Mod.Set_Duty_Cycle (Percentage);
   end Set_Duty_Cycle_Percentage;

   procedure Enable (This : in out Steering) is
   begin
      This.PWM_Mod.Enable_Output;
   end Enable;

   procedure Disable (This : in out Steering) is
   begin
      This.PWM_Mod.Disable_Output;
   end Disable;

end Steering_Motor;
