package body Motor_Driver is

      -- Initialize the motor with a specific timer, GPIO pin, and alternate function
   procedure Initialize
     (This : in out Motor;
      Timer : not null access STM32.Timers.Timer;
      Pin : STM32.GPIO.GPIO_Point;
      Channel : STM32.Timers.Timer_Channel;
      GPIO_AF : STM32.GPIO_Alternate_Function;
      Frequency : STM32.PWM.Hertz) is
   begin
      -- Store timer reference
      This.Generator := Timer;

      -- Configure PWM timer with user-defined frequency
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency * 2);

      -- Attach the PWM channel using user-defined GPIO_AF
      This.PWM_Mod.Attach_PWM_Channel
        (Generator => This.Generator,
         Channel   => Channel,
         Point     => Pin,
         PWM_AF    => GPIO_AF);

      -- Initially disable output
      This.PWM_Mod.Disable_Output;
   end Initialize;

   -- Set PWM frequency
   overriding procedure Set_Frequency (This : in out Motor; Frequency : STM32.PWM.Hertz) is
   begin
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency * 2);
   end Set_Frequency;

   -- Set duty cycle in microseconds
   overriding procedure Set_Duty_Cycle_Us (This : in out Motor; Time_Us : STM32.PWM.Microseconds) is
   begin
      This.PWM_Mod.Set_Duty_Time (Time_Us / 2);
   end Set_Duty_Cycle_Us;

   -- Set duty cycle in percentage
   overriding procedure Set_Duty_Cycle_Percentage (This : in out Motor; Percentage : STM32.PWM.Percentage) is
   begin
      This.PWM_Mod.Set_Duty_Cycle (Percentage);
   end Set_Duty_Cycle_Percentage;

   -- Enable PWM output
   overriding procedure Enable (This : in out Motor) is
   begin
      This.PWM_Mod.Enable_Output;
   end Enable;

   -- Disable PWM output
   overriding procedure Disable (This : in out Motor) is
   begin
      This.PWM_Mod.Disable_Output;
   end Disable;

end Motor_Driver;
