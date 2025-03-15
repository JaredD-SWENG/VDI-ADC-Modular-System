with Motor_Base;
with STM32.PWM;    -- ✅ Added for GPIO_Alternate_Function
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;

package Motor_Driver is
   type Motor is new Motor_Base.Abstract_Motor with private;

procedure Initialize
     (This : in out Motor;
      Timer : not null access STM32.Timers.Timer;
      Pin : STM32.GPIO.GPIO_Point;
      Channel : STM32.Timers.Timer_Channel;
      GPIO_AF : STM32.GPIO_Alternate_Function;
      Frequency : STM32.PWM.Hertz);

   -- Override abstract procedures
   overriding procedure Set_Frequency (This : in out Motor; Frequency : STM32.PWM.Hertz);
   overriding procedure Set_Duty_Cycle_Us (This : in out Motor; Time_Us : STM32.PWM.Microseconds);
   overriding procedure Set_Duty_Cycle_Percentage (This : in out Motor; Percentage : STM32.PWM.Percentage);
   overriding procedure Enable (This : in out Motor);
   overriding procedure Disable (This : in out Motor);

private
   type Motor is new Motor_Base.Abstract_Motor with record
      PWM_Mod : STM32.PWM.PWM_Modulator;
      Generator : access STM32.Timers.Timer;
   end record;
end Motor_Driver;
