with STM32.PWM;
with STM32.Timers;

package Motor_Base is
   type Abstract_Motor is abstract tagged limited private;

   -- Set PWM frequency (Hz)
   procedure Set_Frequency (This : in out Abstract_Motor; Frequency : STM32.PWM.Hertz) is abstract;

   -- Set duty cycle in microseconds
   procedure Set_Duty_Cycle_Us (This : in out Abstract_Motor; Time_Us : STM32.PWM.Microseconds) is abstract;

   -- Set duty cycle as a percentage (0-100%)
   procedure Set_Duty_Cycle_Percentage (This : in out Abstract_Motor; Percentage : STM32.PWM.Percentage) is abstract;

   -- Enable PWM output
   procedure Enable (This : in out Abstract_Motor) is abstract;

   -- Disable PWM output
   procedure Disable (This : in out Abstract_Motor) is abstract;

private
   type Abstract_Motor is abstract tagged limited null record;
end Motor_Base;
