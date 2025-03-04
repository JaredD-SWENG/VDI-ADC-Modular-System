with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;

package Steering_Motor is
   type Steering is limited private;

   -- Initialize the steering Steering (configuration is entirely hardcoded).
   procedure Initialize
     (This      : in out Steering; 
      Timer     : not null access STM32.Timers.Timer := STM32.Device.Timer_2'Access;
      Pin       : STM32.GPIO.GPIO_Point := STM32.Device.PA5; 
      Channel   : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
      GPIO_AF   : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1; 
      Frequency : STM32.PWM.Hertz := 50;
      Center_Duty_Cycle : STM32.PWM.Microseconds := 1500);

   -- Low-level PWM operations:
   procedure Enable (This : in out Steering);
   procedure Disable (This : in out Steering);
   procedure Set_Frequency (This : in out Steering; Frequency : STM32.PWM.Hertz);
   procedure Set_Duty_Cycle_Us (This : in out Steering; Time_Us : STM32.PWM.Microseconds);
   procedure Set_Duty_Cycle_Percentage (This : in out Steering; Percentage : STM32.PWM.Percentage);

   -- High-level steering operations:
   -- Set the steering angle (in degrees); allowed range is -30 (left) to +30 (right).
   procedure Set_Angle (This : in out Steering; Angle : Integer);
   procedure Center    (This : in out Steering);
   procedure Steer_Left  (This : in out Steering);
   procedure Steer_Right (This : in out Steering);

private
   type Steering is tagged limited record
      PWM_Mod           : STM32.PWM.PWM_Modulator;
      Generator         : access STM32.Timers.Timer;

      -- Center (in microseconds)
      Default_Duty_Center : STM32.PWM.Microseconds := 1500;

      -- Distance to extreme left and right (in microseconds)
      Max_Angle_From_Center : STM32.PWM.Microseconds := 500;

      Max_Angle : Integer := 45;
      
   end record;

   -- Internal helper: converts an angle (in degrees) to a PWM duty cycle (in microseconds).
   function Angle_To_Duty (Self : Steering; Angle : Integer) return STM32.PWM.Microseconds;

end Steering_Motor;
