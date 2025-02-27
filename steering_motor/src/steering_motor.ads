-- Steering_Motor.ads
with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;

package Steering_Motor is
   type Steering is limited private;   

   -- Initialize the steering system with proper 50Hz configuration
   procedure Initialize
     (This      : in out Steering; 
      Timer     : not null access STM32.Timers.Timer := STM32.Device.Timer_2'Access;
      Pin       : STM32.GPIO.GPIO_Point := STM32.Device.PA5; 
      Channel   : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
      GPIO_AF   : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1; 
      Frequency : STM32.PWM.Hertz := 50;  -- Fixed 50Hz operation
      Center_Duty_Cycle : STM32.PWM.Microseconds := 1500);

   -- PWM control operations
   procedure Enable (This : in out Steering);
   procedure Disable (This : in out Steering);
   procedure Set_Frequency (This : in out Steering; Frequency : STM32.PWM.Hertz);
   procedure Set_Duty_Cycle_Us (This : in out Steering; Time_Us : STM32.PWM.Microseconds);
   procedure Set_Duty_Cycle_Percentage (This : in out Steering; Percentage : STM32.PWM.Percentage);
   function Get_Angle(This: Steering) return Integer;

   -- Steering control operations
   procedure Set_Angle (This : in out Steering; Angle : Integer);  -- -30 to +30 degrees
   procedure Center    (This : in out Steering);
   procedure Steer_Left  (This : in out Steering);
   procedure Steer_Right (This : in out Steering);
   procedure Set_Relative_Angle(This : in out Steering; i : Integer);
   procedure Set_Scaled_Angle(This : in out Steering; offset : Float);
   procedure Smooth_Steering(This : in out Steering; target : Float; alpha: Float);

private
   type Steering is tagged limited record
      PWM_Mod           : STM32.PWM.PWM_Modulator;
      Generator         : access STM32.Timers.Timer;

      -- Center (in microseconds)
      Default_Duty_Center : STM32.PWM.Microseconds := 1500;

      -- Distance to extreme left and right (in microseconds)
      Max_Angle_From_Center : STM32.PWM.Microseconds := 500;

      Max_Angle : Integer := 45;

      Stored_Angle: Integer := 0;
      
   end record;

   function Angle_To_Duty (Self : Steering; Angle : Integer) return STM32.PWM.Microseconds;

end Steering_Motor;
