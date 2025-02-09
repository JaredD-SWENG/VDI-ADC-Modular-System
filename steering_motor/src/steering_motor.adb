-- Steering_Motor.adb
with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;
with STM32.Board;  -- For clock configuration

package body Steering_Motor is

   procedure Initialize
     (This : in out Steering;
      Timer : not null access STM32.Timers.Timer;
      Pin               : STM32.GPIO.GPIO_Point;
      Channel           : STM32.Timers.Timer_Channel;
      GPIO_AF           : STM32.GPIO_Alternate_Function;
      Frequency         : STM32.PWM.Hertz;
      Center_Duty_Cycle : STM32.PWM.Microseconds) 
   is
      Timer_Clock   : constant := 84_000_000;  -- APB1 timer clock (84 MHz)
      Prescaler_Val : constant UInt32 := (Timer_Clock / (Frequency * 20_000)) - 1;
   begin
      -- Store reference to timer
      This.Generator := Timer;

      -- Configure timer for 50Hz PWM
      This.Generator.Disable;
      This.Generator.Set_Prescaler(Prescaler_Val);
      This.Generator.Set_Counter_Mode(STM32.Timers.Up);
      This.Generator.Set_Autoreload(20_000 - 1);  -- 20ms period
      This.Generator.Enable;

      -- Configure PWM channel
      This.PWM_Mod.Attach_PWM_Channel
        (Generator   => This.Generator,
         Channel     => Channel,
         Point       => Pin,
         AF          => GPIO_AF,
         Polarity    => STM32.PWM.High);

      -- Set initial center position
      This.Default_Duty_Center := Center_Duty_Cycle;
      Set_Duty_Cycle_Us(This, Center_Duty_Cycle);
      Enable(This);
   end Initialize;

   procedure Set_Frequency
     (This : in out Steering; Frequency : STM32.PWM.Hertz) 
   is
      Timer_Clock   : constant := 84_000_000;  -- APB1 timer clock
      Prescaler_Val : constant UInt32 := (Timer_Clock / (Frequency * 20_000)) - 1;
   begin
      This.Generator.Disable;
      This.Generator.Set_Prescaler(Prescaler_Val);
      This.Generator.Enable;
   end Set_Frequency;

   procedure Set_Duty_Cycle_Us
     (This : in out Steering; Time_Us : STM32.PWM.Microseconds) 
   is
   begin
      -- Constrain duty cycle to valid range (1000µs-2000µs)
      This.PWM_Mod.Set_Duty_Time(STM32.PWM.Microseconds'Max(1000, 
        STM32.PWM.Microseconds'Min(2000, Time_Us)));
   end Set_Duty_Cycle_Us;

   procedure Set_Duty_Cycle_Percentage
     (This : in out Steering; Percentage : STM32.PWM.Percentage) 
   is
   begin
      This.PWM_Mod.Set_Duty_Cycle(Percentage);
   end Set_Duty_Cycle_Percentage;

   procedure Enable (This : in out Steering) is
   begin
      This.PWM_Mod.Enable_Output;
   end Enable;

   procedure Disable (This : in out Steering) is
   begin
      This.PWM_Mod.Disable_Output;
   end Disable;

   procedure Center (This : in out Steering) is
   begin
      Set_Duty_Cycle_Us(This, This.Default_Duty_Center);
   end Center;

   procedure Steer_Left (This : in out Steering) is
   begin
      Set_Duty_Cycle_Us(This, This.Default_Duty_Center + This.Max_Angle_From_Center);
   end Steer_Left;

   procedure Steer_Right (This : in out Steering) is
   begin
      Set_Duty_Cycle_Us(This, This.Default_Duty_Center - This.Max_Angle_From_Center);
   end Steer_Right;

   function Angle_To_Duty
     (Self : Steering; Angle : Integer) return STM32.PWM.Microseconds 
   is
      Scaled_Angle : constant Integer := 
        Integer'Max(-Self.Max_Angle, Integer'Min(Self.Max_Angle, Angle));
   begin
      return Self.Default_Duty_Center + 
        STM32.PWM.Microseconds(
          (Scaled_Angle * Integer(Self.Max_Angle_From_Center)) / Self.Max_Angle
        );
   end Angle_To_Duty;

   procedure Set_Angle (This : in out Steering; Angle : Integer) is
   begin
      Set_Duty_Cycle_Us(This, Angle_To_Duty(This, Angle));
   end Set_Angle;

end Steering_Motor;
