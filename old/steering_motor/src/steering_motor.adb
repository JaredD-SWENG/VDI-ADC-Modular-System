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
      This.Enable;
      This.Stored_Angle := Angle;
      Set_Duty_Cycle_Us (This, Angle_To_Duty (This, Angle));
      delay 0.05;  -- Delay for half a second.
      This.Disable;
   end Set_Angle;

   --retrieve the stored angle value
   function Get_Angle(This: Steering) return Integer is
   begin
      return This.Stored_Angle;
   end Get_Angle;

   --change angle based on current position (not absolute position)
   procedure Set_Relative_Angle(This : in out Steering; i : Integer) is 
   begin 
      This.Enable;
      This.Stored_Angle := This.Stored_Angle + i;
      Set_Angle (This , This.Stored_Angle);
      This.Disable;
   end Set_Relative_Angle;

   --change angle based on a -1.0 to 1.0 input based on maximum angle position
   procedure Set_Scaled_Angle(This : in out Steering; offset : Float) is 
   begin
      This.Enable;
         if offset >= -1.0 and offset <= 1.0 then
            Set_Angle (This , Integer(Float(This.Max_Angle)*offset));
         end if;
      This.Disable;
   end Set_Scaled_Angle;

   procedure Smooth_Steering(This : in out Steering; target : Float; alpha: Float) is
      Current_Angle: Float := Float(This.Stored_Angle);
      New_Angle: Float;
   begin
      loop
         exit when abs(Current_Angle - target) < 0.5;
         New_Angle := (1.0 - alpha) * Current_Angle + alpha * target;
         This.Set_Angle (Integer(New_Angle));
         Current_Angle := New_Angle;
      end loop;
   end Smooth_Steering;

end Steering_Motor;
