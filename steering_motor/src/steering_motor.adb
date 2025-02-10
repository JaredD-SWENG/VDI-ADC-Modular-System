with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;
-- with Ada.Text_IO; use Ada.Text_IO;  -- Uncomment for debugging output if needed

package body Steering_Motor is

   procedure Initialize
     (This : in out Steering; Timer : not null access STM32.Timers.Timer := STM32.Device.Timer_2'Access;
      Pin               :        STM32.GPIO.GPIO_Point := STM32.Device.PA5;
      Channel           : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
      GPIO_AF : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1;
      Frequency         :        STM32.PWM.Hertz               := 50;
      Center_Duty_Cycle :        STM32.PWM.Microseconds        := 1_500)
   is
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

   procedure Set_Frequency
     (This : in out Steering; Frequency : STM32.PWM.Hertz)
   is
   begin
      STM32.PWM.Configure_PWM_Timer (This.Generator, Frequency * 2);
   end Set_Frequency;

   procedure Set_Duty_Cycle_Us
     (This : in out Steering; Time_Us : STM32.PWM.Microseconds)
   is
   begin
      This.PWM_Mod.Set_Duty_Time (Time_Us / 2);
   end Set_Duty_Cycle_Us;

   procedure Set_Duty_Cycle_Percentage
     (This : in out Steering; Percentage : STM32.PWM.Percentage)
   is
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

   procedure Center (This : in out Steering) is
   begin
      This.Enable;
      Set_Duty_Cycle_Us (This, This.Default_Duty_Center);
      Delay 0.5;  -- Delay for half a second.
      This.Disable;
   end Center;

   procedure Steer_Left (This : in out Steering) is
   begin
      This.Enable;
      Set_Duty_Cycle_Us (This, This.Default_Duty_Center + This.Max_Angle_From_Center);
      Delay 0.5;  -- Delay for half a second.
      This.Disable;
   end Steer_Left;

   procedure Steer_Right (This : in out Steering) is
   begin
      This.Enable;
      Set_Duty_Cycle_Us (This, This.Default_Duty_Center - This.Max_Angle_From_Center);
      Delay 0.5;  -- Delay for half a second.
      This.Disable;
   end Steer_Right;

   function Angle_To_Duty
     (Self : Steering; Angle : Integer) return STM32.PWM.Microseconds
   is
      Center : constant Integer := Integer (Self.Default_Duty_Center);  -- 1500
      Diff      : constant Integer :=
        Integer (Self.Max_Angle_From_Center);  -- Difference from center to either extreme.
      Duty      : Integer;
   begin
      if Angle >= Self.Max_Angle then
         Duty := Center - Diff;  
      elsif Angle <= -Self.Max_Angle then
         Duty := Center + Diff;  
      else
         -- Interpolate: duty = center + (angle * diff) / max_angle
         Duty := Center + (-Angle * Diff) / Self.Max_Angle;
      end if;
      return STM32.PWM.Microseconds (Duty);
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
