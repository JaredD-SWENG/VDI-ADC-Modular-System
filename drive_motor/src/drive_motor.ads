with STM32.PWM;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;
with Digital_Out;
with Coms_Uart; -- Included UART package for debugging

package Drive_Motor is
   type Motor is limited private;

   -- Initialize the motor.
   procedure Initialize
     (This           : in out Motor; 
      Timer          : not null access STM32.Timers.Timer := STM32.Device.Timer_4'Access;
      PWM_Pin        : STM32.GPIO.GPIO_Point := STM32.Device.PB7; 
      Channel        : STM32.Timers.Timer_Channel := STM32.Timers.Channel_2;
      GPIO_AF        : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM4_2; 
      Frequency      : STM32.PWM.Hertz := 50;
      Max_Duty_Cycle : STM32.PWM.Microseconds := 2000;
      Min_Duty_Cycle : STM32.PWM.Microseconds := 1000);

   -- Low-level PWM operations:
   procedure Enable (This : in out Motor);
   procedure Disable (This : in out Motor);
   procedure Set_Frequency (This : in out Motor; Frequency : STM32.PWM.Hertz);
   procedure Set_Duty_Cycle_Us (This : in out Motor; Time_Us : STM32.PWM.Microseconds);
   procedure Set_Duty_Cycle_Percentage (This : in out Motor; Percentage : STM32.PWM.Percentage);

   -- High-level motor control:
   -- Adjust motor speed as a percentage of max power (0% to 100%).
   procedure Calibrate (This : in out Motor);
   procedure Set_Speed (This : in out Motor; Speed_Percentage : Integer);
   procedure Stop (This : in out Motor);

private

   type Motor is tagged limited record
      PWM_Mod       : STM32.PWM.PWM_Modulator;
      Generator     : access STM32.Timers.Timer;
      Max_Duty_Cycle : STM32.PWM.Microseconds;
      Min_Duty_Cycle : STM32.PWM.Microseconds;
      Power_Pin     : Digital_Out.Digital_Pin;
   end record;

end Drive_Motor;
