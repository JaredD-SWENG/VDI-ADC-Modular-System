with Motor_Driver;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with STM32.PWM;
with HAL; use HAL;
with Ada.Numerics.Float_Random;

use HAL;
use Ada.Numerics.Float_Random;

procedure Main is
   My_Motor  : Motor_Driver.Motor;
   My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_2;
   Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;
   Timer_Channel : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
   Alt_Func : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1;
   Motor_Frequency : STM32.PWM.Hertz := 50;

   --  My_Motor  : Motor_Driver.Motor;
   --  My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_4;
   --  Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PB7;
   --  Timer_Channel : STM32.Timers.Timer_Channel := STM32.Timers.Channel_2;
   --  Alt_Func : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM4_2;
   --  Motor_Frequency : STM32.PWM.Hertz := 50;

   -- Delay times
   delay_time : constant := 1.0;

   -- Steering range limits
   Min_Duty_Cycle_Us    : constant := 500;   -- Leftmost position
   Max_Duty_Cycle_Us    : constant := 2_500;  -- Rightmost position
   Center_Duty_Cycle_Us : constant := 1_500;

   -- Random number generator
   Gen                  : Generator;
   Rand_Pos             : Float;
   Random_Duty_Cycle_Us : UInt32;
begin
   -- Initialize random generator
   Reset (Gen);

   -- Initialize motor
   My_Motor.Initialize
     (Timer   => My_Timer'Access, Pin => Motor_Pin,
      Channel => Timer_Channel,
      GPIO_AF => Alt_Func, Frequency => Motor_Frequency);

   My_Motor.Enable;

   loop
      -- Generate a random number in the range [Min_Duty_Cycle_Us, Max_Duty_Cycle_Us]
      Rand_Pos             := Random (Gen);
      Random_Duty_Cycle_Us :=
        Min_Duty_Cycle_Us +
        UInt32 (Rand_Pos * Float (Max_Duty_Cycle_Us - Min_Duty_Cycle_Us));

      --  -- Move to a random position
      --  My_Motor.Set_Duty_Cycle_Us (Random_Duty_Cycle_Us);
      --  delay delay_time;

      -- Move back to center
      My_Motor.Set_Duty_Cycle_Us (Center_Duty_Cycle_Us);
      delay delay_time;
   end loop;
end Main;
