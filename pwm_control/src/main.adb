with Motor_Driver;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with STM32.PWM;
with HAL; use HAL;

procedure Main is
   -- First selected configuration
   PWM_Pin  : Motor_Driver.Motor;
   My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_2;
   Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;
   Timer_Channel : STM32.Timers.Timer_Channel := STM32.Timers.Channel_1;
   Alt_Func : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM2_1;
   Motor_Frequency : STM32.PWM.Hertz := 50;
   -- Target_Duty_Cycle_Percentage : STM32.PWM.Percentage := 50;
   Target_Duty_Cucle_Us : STM32.PWM.Microseconds := 2000;


   -- Second selected configuration
   --  My_Motor  : Motor_Driver.Motor;
   --  My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_4;
   --  Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PB7;
   --  Timer_Channel : STM32.Timers.Timer_Channel := STM32.Timers.Channel_2;
   --  Alt_Func : STM32.GPIO_Alternate_Function := STM32.Device.GPIO_AF_TIM4_2;
   --  Motor_Frequency : STM32.PWM.Hertz := 50;
   -- Target_Duty_Cycle_Percentage : STM32.PWM.Percentage := 50;
   -- Target_Duty_Cucle_Us : STM32.PWM.Microseconds := 1500;

   -- Delay times
   delay_time : constant := 1.0;

begin

   PWM_Pin.Initialize
     (Timer   => My_Timer'Access, Pin => Motor_Pin,
      Channel => Timer_Channel,
      GPIO_AF => Alt_Func, Frequency => Motor_Frequency);

   PWM_Pin.Enable;


   PWM_Pin.Set_Duty_Cycle_Us (Target_Duty_Cucle_Us);

   loop
      delay delay_time;
   end loop;
end Main;
