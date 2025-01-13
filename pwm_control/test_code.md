## Random Steering

```Ada
    with Motor_Driver;
    with STM32.Timers;
    with STM32.GPIO;
    with STM32.Device;
    with HAL;
    with Ada.Numerics.Float_Random;

    use HAL;
    use Ada.Numerics.Float_Random;

    procedure Main is
    My_Motor  : Motor_Driver.Motor;
    My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_2;
    Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;

    -- Delay times
    delay_time : constant := 1.0;

    -- Steering range limits
    Min_Duty_Cycle_Us    : constant := 500;   -- Leftmost position
    Max_Duty_Cycle_Us    : constant := 2500;  -- Rightmost position
    Center_Duty_Cycle_Us : constant := 1500;

    -- Random number generator
    Gen : Generator;
    Rand_Pos : Float;
    Random_Duty_Cycle_Us : UInt32;
    begin
    -- Initialize random generator
    Reset(Gen);

    -- Initialize motor
    My_Motor.Initialize
        (Timer     => My_Timer'Access,
        Pin       => Motor_Pin,
        Channel   => STM32.Timers.Channel_1,
        GPIO_AF   => STM32.Device.GPIO_AF_TIM2_1,
        Frequency => 50);

    My_Motor.Enable;

    loop
        -- Generate a random number in the range [Min_Duty_Cycle_Us, Max_Duty_Cycle_Us]
        Rand_Pos := Random(Gen);
        Random_Duty_Cycle_Us := Min_Duty_Cycle_Us +
            UInt32 (Rand_Pos * Float (Max_Duty_Cycle_Us - Min_Duty_Cycle_Us));

        -- Move to a random position
        My_Motor.Set_Duty_Cycle_Us(Random_Duty_Cycle_Us);
        Delay delay_time;

        -- Move back to center
        My_Motor.Set_Duty_Cycle_Us(Center_Duty_Cycle_Us);
        Delay delay_time;
    end loop;
    end Main;

```

## Limit and range testing - steering

```Ada
with Motor_Driver;
with STM32.Timers;
with STM32.GPIO;
with STM32.Device;
with HAL; use HAL;

procedure Main is
   My_Motor  : Motor_Driver.Motor;
   My_Timer  : STM32.Timers.Timer renames STM32.Device.Timer_2;
   Motor_Pin : STM32.GPIO.GPIO_Point renames STM32.Device.PA5;

   Working_Time : HAL.UInt32 := 0;
   Top_Time  : constant := 2000;
   Bottom_Time : constant := 1000;
   Step_Time : constant := 100;
   delay_time : constant := 3.0;

begin

   My_Motor.Initialize
     (Timer     => My_Timer'Access,
      Pin       => Motor_Pin,
      Channel   => STM32.Timers.Channel_1,
      GPIO_AF   => STM32.Device.GPIO_AF_TIM2_1,
      Frequency => 50);

   My_Motor.Enable;

   -- Left most position
   My_Motor.Set_Duty_Cycle_Us(500);
   Delay delay_time;

   -- Right most position
   My_Motor.Set_Duty_Cycle_Us(2500);
   Delay delay_time;

   -- Center position
   My_Motor.Set_Duty_Cycle_Us(1500);
   Delay delay_time;

loop
   Working_Time := Bottom_Time;

   while Working_Time < Top_Time loop
      My_Motor.Set_Duty_Cycle_Us(Working_Time);
      
      Delay delay_time;

      Working_Time := Working_Time + Step_Time;
   end loop;

   Working_Time := Top_Time;

   while Working_Time > Bottom_Time loop
      My_Motor.Set_Duty_Cycle_Us(Working_Time);
      
      Delay delay_time;

      Working_Time := Working_Time - Step_Time;
   end loop;
   
end loop;

end Main;
```