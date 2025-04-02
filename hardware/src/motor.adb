with Ada.Real_Time; use Ada.Real_Time;

package body Motor is

   Period : constant Time_Span := Milliseconds (System_Config.Motor_Period);
   Drive_Motor_Modulator : STM32.PWM.PWM_Modulator;

   protected type Motor_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
   private
      Speed : Integer := 0;
   end Motor_Data_T;

   --  PROTECTED TYPE (Motor_Data_T)
   --  This is a protected type that encapsulates the speed of the motor.
   --  It provides methods to set and get the speed, ensuring that access
   --  to the speed variable is thread-safe.
   --  The protected type is instantiated as a single object (Drive_Motor) to
   protected body Motor_Data_T is
      procedure Set_Speed (S : Integer) is
      begin
         Speed := S;
      end Set_Speed;

      function Get_Speed return Integer is
      begin
         return Speed;
      end Get_Speed;

   end Motor_Data_T;

   ---------------------------------------------------------
   --  SINGLE PROTECTED OBJECT ("Drive_Motor" instance)
   ---------------------------------------------------------
   Drive_Motor : Motor_Data_T;

   ---------------------------------------------------------
   --  PUBLIC GET/SET
   ---------------------------------------------------------
   function Get_Speed_Drive return Integer is
   begin
      return Drive_Motor.Get_Speed;
   end Get_Speed_Drive;

   procedure Set_Speed_Drive (S : Integer) is
   begin
      Drive_Motor.Set_Speed (S);
   end Set_Speed_Drive;

   ---------------------------------------------------------
   --  TASK BODY (non-blocking driver or periodic logic)
   ---------------------------------------------------------
   task body Motor_Task is
      Next_Release : Time := Clock;
   begin
      Init;
      loop

         --PWM_Control.Set (Get_Speed_Drive.Get_Speed);
         Drive_Motor_Modulator.Set_Duty_Cycle (Get_Speed_Drive);

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Motor_Task;

   ---------------------------------------------------------
   --  CALIBRATE ESC-30A
   ---------------------------------------------------------
   procedure Calibrate_Esc_30A (Min : Integer; Max : Integer) is
      Next_Release : Time := Clock;
      Frequency : constant STM32.PWM.Hertz := 50;
   begin
      STM32.PWM.Configure_PWM_Timer
         (Generator => STM32.Device.Timer_4'Access,
          Frequency => Frequency);

      Drive_Motor_Modulator.Attach_PWM_Channel
         (Generator => STM32.Device.Timer_4'Access,
          Channel   => STM32.Timers.Channel_2,
          Point     => STM32.Device.PB7,
          PWM_AF    => STM32.Device.GPIO_AF_TIM4_2);

      Drive_Motor_Modulator.Enable_Output;
      Drive_Motor_Modulator.Set_Duty_Cycle(Min);
      delay until Next_Release + Milliseconds(2000);
      Drive_Motor_Modulator.Set_Duty_Cycle(Max);
      delay until Next_Release + Milliseconds(2000);
   end Calibrate_Esc_30A;

   ---------------------------------------------------------
   --  INIT (called from main)
   ---------------------------------------------------------
   procedure Init is
      Next_Release : Time := Clock;
      Frequency : constant STM32.PWM.Hertz := 50;
   begin
      STM32.PWM.Configure_PWM_Timer
         (Generator => STM32.Device.Timer_4'Access,
          Frequency => Frequency);

      Drive_Motor_Modulator.Attach_PWM_Channel
         (Generator => STM32.Device.Timer_4'Access,
          Channel   => STM32.Timers.Channel_2,
          Point     => STM32.Device.PB7,
          PWM_AF    => STM32.Device.GPIO_AF_TIM4_2);

      Drive_Motor_Modulator.Enable_Output;
      Drive_Motor_Modulator.Set_Duty_Cycle(42);

      Next_Release := Next_Release + Period;
      delay until Next_Release;

   end Init;

end Motor;
