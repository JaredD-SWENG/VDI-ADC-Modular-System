with Ada.Real_Time; use Ada.Real_Time;

package body Motor is

   Period : constant Time_Span := Milliseconds (System_Config.Motor_Period);
   Modulator_1 : STM32.PWM.PWM_Modulator;

   protected type Motor_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
   private
      Speed : Integer := 0;
   end Motor_Data_T;

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
   --  SINGLE PROTECTED OBJECT ("Motor1" instance)
   ---------------------------------------------------------
   Motor1 : Motor_Data_T;

   ---------------------------------------------------------
   --  PUBLIC GET/SET
   ---------------------------------------------------------
   function Get_Speed_Motor_1 return Integer is
   begin
      return Motor1.Get_Speed;
   end Get_Speed_Motor_1;

   procedure Set_Speed_Motor_1 (S : Integer) is
   begin
      Motor1.Set_Speed (S);
   end Set_Speed_Motor_1;

   ---------------------------------------------------------
   --  TASK BODY (non-blocking driver or periodic logic)
   ---------------------------------------------------------
   task body Motor_Task is
      Next_Release : Time := Clock;
   begin

      loop

         --PWM_Control.Set (Get_Speed_Motor_1.Get_Speed);
         Modulator_1.Set_Duty_Cycle (Get_Speed_Motor_1);

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Motor_Task;

   procedure Init is
      Next_Release : Time := Clock;
      Frequency : constant STM32.PWM.Hertz := 50;
   begin
      STM32.PWM.Configure_PWM_Timer
         (Generator => STM32.Device.Timer_4'Access,
          Frequency => Frequency);

      Modulator_1.Attach_PWM_Channel
         (Generator => STM32.Device.Timer_4'Access,
          Channel   => STM32.Timers.Channel_2,
          Point     => STM32.Device.PB7,
          PWM_AF    => STM32.Device.GPIO_AF_TIM4_2);

      Modulator_1.Enable_Output;
      Modulator_1.Set_Duty_Cycle(42);

      Next_Release := Next_Release + Period;
      delay until Next_Release;

   end Init;

end Motor;
