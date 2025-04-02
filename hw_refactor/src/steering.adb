with Ada.Real_Time; use Ada.Real_Time;

package body Steering is

   Period : constant Time_Span := Milliseconds(System_Config.Steering_Period);
   Modulator_1 : STM32.PWM.PWM_Modulator;

   protected type Steering_Data_T is
      procedure Set_Angle (A : Integer);
      function Get_Angle return Integer;
   private
      Angle : Integer := 0;
   end Steering_Data_T;

   protected body Steering_Data_T is
      procedure Set_Angle (A: Integer) is
      begin
         Angle := A;
      end Set_Angle;

      function Get_Angle return Integer is
      begin
         return Angle;
      end Get_Angle;
   end Steering_Data_T;

   ---------------------------------------------------------
   --  SINGLE PROTECTED OBJECT ("Steering1" instance)
   ---------------------------------------------------------

   Steering1 : Steering_Data_T;

   ---------------------------------------------------------
   --  PUBLIC GET/SET
   ---------------------------------------------------------

   function Get_Angle_Steering1 return Integer is
   begin
      return Steering1.Get_Angle;
   end Get_Angle_Steering1;

   procedure Set_Angle_Steering1 (A: Integer) is
   begin
      Steering1.Set_Angle(A);
   end Set_Angle_Steering1;
   
   ---------------------------------------------------------
   --  TASK BODY (non-blocking driver or periodic logic)
   ---------------------------------------------------------

   task body Steering_Task is 
      Next_Release : Time := Clock;
   begin
      Init;
      loop
         Modulator_1.Set_Duty_Cycle (Get_Angle_Steering1);

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Steering_Task;

   procedure Init is 
   Next_Release : Time := Clock;
   Frequency : constant STM32.PWM.Hertz := 50;
   begin
      STM32.PWM.Configure_PWM_Timer
         (Generator => STM32.Device.Timer_2'Access,
          Frequency => Frequency);

      Modulator_1.Attach_PWM_Channel
         (Generator => STM32.Device.Timer_2'Access,
          Channel   => STM32.Timers.Channel_1,
          Point     => STM32.Device.PA5,
          PWM_AF    => STM32.Device.GPIO_AF_TIM2_1);

      Modulator_1.Enable_Output;
      Modulator_1.Set_Duty_Cycle(42);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end Init;

end Steering;