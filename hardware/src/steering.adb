with Ada.Real_Time; use Ada.Real_Time;

package body Steering is

   Period : constant Time_Span := Milliseconds(System_Config.Steering_Period);
   Modulator_1 : STM32.PWM.PWM_Modulator;
   Min_Angle: Integer := -60;
   Max_Angle: Integer := 60;
   Max_PWM: Integer := 10;
   Min_PWM: Integer := 4;

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
   --  CONVERSION (Handles the angle conversion based on min and max values)
   ---------------------------------------------------------
   
   function Convert_Angle_to_PWM(i: Integer) return Integer is
      Angle: Integer;
      Span: Float := Float(abs(Max_Angle) + abs(Min_Angle));
      Scale: Float := (Float(Min_PWM) - Float(Max_PWM)) / (Span);
      PWM_Center: Integer := ((Max_PWM - Min_PWM) / 2) + Min_PWM;
   begin
      if i > Max_Angle then
         Angle := Max_Angle;
      elsif i < Min_Angle then
         Angle := Min_Angle;
      else
         Angle := i;
      end if;

      return Integer((Float(Angle) * Scale) + Float(PWM_Center));
   end;

   ---------------------------------------------------------
   --  TASK BODY (non-blocking driver or periodic logic)
   ---------------------------------------------------------

   task body Steering_Task is 
      Next_Release : Time := Clock;
      Angle: Integer;
   begin
      Init;
      loop
         Angle := Convert_Angle_to_PWM(Get_Angle_Steering1);
         Modulator_1.Set_Duty_Cycle (Angle);

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
      Modulator_1.Set_Duty_Cycle(7);

      Next_Release := Next_Release + Period;
      delay until Next_Release;
   end Init;

end Steering;