-------------------------------------------------------------------------------
--  Motor Control Package Body
--
--  File Name   : motor.adb
--  Description : Implementation of motor control logic, including PWM
--                modulation, GPIO configuration, and ESC calibration.
--  Author      : Team 21
--  Created     : 28 MAR 2025
--  Last Update : 04 APR 2025
--  Version     : 0.2
--
--  Notes       : This package is designed for STM32 microcontrollers and
--                assumes the use of Ada drivers for STM32 peripherals.
--
--  Dependencies: Ada.Real_Time, STM32.PWM, STM32.GPIO, STM32.Device,
--                STM32.Timers, STM32.Board
--
--  Revision History:
--    Version 0.1 - Initial implementation
--    Version 0.2 - Added calibration procedure for ESC-30A
-------------------------------------------------------------------------------
with Ada.Real_Time; use Ada.Real_Time;

package body Motor is

   Period : constant Time_Span    := Milliseconds (System_Config.Motor_Period);

   -- To limit capacity of the motor
   Max_Speed : constant Integer := System_Config.Drive_Max_Speed;
   Min_Speed : constant Integer := System_Config.Drive_Min_Speed;

   -- PWM Modulator for the motor
   Drive_Motor_Modulator : STM32.PWM.PWM_Modulator;
   Drive_GPIO            : STM32.GPIO.GPIO_Point := STM32.Device.PC8;

   -- ESC parameters 
   PWM_Frequency : constant STM32.PWM.Hertz := System_Config.Drive_Frequency;
   Max_Duty_Cycle : constant Integer := System_Config.Drive_Max_Duty_Cycle;
   Min_Duty_Cycle : constant Integer := System_Config.Drive_Min_Duty_Cycle;
   Cal_Time : constant Standard.Duration := System_Config.ESC30A_Cal_Time;

   protected type Motor_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
   private
      Speed : Integer := Min_Speed;
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
         Drive_Motor_Modulator.Set_Duty_Cycle
           (Map_Speed_To_Duty (Drive_Motor.Get_Speed));

         Next_Release := Next_Release + Period;
         delay until Next_Release;
      end loop;
   end Motor_Task;

   ---------------------------------------------------------
   --  CALIBRATE ESC-30A
   ---------------------------------------------------------
   procedure Calibrate_Esc_30A is
      Next_Release : Time := Clock;
   begin

      -- Set PWM to 100%
      Drive_Motor_Modulator.Set_Duty_Cycle
        (Max_Duty_Cycle);

      -- Turn on ESC
      STM32.GPIO.Set (Drive_GPIO);

      delay Cal_Time;

      -- Set PWM to 0%
      Drive_Motor_Modulator.Set_Duty_Cycle
        (Min_Duty_Cycle);

   end Calibrate_Esc_30A;

   ---------------------------------------------------------
   --  INIT (called from main)
   ---------------------------------------------------------
   procedure Init is
   begin
      STM32.Device.Enable_Clock (Drive_GPIO);
      STM32.GPIO.Configure_IO
        (Drive_GPIO,
         (Mode  => STM32.GPIO.Mode_Out, Output_Type => STM32.GPIO.Push_Pull,
          Speed => STM32.GPIO.Speed_100MHz, Resistors => STM32.GPIO.Floating));
      STM32.GPIO.Clear (Drive_GPIO);

      STM32.PWM.Configure_PWM_Timer
        (Generator => STM32.Device.Timer_4'Access, Frequency => PWM_Frequency);

      Drive_Motor_Modulator.Attach_PWM_Channel
        (Generator => STM32.Device.Timer_4'Access,
         Channel   => STM32.Timers.Channel_2, Point => STM32.Device.PB7,
         PWM_AF    => STM32.Device.GPIO_AF_TIM4_2);

      Drive_Motor_Modulator.Enable_Output;

      Calibrate_Esc_30A;

   end Init;

   function Map_Speed_To_Duty (Speed : Integer) return Integer is
      Duty_Cycle     : Integer;
      Speed_Setpoint : Integer := 0;

   begin
      if Speed < Min_Speed then
         Speed_Setpoint := Min_Speed;
      elsif Speed > Max_Speed then
         Speed_Setpoint := Max_Speed;
      else
         Speed_Setpoint := Speed;
      end if;

      Duty_Cycle :=
        Min_Duty_Cycle +
        ((Max_Duty_Cycle -
            Min_Duty_Cycle) *
           Speed_Setpoint) /
          100;

      return Duty_Cycle;
   end Map_Speed_To_Duty;

end Motor;
