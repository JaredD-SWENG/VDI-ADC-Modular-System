package Commands is
   -- Angle and percentage subtypes
   subtype Angle_Degrees is Integer range -45..45;
   subtype Percentage is Integer range 0..100;

   -- Separate command types for different subsystems
   --  type Drive_Command is (Calibrate, 
   --                             Set_Speed, 
   --                             Emergency_Stop);

   --  type Steering_Command is (Set_Angle, 
   --                             Center, 
   --                             Steer_Left, 
   --                             Steer_Right);


   type Command_Type is (
      Calibrate_Motor,
      Set_Motor_Speed,
      Motor_Stop,
      Emergency_Stop,
      Set_Steering_Angle,
      Center_Steering,
      Display_Status,
      Exit_Command
   );

   -- General command parameter structure
   type Command_Param is record
      Speed : Integer := 0; -- For either
   end record;
end Commands;
