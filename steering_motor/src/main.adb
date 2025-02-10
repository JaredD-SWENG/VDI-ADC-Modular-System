with HAL; use HAL;
with Steering_Motor;

procedure Main is

   -- Single instance of the steering motor.
   My_Steering : Steering_Motor.Steering;
   temp : Float;
   
begin
   -- Initialize the steering motor instance.
   Steering_Motor.Initialize(My_Steering);

   Steering_Motor.Center (My_Steering);
   delay 5.0;

   loop
      -- Center the steering motor.
      --Steering_Motor.Center (My_Steering);
      --delay 5.0;

      -- Steer left.
      --Steering_Motor.Steer_Left (My_Steering);
      --delay 1.0;

      --Steer right.
      --Steering_Motor.Steer_Right (My_Steering);
      --delay 1.0;

      --delay 5.0;

      -- Center the steering motor using the Set_Angle operation.
      --Steering_Motor.Set_Angle (My_Steering, 0);
      --delay 1.0;

      -- Steer left 30 degrees.
      --Steering_Motor.Set_Angle (My_Steering, 30);
      --delay 1.0;

      -- Steer right 30 degrees.
      --Steering_Motor.Set_Angle (My_Steering, -30);
      --delay 1.0;

      --Steering_Motor.Set_Scaled_Angle(My_Steering, (-0.4));
      --delay(0.5);
      --Steering_Motor.Set_Scaled_Angle(My_Steering, (-1.0));
      --delay(0.5);
      --Steering_Motor.Set_Scaled_Angle(My_Steering, (0.4));
      --delay(0.5);
      --Steering_Motor.Set_Scaled_Angle(My_Steering, (1.0));
      --delay(0.5);

      Steering_Motor.Smooth_Steering(My_Steering, -30.0, 0.1);
      delay 1.0;
      Steering_Motor.Smooth_Steering(My_Steering, 30.0, 0.5);
      delay 1.0;
      Steering_Motor.Smooth_Steering(My_Steering, 0.0, 0.05);
      delay 1.0;

   end loop;
end Main;
