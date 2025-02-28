with Coms_Uart; use Coms_Uart;
with Drive_Motor; use Drive_Motor;
with Motor_Task; use Motor_Task;

procedure Demo_Driver is
   Motor_Instance : aliased Motor;
begin
   -- Initialize the UART for communication
   Initialize_Coms_Uart;

   -- Initialize and start Motor_Task with the Motor instance
   Initialize(Motor_Instance);
   Motor_Task.Start(Motor_Instance);

   loop
      null;
      delay 0.1;
   end loop;
end Demo_Driver;
