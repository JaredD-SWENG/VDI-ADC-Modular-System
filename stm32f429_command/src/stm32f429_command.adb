-- stm32f429_command.adb (Use the task)
with Ada.Real_Time; use Ada.Real_Time;
with Tasks;
with Coms_Uart;

procedure Stm32f429_Command is
begin
   Coms_Uart.Initialize_Coms_Uart;
   loop
      delay until Clock + Seconds(10);
   end loop;
   
end Stm32f429_Command;
