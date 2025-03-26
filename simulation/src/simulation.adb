--  with Ada.Text_IO;
with Simulated_Camera;
with Signal_Recognition;
with Lane_Detection;
with Path_Planning;
with Event_Broker;

with Camera;      use Camera;
with CV_Ada;      use CV_Ada;
--  with Host_Serial; use Host_Serial;

with GUI_Function;

procedure Simulation is
   CAMERA_PATH : constant String := "..\camera\frames_folder";
begin
   Simulated_Camera.Start (CAMERA_PATH);
   Lane_Detection.Lane_Detection_Task.Start (Priority => 2);
   Signal_Recognition.Signal_Recognition_Task.Start (Priority => 1);
   Path_Planning.Path_Planning_Task.Start;
   Event_Broker.Event_Broker_Task.Start;

   GUI_Function.Simulation_Task.Start;
end Simulation;