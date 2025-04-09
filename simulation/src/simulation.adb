with Ada.Text_IO;
with GUI_Functions;
with Simulated_Camera;
with Signal_Recognition;
with Lane_Detection;
with Path_Planning;
with Event_Broker;
with Camera;      use Camera;
with CV_Ada;      use CV_Ada;
with Host_Serial; use Host_Serial;

procedure Simulation is
   CAMERA_PATH : constant String := "..\..\..\..\..\2025 1 Spring Semester\SWENG 481\frames_folder\";
   --  CAMERA_PATH : constant String := "..\camera\frames_folder";
begin
   Event_Broker.Event_Broker_Task.Start;
   Path_Planning.Path_Planning_Task.Start;
   Simulated_Camera.Start (CAMERA_PATH);
   Lane_Detection.Lane_Detection_Task.Start (Priority => 1);
   Signal_Recognition.Signal_Recognition_Task.Start (Priority => 2);

   GUI_Functions.Initialize ("..\simulation_gui\glade_source\CarSimulator.glade");
end Simulation;