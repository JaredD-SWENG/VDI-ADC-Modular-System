package Simulation_GUI is
   procedure Set_GUI_Path(Path : String := "..\..\GladeGUI\CarSimulatorGUI.glade");
   procedure Start_Simulation_GUI;
private
   FilePath : String := "..\..\GladeGUI\CarSimulatorGUI.glade";
end Simulation_GUI;