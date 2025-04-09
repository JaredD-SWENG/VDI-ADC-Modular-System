package Simulation_GUI is
   procedure Set_GUI_Path(Path : String := "glade_source\CarSimulator.glade");
   procedure Start_Simulation_GUI;
private
   FilePath : String := "glade_source\CarSimulator.glade";
end Simulation_GUI;