with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GUI_Functions;

package body Simulation_GUI is
   procedure Set_GUI_Path(Path : String := "..\..\GladeGUI\CarSimulatorGUI.glade") is
   begin
      FilePath := To_String (To_Unbounded_String (Path));
   end Set_GUI_Path;

   procedure Start_Simulation_GUI is
   begin
      Put_Line ("Starting Simulation GUI: " & FilePath);

      --  GUI_Functions.Simulation_Wrapper.Start (FilePath);
      --  GUI_Functions.Initialize (FilePath);
      --  GUI_Functions.Simulation_Task.Start (FilePath);
      
      Put_Line ("Finished Running: " & FilePath);
   end Start_Simulation_GUI;
end Simulation_GUI;