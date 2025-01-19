with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Simulated_Camera is
   -- Initialize the camera with the path to the folder containing frames
   procedure Initialize (Folder : String);

   -- Get the next frame in sequence
   function Get_Frame return String;
end Simulated_Camera;
