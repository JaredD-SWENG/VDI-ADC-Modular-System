with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Directories; use Ada.Directories;

package body Simulated_Camera is
   task body Camera_Task is
   begin
      -- Wait for initialization data
      accept Initialize(Path : String) do
         Camera.Initialize(Path);
      end Initialize;

      
      -- Initialize the camera with the folder path
      
      
      -- Start advancing frames
      loop
         delay 0.033; -- ~30 fps, adjust as needed for your application
         Camera.Advance_Global_Frame;
         
         if Camera.Get_Current_Global_Frame >= Camera.Get_Frame_Max_Count then
            exit; -- No more frames
         end if;
      end loop;
   end Camera_Task;
   
   procedure Start(Folder_Path : String) is
   begin
      -- Create a new camera task
      Camera_Instance := new Camera_Task;
      
      -- Initialize the task with the folder path
      Camera_Instance.Initialize(Folder_Path);
   end Start;
   
   procedure Finalize is
      procedure Free is new Ada.Unchecked_Deallocation(
         Object => Camera_Task, 
         Name => Camera_Task_Access);
   begin
      if Camera_Instance /= null then
         -- In Ada, we can't explicitly terminate a task, but we can free its memory
         -- The task will terminate naturally when it completes its execution
         Free(Camera_Instance);
      end if;
   end Finalize;
end Simulated_Camera;
