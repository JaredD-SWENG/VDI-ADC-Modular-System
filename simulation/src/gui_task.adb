with Camera;
with GUI_Functions;

with Ada.Text_IO; use Ada.Text_IO;

package body GUI_Task is
   task body Send_Task is
      begin
         accept Send_Raw_Image do
            GUI_Functions.Update_GUI.Set_Raw_Image (Camera.Vid (Camera.Get_Current_Global_Frame));
         end Send_Raw_Image;

         accept Send_Console_Text (Text : String) do
            GUI_Functions.AddConsoleText (Text);
         end Send_Console_Text;

         accept Left_Signal_On do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Right_Signal, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Left_Signal, GUI_Functions.On);
         end Left_Signal_On;

         accept Right_Signal_On do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Left_Signal, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Right_Signal, GUI_Functions.On);
         end Right_Signal_On;

         accept Signal_Off do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Left_Signal, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Right_Signal, GUI_Functions.Off);
         end Signal_Off;

         accept Red_Light_On do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Yellow_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Green_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Red_Light, GUI_Functions.On);
         end Red_Light_On;

         accept Yellow_Light_On do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Red_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Green_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Yellow_Light, GUI_Functions.On);
         end Yellow_Light_On;

         accept Green_Light_On do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Red_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Yellow_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Green_Light, GUI_Functions.On);
         end Green_Light_On;

         accept Light_Off do
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Red_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Yellow_Light, GUI_Functions.Off);
            GUI_Functions.Set_Detection_Elements (GUI_Functions.Green_Light, GUI_Functions.Off);
         end Light_Off;

         --  loop
         --     GUI_Functions.Update_GUI.Set_Raw_Image (Camera.Vid (Camera.Get_Current_Global_Frame));
         --     delay 0.1;
         --  end loop;
      end Send_Task;
end GUI_Task;