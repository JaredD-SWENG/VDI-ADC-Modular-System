with CV_Ada;
with GUI_Functions;

package GUI_Task is
   task Send_Task is
      entry Send_Raw_Image;
      entry Send_Console_Text (Text : String);
      entry Left_Signal_On;
      entry Right_Signal_On;
      entry Signal_Off;
      entry Red_Light_On;
      entry Yellow_Light_On;
      entry Green_Light_On;
      entry Light_Off;
   end Send_Task;
end GUI_Task;