package body CV_Ada is
   procedure Free_Input_Data (Object : in out Input_Data) is
   begin
      Free_Storage_Array (Object.Data);
      Object.Desc := (0, 0, 0, QOI.SRGB);
   end Free_Input_Data;
end CV_Ada;