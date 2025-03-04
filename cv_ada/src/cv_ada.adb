package body CV_Ada is
   procedure Free_Input_Data (Object : in out Input_Data) is
      begin
         if Object.Data /= null then
            Free_Storage_Array (Object.Data);
            Object.Data := null;
         end if;
      Object.Desc := (0, 0, 0, QOI.SRGB);
   end Free_Input_Data;
end CV_Ada;