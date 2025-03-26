package CV_Ada.IO_Operations is
   procedure Write_To_File
      (Filename   : String;
       D          : in out Storage_Array_Access;
       Size       : Storage_Count);

   function Load_QOI (Filename : String) return Input_Data;
end CV_Ada.IO_Operations;