package CV_Ada.IO_Operations is
   procedure Write_To_File
      (Filename   : String;
       D          : Storage_Array;
       Size       : Storage_Count);

   function Load_QOI (Filename : String) return Input_Data;
end CV_Ada.IO_Operations;