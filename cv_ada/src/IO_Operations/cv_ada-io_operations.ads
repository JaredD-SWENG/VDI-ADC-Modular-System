package CV_Ada.IO_Operations is
   procedure Write_To_File
      (Filename   : String;
       D          : Storage_Array;
       Size       : Storage_Count);

   function Load_QOI (Filename : String; Log : Boolean := False) return Input_Data;
   procedure Load_QOI 
      (Filename : String;
       I_Data : in out Storage_Array_Access;
       O_Data : in out Storage_Array_Access;
       Result : in out Input_Data;
       Log : Boolean := False);
end CV_Ada.IO_Operations;