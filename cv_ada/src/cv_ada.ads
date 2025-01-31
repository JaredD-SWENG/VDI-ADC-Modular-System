with QOI;
with System.Storage_Elements;           use System.Storage_Elements;

package CV_Ada is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;
    procedure test;
end CV_Ada;