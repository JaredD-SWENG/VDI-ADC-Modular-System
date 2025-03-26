with QOI;
with System.Storage_Elements;           use System.Storage_Elements;
with Ada.Unchecked_Deallocation;

package CV_Ada is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;
   
   procedure Free_Storage_Array is new Ada.Unchecked_Deallocation (Storage_Array, Storage_Array_Access);
   procedure Free_Input_Data (Object : in out Input_Data);
end CV_Ada;