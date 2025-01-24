with QOI;
with System.Storage_Elements; use System.Storage_Elements;

package Load_QOI is
   type Storage_Array_Access is access all Storage_Array;

   type Input_Data is record
      Data : Storage_Array_Access;
      Desc : QOI.QOI_Desc;
   end record;

   procedure Write_To_File (Filename : String; D : Storage_Array; Size : Storage_Count);
   function Get_QOI (Filename : String) return Input_Data;
end Load_QOI;
