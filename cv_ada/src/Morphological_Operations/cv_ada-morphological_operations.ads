package CV_Ada.Morphological_Operations is
   -- Define an enumeration type for operations
   type Morph_Operations is (Erosion, Dilation, Opening, Closing);
   type SE_Types is (SE_Square, SE_Circle);

   procedure Morphological_Operation
      (Data       : in out Storage_Array;
       Desc       : QOI.QOI_Desc;
       Operation  : Morph_Operations;
       SE_Type    : SE_Types;
       SE_Size    : Integer);
end CV_Ada.Morphological_Operations;