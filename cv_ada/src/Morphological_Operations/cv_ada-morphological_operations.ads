package CV_Ada.Morphological_Operations is
   -- Define an enumeration type for operations
   type Morph_Operations is (Erosion, Dilation, Opening, Closing);
   type SE_Types is (SE_Square, SE_Circle);

   procedure Morphological_Operation
     (Input   : in out Input_Data; Operation : Morph_Operations;
      SE_Type :        SE_Types; SE_Size : Integer);
end CV_Ada.Morphological_Operations;
