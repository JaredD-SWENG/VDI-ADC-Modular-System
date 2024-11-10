with System.Storage_Elements; use System.Storage_Elements;

package OpenCV_Ada.Hough_Transform is
   -- Add these type declarations after the existing ones
   type Accumulator_Cell is record
      Rho   : Integer;
      Theta : Integer;
      Votes : Natural;
   end record;

   type Accumulator_Array is
     array (Positive range <>, Positive range <>) of Natural;
   type Accumulator_Access is access Accumulator_Array;

   procedure Hough_Line_Transform
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Theta_Resolution        : Positive := 180;
      Rho_Resolution          : Positive := 180);
end OpenCV_Ada.Hough_Transform;
