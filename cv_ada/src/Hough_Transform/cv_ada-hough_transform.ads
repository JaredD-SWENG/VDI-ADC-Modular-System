package CV_Ada.Hough_Transform is

   -- Add to CV_Ada.Hough_Transform package specification
type Line_Parameters is record
   Rho, Theta : Float;
   X1, Y1, X2, Y2 : Integer;
end record;

   procedure Hough_Line_Transform
      (Input : in out Input_Data;
    Theta_Resolution : Positive := 180; 
    Rho_Resolution : Positive := 180;
    Left_Line : out Line_Parameters;
    Right_Line : out Line_Parameters);

   procedure Hough_Circle_Transform
      (Input : in out Input_Data;
       Min_Radius                : Storage_Count;
       Max_Radius                : Storage_Count;
       Threshold                 : Natural;
       Max_Circles               : Positive := 10);

private
   -- Accumulator_Cell represents a single cell in the Hough Transform accumulator,
   -- with parameters for the line (Rho and Theta) and its corresponding vote count.
   type Accumulator_Cell is record
      Rho   : Integer;  -- Distance from the origin (rho)
      Theta : Integer;  -- Angle in degrees (theta)
      Votes : Natural;  -- Vote count for this (Rho, Theta) pair
   end record;

   -- Accumulator_Array defines a 2D array for storing votes, indexed by rho and theta.
   type Accumulator_Array is
     array (Positive range <>, Positive range <>) of Natural;

   -- Accumulator_Access is an access type for dynamically allocated Accumulator_Array.
   type Accumulator_Access is access Accumulator_Array;


   -- Add these type declarations after the existing ones
   type Circle_Parameters is record
      X, Y  : Storage_Count;  -- Center coordinates
      R     : Storage_Count;  -- Radius
      Votes : Natural;       -- Number of votes
   end record;

   type Circle_Array is array (Positive range <>) of Circle_Parameters;
   type Circle_Array_Access is access Circle_Array;
end CV_Ada.Hough_Transform;
