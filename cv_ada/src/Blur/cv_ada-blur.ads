package CV_Ada.Blur is

   procedure Box_Blur
      (Input                    : in out Input_Data);

   procedure Gaussian_Blur
      (Input                    : in out Input_Data;
       Sigma                   : Float := 1.4);
end CV_Ada.Blur;



--  package CV_Ada.Blur is

--     procedure Box_Blur
--        (Data                    : in out Storage_Array;
--         Width, Height, Channels : Storage_Count);

--     procedure Gaussian_Blur
--        (Data                    : in out Storage_Array;
--         Width, Height, Channels : Storage_Count;
--         Sigma                   : Float := 1.4);
--  end CV_Ada.Blur;
