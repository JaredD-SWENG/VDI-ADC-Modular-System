with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body OpenCV_Ada.Blur is
   -- Add blur functionality
   procedure Average_Blur
     (Data : in out Storage_Array; Width, Height, Channels : Storage_Count)
   is
      Temp        : Storage_Array := Data;
      Kernel_Size : constant Storage_Count := 6;
      Half_Kernel : constant Storage_Count := Kernel_Size / 2;
   begin
      for Y in Half_Kernel .. Height - Half_Kernel - 1 loop
         for X in Half_Kernel .. Width - Half_Kernel - 1 loop
            for C in 0 .. Channels - 1 loop
               declare
                  Sum   : Integer := 0;
                  Count : Integer := 0;
               begin
                  -- Apply box blur kernel
                  for KY in -Half_Kernel .. Half_Kernel loop
                     for KX in -Half_Kernel .. Half_Kernel loop
                        declare
                           Pos : constant Storage_Count :=
                             ((Y + KY) * Width + (X + KX)) * Channels + C + 1;
                        begin
                           Sum := Sum + Integer (Temp (Pos));
                           Count := Count + 1;
                        end;
                     end loop;
                  end loop;

                  -- Store averaged result
                  Data (((Y * Width) + X) * Channels + C + 1) :=
                    Storage_Element (Sum / Count);
               end;
            end loop;
         end loop;
      end loop;
   end Average_Blur;

   procedure Gaussian_Blur
     (Data                    : in out Storage_Array;
      Width, Height, Channels : Storage_Count;
      Sigma                   : Float := 1.4)
   is

      -- Calculate kernel size based on sigma (usually 6*sigma)
      Kernel_Size : constant Positive :=
        Positive (Float'Ceiling (6.0 * Sigma));
      Half_Size   : constant Integer := Kernel_Size / 2;

      -- Create temporary array for processing
      Temp : Storage_Array := Data;
   begin
      -- Create Gaussian kernel
      declare
         type Kernel_Type is
           array (Integer range <>, Integer range <>) of Float;
         Kernel :
           Kernel_Type (-Half_Size .. Half_Size, -Half_Size .. Half_Size);
         Sum    : Float := 0.0;
      begin
         -- Generate Gaussian kernel
         for Y in Kernel'Range(1) loop
            for X in Kernel'Range(2) loop
               Kernel (Y, X) :=
                 (1.0 / (2.0 * Ada.Numerics.Pi * Sigma * Sigma))
                 * Float
                     (Exp (-(Float (X * X + Y * Y) / (2.0 * Sigma * Sigma))));
               Sum := Sum + Kernel (Y, X);
            end loop;
         end loop;

         -- Normalize kernel
         for Y in Kernel'Range(1) loop
            for X in Kernel'Range(2) loop
               Kernel (Y, X) := Kernel (Y, X) / Sum;
            end loop;
         end loop;

         -- Apply convolution
         for Y in Half_Size + 1 .. Integer (Height) - Half_Size loop
            for X in Half_Size + 1 .. Integer (Width) - Half_Size loop
               for C in 0 .. Integer (Channels) - 1 loop
                  declare
                     Sum : Float := 0.0;
                  begin
                     for KY in Kernel'Range(1) loop
                        for KX in Kernel'Range(2) loop
                           declare
                              Idx : constant Storage_Count :=
                                ((Storage_Count (Y + KY - 1) * Width
                                  + Storage_Count (X + KX - 1))
                                 * Channels
                                 + Storage_Count (C)
                                 + 1);
                           begin
                              Sum :=
                                Sum + Float (Temp (Idx)) * Kernel (KY, KX);
                           end;
                        end loop;
                     end loop;
                     Data
                       (((Storage_Count (Y - 1) * Width
                          + Storage_Count (X - 1))
                         * Channels
                         + Storage_Count (C)
                         + 1)) :=
                       Storage_Element (Float'Rounding (Sum));
                  end;
               end loop;
            end loop;
         end loop;
      end;
   end Gaussian_Blur;
end OpenCV_Ada.Blur;
