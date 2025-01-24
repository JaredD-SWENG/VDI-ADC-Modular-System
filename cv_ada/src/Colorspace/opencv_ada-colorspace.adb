package body OpenCV_Ada.Colorspace is
   -- Convert RGB(A) image to grayscale
   procedure Convert_To_Grayscale
     (Data : in out Storage_Array; Desc : QOI.QOI_Desc)
   is
      Pixel_Size : constant Storage_Count := Desc.Channels;
      Gray_Value : Storage_Element;
   begin
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if I mod Pixel_Size = 1 then
            -- Convert to grayscale using luminosity method
            -- Y = 0.299R + 0.587G + 0.114B
            Gray_Value :=
              Storage_Element
                ((Integer (Data (I)) * 299 + Integer (Data (I + 1)) * 587
                  + Integer (Data (I + 2)) * 114)
                 / 1000);

            -- Set RGB channels to the same gray value
            Data (I) := Gray_Value;     -- R
            Data (I + 1) := Gray_Value; -- G
            Data (I + 2) := Gray_Value; -- B

            -- Preserve alpha channel if it exists
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Grayscale;

   -- Add this procedure after Convert_To_Grayscale
   procedure Convert_To_Black_And_White
     (Data      : in out Storage_Array;
      Desc      : QOI.QOI_Desc;
      Threshold : Storage_Element := 128)
   is
      Pixel_Size : constant Storage_Count := Desc.Channels;
      BW_Value   : Storage_Element;
   begin
      for I in Data'First .. Data'Last - (Pixel_Size - 1) loop
         if I mod Pixel_Size = 1 then
            -- Check if the pixel is above or below threshold
            -- Since image is already grayscale, we can just check one channel
            if Data (I) >= Threshold then
               BW_Value := 255; -- White

            else
               BW_Value := 0;   -- Black
            end if;

            -- Set RGB channels to either black or white
            Data (I) := BW_Value;     -- R
            Data (I + 1) := BW_Value; -- G
            Data (I + 2) := BW_Value; -- B

            -- Preserve alpha channel if it exists
            if Pixel_Size = 4 then
               Data (I + 3) := Data (I + 3);
            end if;
         end if;
      end loop;
   end Convert_To_Black_And_White;
end OpenCV_Ada.Colorspace;
