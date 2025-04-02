with STM32.Device;
with STM32.GPIO;

package body Digital_Out is
   procedure Initialize (This : in out Digital_Pin; Pin : STM32.GPIO.GPIO_Point) is
   begin
      This.Pin := Pin;

      STM32.Device.Enable_Clock (This.Pin);

      STM32.GPIO.Configure_IO (This.Pin,
         (Mode        => STM32.GPIO.Mode_Out,
          Output_Type => STM32.GPIO.Push_Pull,
          Speed       => STM32.GPIO.Speed_100MHz,
          Resistors   => STM32.GPIO.Floating));

      STM32.GPIO.Clear (This.Pin);
   end Initialize;

   procedure Enable (This : in out Digital_Pin) is
   begin
      STM32.GPIO.Set (This.Pin);
   end Enable;

   procedure Disable (This : in out Digital_Pin) is
   begin
      STM32.GPIO.Clear (This.Pin);
   end Disable;

   function Is_Enabled (This : in Digital_Pin) return Boolean is
   begin
      return STM32.GPIO.Set (This.Pin);
   end Is_Enabled;

end Digital_Out;

--  procedure Digital_Out is
   
--  begin
--     STM32.Device.Enable_Clock (Power_Pin);

--     STM32.GPIO.Configure_IO (Power_Pin,
--        (Mode        => STM32.GPIO.Mode_Out,
--           Output_Type => STM32.GPIO.Push_Pull,
--           Speed       => STM32.GPIO.Speed_100MHz,
--           Resistors   => STM32.GPIO.Floating));
   
--     STM32.GPIO.Set (Power_Pin);

--     loop
--        STM32.GPIO.Clear (Power_Pin);
--        delay 1.0;
--        STM32.GPIO.Set (Power_Pin);
--        delay 1.0;
--     end loop;
--  end Digital_Out;
