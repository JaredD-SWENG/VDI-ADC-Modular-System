with STM32.Device;
with STM32.GPIO;

package Digital_Out is

   type Digital_Pin is limited private;
   
   Power_Pin : STM32.GPIO.GPIO_Point := STM32.Device.PC8;

   procedure Initialize (This : in out Digital_Pin; Pin : STM32.GPIO.GPIO_Point);
   procedure Enable (This : in out Digital_Pin);
   procedure Disable (This : in out Digital_Pin);
   function Is_Enabled (This : in Digital_Pin) return Boolean;
private
   type Digital_Pin is tagged limited record
      Pin : STM32.GPIO.GPIO_Point;
   end record;
end Digital_Out;