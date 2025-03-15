with HAL.Bitmap;
with STM32.Board;
with LCD_Std_Out;
with BMP_Fonts;    use BMP_Fonts;

package RC_Car_Display is
   ----------------------------------------------------------------------------
   -- Display layout constants
   ----------------------------------------------------------------------------
   Status_Start_X  : constant := 10;  -- X-coordinate for status messages
   Motor_Y         : constant := 10;  -- Y-coordinate for motor status
   Steering_Y      : constant := 40;  -- Y-coordinate for steering status
   Battery_Y       : constant := 70;  -- Y-coordinate for battery status
   Debug_Start_Y   : constant := 100; -- Starting Y-coordinate for debug messages
   Debug_Line_Step : constant := 30;  -- Vertical space between debug lines

   ----------------------------------------------------------------------------
   -- Initialize
   -- Initializes the display, sets colors, and draws static labels.
   ----------------------------------------------------------------------------
   procedure Initialize;

   ----------------------------------------------------------------------------
   -- Update_Motor
   -- Updates the motor speed display with the given speed value.
   ----------------------------------------------------------------------------
   procedure Update_Motor(Speed : Natural);

   ----------------------------------------------------------------------------
   -- Update_Steering
   -- Updates the steering angle display with the given angle value.
   ----------------------------------------------------------------------------
   procedure Update_Steering(Angle : Integer);

   ----------------------------------------------------------------------------
   -- Update_Battery
   -- Updates the battery voltage display with the given voltage value.
   ----------------------------------------------------------------------------
   procedure Update_Battery(Voltage : Float);

   ----------------------------------------------------------------------------
   -- Debug_Message
   -- Adds a new debug message to the debug area and shifts previous messages up.
   ----------------------------------------------------------------------------
   procedure Debug_Message(Msg : String);

private
   ----------------------------------------------------------------------------
   -- Private constants and data structures for debug message management.
   ----------------------------------------------------------------------------
   Max_Debug_Lines    : constant := 14;          -- Maximum number of debug lines.
   Debug_Line_Length  : constant := 30;          -- Maximum length of each debug line.
   type Debug_Buffer is array (1..Max_Debug_Lines) of String(1..Debug_Line_Length);
   Debug_History      : Debug_Buffer := (others => (others => ' '));  
      -- Buffer to store debug messages.
   Current_Debug      : Natural := 0;            -- Current number of debug messages.
   Current_Font       : BMP_Font := Font16x24;    -- Font used for display.
end RC_Car_Display;