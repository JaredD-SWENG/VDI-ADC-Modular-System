with Ada.Real_Time; use Ada.Real_Time;
with HAL; use HAL;

with STM32.Device;  use STM32.Device;

package body Uart is

   protected type Uart_Data_T is
      procedure Set_Speed (S : Integer);
      function Get_Speed return Integer;
      procedure Set_Emergency_Stop (S : Boolean);
      function Get_Emergency_Stop return Boolean;
   private
      Speed : Integer := 0;
      Emergency_Stop : Boolean := False;
   end Uart_Data_T;

   protected body Uart_Data_T is
      procedure Set_Speed (S : Integer) is
      begin
         Speed := S;
      end Set_Speed;

      function Get_Speed return Integer is
         (Speed);

      procedure Set_Emergency_Stop (S : Boolean) is
      begin
         Emergency_Stop := S;
      end Set_Emergency_Stop;

      function Get_Emergency_Stop return Boolean is
         (Emergency_Stop);
   end Uart_Data_T;

   Uart1 : Uart_Data_T;

   function Get_Speed_Cmd return Integer is
      (Uart1.Get_Speed);

   function Emergency_Stop return Boolean is
      (Uart1.Get_Emergency_Stop);

   type Cmd_T is (Emergency_Stop, Set_Speed, Undefined);
   for Cmd_T use (Emergency_Stop => Character'Pos('E'), 
                  Set_Speed => Character'Pos('S'),
                  Undefined => Character'Pos('U'));

   type Command (Cmd : Cmd_T) is record
      case Cmd is
         when Set_Speed =>
            Speed : Integer;
         when Emergency_Stop =>
            State : Boolean;
         when Undefined =>
            null; 
      end case;
   end record;

   function Read_Uart return Command is
      Received   : UInt9;
      C          : Character with Address => Received'Address;
      Msg        : String (1 .. 120);
      Pos        : Natural := Msg'First;
   begin
      loop
         USART_1.Receive (Received);
         exit when C = Ascii.NUL;
         Msg (Pos) := C;
         Pos := Pos + 1;
      end loop;
      if Msg (1) = 'E' then
         return (Cmd => Emergency_Stop, State => True);
      elsif Msg (1) = 'S' then
         return (Cmd => Set_Speed, Speed => Integer'Value (Msg (2 .. Msg'Last)));
      else
         return (Cmd => Undefined);
      end if;
   end Read_Uart;

   task body Uart_Task is
      Cmd : Command := (Cmd => Set_Speed, Speed => 0);
   begin
      loop
         Cmd := Read_Uart;
         if Cmd.Cmd = Set_Speed then
            Uart1.Set_Speed (Cmd.Speed);
         elsif Cmd.Cmd = Emergency_Stop then
            Uart1.Set_Emergency_Stop (Cmd.State);
         end if;
      end loop;
   end Uart_Task;

   procedure Init is
   begin
      null;
   end Init;

end Uart;