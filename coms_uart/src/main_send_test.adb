with Coms_Uart; use Coms_Uart;
with HAL;       use HAL;

procedure Main_Send_Test is
begin
   -- Initialize the UART system.
   Initialize;
   -- Optionally, flush any residual data.
   Flush_RX;
   
   -- Send an initial prompt.
   Send_String ("Hello from Ada!" & ASCII.CR & ASCII.LF);

   -- Enter the continuous demo loop.
   Run;
end Main_Send_Test;
