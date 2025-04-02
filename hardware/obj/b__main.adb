pragma Warnings (Off);
pragma Ada_95;
pragma Source_File_Name (ada_main, Spec_File_Name => "b__main.ads");
pragma Source_File_Name (ada_main, Body_File_Name => "b__main.adb");
pragma Suppress (Overflow_Check);

with System.Restrictions;

package body ada_main is

   E109 : Short_Integer; pragma Import (Ada, E109, "ada__tags_E");
   E100 : Short_Integer; pragma Import (Ada, E100, "ada__strings__text_buffers_E");
   E098 : Short_Integer; pragma Import (Ada, E098, "system__bb__timing_events_E");
   E016 : Short_Integer; pragma Import (Ada, E016, "ada__exceptions_E");
   E044 : Short_Integer; pragma Import (Ada, E044, "system__soft_links_E");
   E042 : Short_Integer; pragma Import (Ada, E042, "system__exception_table_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "ada__streams_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__finalization_root_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "ada__finalization_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__storage_pools_E");
   E168 : Short_Integer; pragma Import (Ada, E168, "system__finalization_masters_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__real_time_E");
   E175 : Short_Integer; pragma Import (Ada, E175, "system__pool_global_E");
   E193 : Short_Integer; pragma Import (Ada, E193, "system__tasking__protected_objects_E");
   E199 : Short_Integer; pragma Import (Ada, E199, "system__tasking__protected_objects__multiprocessors_E");
   E212 : Short_Integer; pragma Import (Ada, E212, "system__tasking__restricted__stages_E");
   E217 : Short_Integer; pragma Import (Ada, E217, "hal__audio_E");
   E258 : Short_Integer; pragma Import (Ada, E258, "hal__bitmap_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "hal__framebuffer_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "hal__gpio_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "hal__i2c_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "hal__real_time_clock_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "hal__spi_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "hal__time_E");
   E284 : Short_Integer; pragma Import (Ada, E284, "hal__touch_panel_E");
   E237 : Short_Integer; pragma Import (Ada, E237, "hal__uart_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "bitmap_color_conversion_E");
   E275 : Short_Integer; pragma Import (Ada, E275, "ili9341_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "l3gd20_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "ravenscar_time_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "soft_drawing_bitmap_E");
   E266 : Short_Integer; pragma Import (Ada, E266, "memory_mapped_bitmap_E");
   E140 : Short_Integer; pragma Import (Ada, E140, "stm32__adc_E");
   E146 : Short_Integer; pragma Import (Ada, E146, "stm32__dac_E");
   E205 : Short_Integer; pragma Import (Ada, E205, "stm32__dma__interrupts_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "stm32__dma2d_E");
   E255 : Short_Integer; pragma Import (Ada, E255, "stm32__dma2d__interrupt_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "stm32__dma2d__polling_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "stm32__dma2d_bitmap_E");
   E159 : Short_Integer; pragma Import (Ada, E159, "stm32__exti_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "stm32__fmc_E");
   E216 : Short_Integer; pragma Import (Ada, E216, "stm32__i2s_E");
   E223 : Short_Integer; pragma Import (Ada, E223, "stm32__power_control_E");
   E220 : Short_Integer; pragma Import (Ada, E220, "stm32__rtc_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "stm32__sdram_E");
   E228 : Short_Integer; pragma Import (Ada, E228, "stm32__spi_E");
   E231 : Short_Integer; pragma Import (Ada, E231, "stm32__spi__dma_E");
   E152 : Short_Integer; pragma Import (Ada, E152, "stm32__gpio_E");
   E186 : Short_Integer; pragma Import (Ada, E186, "stm32__i2c_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "stm32__i2c__dma_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "stm32__syscfg_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "stm32__usarts_E");
   E136 : Short_Integer; pragma Import (Ada, E136, "stm32__device_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "stm32__ltdc_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "framebuffer_ltdc_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "stm32__pwm_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "stmpe811_E");
   E248 : Short_Integer; pragma Import (Ada, E248, "framebuffer_ili9341_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "touch_panel_stmpe811_E");
   E240 : Short_Integer; pragma Import (Ada, E240, "stm32__board_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "motor_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "uarts_E");
   E289 : Short_Integer; pragma Import (Ada, E289, "uart_E");

   Sec_Default_Sized_Stacks : array (1 .. 3) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

   Local_Priority_Specific_Dispatching : constant String := "";
   Local_Interrupt_States : constant String := "";

   Is_Elaborated : Boolean := False;

   procedure adafinal is
      procedure s_stalib_adafinal;
      pragma Import (Ada, s_stalib_adafinal, "system__standard_library__adafinal");

      procedure Runtime_Finalize;
      pragma Import (C, Runtime_Finalize, "__gnat_runtime_finalize");

   begin
      if not Is_Elaborated then
         return;
      end if;
      Is_Elaborated := False;
      Runtime_Finalize;
      s_stalib_adafinal;
   end adafinal;

   procedure adainit is
      Main_Priority : Integer;
      pragma Import (C, Main_Priority, "__gl_main_priority");
      Time_Slice_Value : Integer;
      pragma Import (C, Time_Slice_Value, "__gl_time_slice_val");
      WC_Encoding : Character;
      pragma Import (C, WC_Encoding, "__gl_wc_encoding");
      Locking_Policy : Character;
      pragma Import (C, Locking_Policy, "__gl_locking_policy");
      Queuing_Policy : Character;
      pragma Import (C, Queuing_Policy, "__gl_queuing_policy");
      Task_Dispatching_Policy : Character;
      pragma Import (C, Task_Dispatching_Policy, "__gl_task_dispatching_policy");
      Priority_Specific_Dispatching : System.Address;
      pragma Import (C, Priority_Specific_Dispatching, "__gl_priority_specific_dispatching");
      Num_Specific_Dispatching : Integer;
      pragma Import (C, Num_Specific_Dispatching, "__gl_num_specific_dispatching");
      Main_CPU : Integer;
      pragma Import (C, Main_CPU, "__gl_main_cpu");
      Interrupt_States : System.Address;
      pragma Import (C, Interrupt_States, "__gl_interrupt_states");
      Num_Interrupt_States : Integer;
      pragma Import (C, Num_Interrupt_States, "__gl_num_interrupt_states");
      Unreserve_All_Interrupts : Integer;
      pragma Import (C, Unreserve_All_Interrupts, "__gl_unreserve_all_interrupts");
      Exception_Tracebacks : Integer;
      pragma Import (C, Exception_Tracebacks, "__gl_exception_tracebacks");
      Exception_Tracebacks_Symbolic : Integer;
      pragma Import (C, Exception_Tracebacks_Symbolic, "__gl_exception_tracebacks_symbolic");
      Detect_Blocking : Integer;
      pragma Import (C, Detect_Blocking, "__gl_detect_blocking");
      Default_Stack_Size : Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
      Default_Secondary_Stack_Size : System.Parameters.Size_Type;
      pragma Import (C, Default_Secondary_Stack_Size, "__gnat_default_ss_size");
      Bind_Env_Addr : System.Address;
      pragma Import (C, Bind_Env_Addr, "__gl_bind_env_addr");

      procedure Runtime_Initialize (Install_Handler : Integer);
      pragma Import (C, Runtime_Initialize, "__gnat_runtime_initialize");
      Binder_Sec_Stacks_Count : Natural;
      pragma Import (Ada, Binder_Sec_Stacks_Count, "__gnat_binder_ss_count");
      Default_Sized_SS_Pool : System.Address;
      pragma Import (Ada, Default_Sized_SS_Pool, "__gnat_default_ss_pool");

   begin
      if Is_Elaborated then
         return;
      end if;
      Is_Elaborated := True;
      Main_Priority := -1;
      Time_Slice_Value := 0;
      WC_Encoding := '8';
      Locking_Policy := 'C';
      Queuing_Policy := ' ';
      Task_Dispatching_Policy := 'F';
      System.Restrictions.Run_Time_Restrictions :=
        (Set =>
          (False, True, True, False, False, False, False, True, 
           False, False, False, False, False, False, False, False, 
           True, True, True, False, False, False, False, False, 
           True, False, False, False, False, False, False, False, 
           False, False, True, True, False, False, True, True, 
           False, False, False, True, False, False, False, False, 
           True, False, True, True, False, False, False, False, 
           False, True, True, True, True, True, False, False, 
           True, False, False, False, False, False, False, False, 
           False, False, False, False, False, False, False, False, 
           False, True, False, False, False, False, False, False, 
           False, False, False, False, True, True, False, True, 
           False, False),
         Value => (0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
         Violated =>
          (False, False, False, False, True, True, False, False, 
           False, False, False, True, True, True, True, False, 
           False, False, False, True, False, False, True, True, 
           False, True, True, False, True, True, False, True, 
           False, False, False, False, False, True, False, False, 
           True, False, False, False, True, True, False, False, 
           False, True, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, True, 
           False, True, True, True, True, False, True, False, 
           True, True, True, False, True, True, False, False, 
           True, True, True, False, False, False, False, False, 
           False, False, False, True, False, False, True, False, 
           True, False),
         Count => (0, 0, 0, 1, 0, 0, 2, 0, 6, 0),
         Unknown => (False, False, False, False, False, False, False, False, True, False));
      Priority_Specific_Dispatching :=
        Local_Priority_Specific_Dispatching'Address;
      Num_Specific_Dispatching := 0;
      Main_CPU := -1;
      Interrupt_States := Local_Interrupt_States'Address;
      Num_Interrupt_States := 0;
      Unreserve_All_Interrupts := 0;
      Exception_Tracebacks := 1;
      Exception_Tracebacks_Symbolic := 1;
      Detect_Blocking := 1;
      Default_Stack_Size := -1;

      ada_main'Elab_Body;
      Default_Secondary_Stack_Size := System.Parameters.Runtime_Default_Sec_Stack_Size;
      Binder_Sec_Stacks_Count := 3;
      Default_Sized_SS_Pool := Sec_Default_Sized_Stacks'Address;

      Runtime_Initialize (1);

      Ada.Strings.Text_Buffers'Elab_Spec;
      E100 := E100 + 1;
      System.Bb.Timing_Events'Elab_Spec;
      E098 := E098 + 1;
      Ada.Exceptions'Elab_Spec;
      System.Soft_Links'Elab_Spec;
      Ada.Tags'Elab_Body;
      E109 := E109 + 1;
      System.Exception_Table'Elab_Body;
      E042 := E042 + 1;
      E044 := E044 + 1;
      E016 := E016 + 1;
      Ada.Streams'Elab_Spec;
      E164 := E164 + 1;
      System.Finalization_Root'Elab_Spec;
      E171 := E171 + 1;
      Ada.Finalization'Elab_Spec;
      E169 := E169 + 1;
      System.Storage_Pools'Elab_Spec;
      E173 := E173 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E168 := E168 + 1;
      Ada.Real_Time'Elab_Body;
      E006 := E006 + 1;
      System.Pool_Global'Elab_Spec;
      E175 := E175 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E193 := E193 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E199 := E199 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E212 := E212 + 1;
      HAL.AUDIO'ELAB_SPEC;
      E217 := E217 + 1;
      HAL.BITMAP'ELAB_SPEC;
      E258 := E258 + 1;
      HAL.FRAMEBUFFER'ELAB_SPEC;
      E259 := E259 + 1;
      HAL.GPIO'ELAB_SPEC;
      E166 := E166 + 1;
      HAL.I2C'ELAB_SPEC;
      E189 := E189 + 1;
      HAL.REAL_TIME_CLOCK'ELAB_SPEC;
      E221 := E221 + 1;
      HAL.SPI'ELAB_SPEC;
      E229 := E229 + 1;
      HAL.TIME'ELAB_SPEC;
      E277 := E277 + 1;
      HAL.TOUCH_PANEL'ELAB_SPEC;
      E284 := E284 + 1;
      HAL.UART'ELAB_SPEC;
      E237 := E237 + 1;
      E268 := E268 + 1;
      ILI9341'ELAB_SPEC;
      ILI9341'ELAB_BODY;
      E275 := E275 + 1;
      L3GD20'ELAB_SPEC;
      L3GD20'ELAB_BODY;
      E281 := E281 + 1;
      Ravenscar_Time'Elab_Spec;
      Ravenscar_Time'Elab_Body;
      E279 := E279 + 1;
      Soft_Drawing_Bitmap'Elab_Spec;
      Soft_Drawing_Bitmap'Elab_Body;
      E270 := E270 + 1;
      Memory_Mapped_Bitmap'Elab_Spec;
      Memory_Mapped_Bitmap'Elab_Body;
      E266 := E266 + 1;
      STM32.ADC'ELAB_SPEC;
      E140 := E140 + 1;
      E146 := E146 + 1;
      E205 := E205 + 1;
      E252 := E252 + 1;
      STM32.DMA2D.INTERRUPT'ELAB_BODY;
      E255 := E255 + 1;
      E257 := E257 + 1;
      STM32.DMA2D_BITMAP'ELAB_SPEC;
      STM32.DMA2D_BITMAP'ELAB_BODY;
      E261 := E261 + 1;
      E159 := E159 + 1;
      E244 := E244 + 1;
      STM32.I2S'ELAB_SPEC;
      STM32.I2S'ELAB_BODY;
      E216 := E216 + 1;
      E223 := E223 + 1;
      STM32.RTC'ELAB_SPEC;
      STM32.RTC'ELAB_BODY;
      E220 := E220 + 1;
      E242 := E242 + 1;
      STM32.SPI'ELAB_SPEC;
      STM32.SPI'ELAB_BODY;
      E228 := E228 + 1;
      STM32.SPI.DMA'ELAB_SPEC;
      STM32.SPI.DMA'ELAB_BODY;
      E231 := E231 + 1;
      STM32.GPIO'ELAB_SPEC;
      STM32.I2C'ELAB_SPEC;
      STM32.I2C.DMA'ELAB_SPEC;
      E157 := E157 + 1;
      STM32.GPIO'ELAB_BODY;
      E152 := E152 + 1;
      STM32.USARTS'ELAB_SPEC;
      STM32.DEVICE'ELAB_SPEC;
      E136 := E136 + 1;
      STM32.I2C'ELAB_BODY;
      E186 := E186 + 1;
      STM32.I2C.DMA'ELAB_BODY;
      E191 := E191 + 1;
      STM32.USARTS'ELAB_BODY;
      E235 := E235 + 1;
      STM32.LTDC'ELAB_BODY;
      E272 := E272 + 1;
      Framebuffer_Ltdc'Elab_Spec;
      Framebuffer_Ltdc'Elab_Body;
      E250 := E250 + 1;
      STM32.PWM'ELAB_SPEC;
      STM32.PWM'ELAB_BODY;
      E132 := E132 + 1;
      STMPE811'ELAB_SPEC;
      STMPE811'ELAB_BODY;
      E286 := E286 + 1;
      Framebuffer_Ili9341'Elab_Spec;
      Touch_Panel_Stmpe811'Elab_Spec;
      STM32.BOARD'ELAB_SPEC;
      STM32.BOARD'ELAB_BODY;
      E240 := E240 + 1;
      Framebuffer_Ili9341'Elab_Body;
      E248 := E248 + 1;
      Touch_Panel_Stmpe811'Elab_Body;
      E283 := E283 + 1;
      Motor'Elab_Spec;
      Motor'Elab_Body;
      E130 := E130 + 1;
      Uarts'Elab_Body;
      E291 := E291 + 1;
      Uart'Elab_Spec;
      Uart'Elab_Body;
      E289 := E289 + 1;
   end adainit;

   procedure Ada_Main_Program;
   pragma Import (Ada, Ada_Main_Program, "_ada_main");

   procedure main is
      procedure Initialize (Addr : System.Address);
      pragma Import (C, Initialize, "__gnat_initialize");

      procedure Finalize;
      pragma Import (C, Finalize, "__gnat_finalize");
      SEH : aliased array (1 .. 2) of Integer;

      Ensure_Reference : aliased System.Address := Ada_Main_Program_Name'Address;
      pragma Volatile (Ensure_Reference);

   begin
      Initialize (SEH'Address);
      adainit;
      Ada_Main_Program;
      adafinal;
      Finalize;
   end;

--  BEGIN Object file/option list
   --   C:\Code\VDI-ADC-Modular-System\hardware\obj\system_config.o
   --   C:\Code\VDI-ADC-Modular-System\hardware\obj\motor.o
   --   C:\Code\VDI-ADC-Modular-System\hardware\obj\uarts.o
   --   C:\Code\VDI-ADC-Modular-System\hardware\obj\uart.o
   --   C:\Code\VDI-ADC-Modular-System\hardware\obj\main.o
   --   -LC:\Code\VDI-ADC-Modular-System\hardware\obj\
   --   -LC:\Code\VDI-ADC-Modular-System\hardware\obj\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\cortex_m_0.5.0_39667d15\476f859549ba1af1d200af7bcb2f01b3ec0faabcfe00cbc7d6402ba313b82772\lib\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\hal_0.3.1_58bb6bd7\45a7230b1521a74e4d762c42ec802710efe319e84e7227b385f535fafd1bbbac\lib\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\stm32f429disco_0.1.0_aa5fe0cb\c82c5fd28bae1b74f73b5bc408fb0d2a6c4a95b458bd5f92faf6278736579e80\lib\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\embedded_components_0.2.0_37c39b23\0bc6b1629f0f1e94583a71ab62faf08ad25f0bb4872a81641612ad56c49d554e\lib\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\adl_middleware_0.2.0_623c6913\b4ec6094b58d9cc456e277cd32fca5ea0d0a72e5eafac9d58bb4557f37b435b9\lib\
   --   -LC:\Users\kevin\AppData\Local\alire\cache\builds\stm32_hal_0.1.0_9276ee80\578fa707396e3337e837799488d7c70c27ce3a691db6465f915ac716a8ea4c9b\lib\
   --   -LC:\users\kevin\appdata\local\alire\cache\toolchains\gnat_arm_elf_12.2.1_351564ba\arm-eabi\lib\gnat\embedded-stm32f429disco\adalib\
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
