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
   E162 : Short_Integer; pragma Import (Ada, E162, "ada__streams_E");
   E169 : Short_Integer; pragma Import (Ada, E169, "system__finalization_root_E");
   E167 : Short_Integer; pragma Import (Ada, E167, "ada__finalization_E");
   E171 : Short_Integer; pragma Import (Ada, E171, "system__storage_pools_E");
   E166 : Short_Integer; pragma Import (Ada, E166, "system__finalization_masters_E");
   E006 : Short_Integer; pragma Import (Ada, E006, "ada__real_time_E");
   E173 : Short_Integer; pragma Import (Ada, E173, "system__pool_global_E");
   E191 : Short_Integer; pragma Import (Ada, E191, "system__tasking__protected_objects_E");
   E197 : Short_Integer; pragma Import (Ada, E197, "system__tasking__protected_objects__multiprocessors_E");
   E210 : Short_Integer; pragma Import (Ada, E210, "system__tasking__restricted__stages_E");
   E215 : Short_Integer; pragma Import (Ada, E215, "hal__audio_E");
   E260 : Short_Integer; pragma Import (Ada, E260, "hal__bitmap_E");
   E261 : Short_Integer; pragma Import (Ada, E261, "hal__framebuffer_E");
   E164 : Short_Integer; pragma Import (Ada, E164, "hal__gpio_E");
   E187 : Short_Integer; pragma Import (Ada, E187, "hal__i2c_E");
   E219 : Short_Integer; pragma Import (Ada, E219, "hal__real_time_clock_E");
   E227 : Short_Integer; pragma Import (Ada, E227, "hal__spi_E");
   E279 : Short_Integer; pragma Import (Ada, E279, "hal__time_E");
   E286 : Short_Integer; pragma Import (Ada, E286, "hal__touch_panel_E");
   E235 : Short_Integer; pragma Import (Ada, E235, "hal__uart_E");
   E270 : Short_Integer; pragma Import (Ada, E270, "bitmap_color_conversion_E");
   E277 : Short_Integer; pragma Import (Ada, E277, "ili9341_E");
   E283 : Short_Integer; pragma Import (Ada, E283, "l3gd20_E");
   E281 : Short_Integer; pragma Import (Ada, E281, "ravenscar_time_E");
   E272 : Short_Integer; pragma Import (Ada, E272, "soft_drawing_bitmap_E");
   E268 : Short_Integer; pragma Import (Ada, E268, "memory_mapped_bitmap_E");
   E138 : Short_Integer; pragma Import (Ada, E138, "stm32__adc_E");
   E144 : Short_Integer; pragma Import (Ada, E144, "stm32__dac_E");
   E203 : Short_Integer; pragma Import (Ada, E203, "stm32__dma__interrupts_E");
   E254 : Short_Integer; pragma Import (Ada, E254, "stm32__dma2d_E");
   E257 : Short_Integer; pragma Import (Ada, E257, "stm32__dma2d__interrupt_E");
   E259 : Short_Integer; pragma Import (Ada, E259, "stm32__dma2d__polling_E");
   E263 : Short_Integer; pragma Import (Ada, E263, "stm32__dma2d_bitmap_E");
   E157 : Short_Integer; pragma Import (Ada, E157, "stm32__exti_E");
   E246 : Short_Integer; pragma Import (Ada, E246, "stm32__fmc_E");
   E214 : Short_Integer; pragma Import (Ada, E214, "stm32__i2s_E");
   E221 : Short_Integer; pragma Import (Ada, E221, "stm32__power_control_E");
   E218 : Short_Integer; pragma Import (Ada, E218, "stm32__rtc_E");
   E244 : Short_Integer; pragma Import (Ada, E244, "stm32__sdram_E");
   E226 : Short_Integer; pragma Import (Ada, E226, "stm32__spi_E");
   E229 : Short_Integer; pragma Import (Ada, E229, "stm32__spi__dma_E");
   E150 : Short_Integer; pragma Import (Ada, E150, "stm32__gpio_E");
   E184 : Short_Integer; pragma Import (Ada, E184, "stm32__i2c_E");
   E189 : Short_Integer; pragma Import (Ada, E189, "stm32__i2c__dma_E");
   E155 : Short_Integer; pragma Import (Ada, E155, "stm32__syscfg_E");
   E233 : Short_Integer; pragma Import (Ada, E233, "stm32__usarts_E");
   E132 : Short_Integer; pragma Import (Ada, E132, "stm32__device_E");
   E274 : Short_Integer; pragma Import (Ada, E274, "stm32__ltdc_E");
   E252 : Short_Integer; pragma Import (Ada, E252, "framebuffer_ltdc_E");
   E238 : Short_Integer; pragma Import (Ada, E238, "stm32__pwm_E");
   E288 : Short_Integer; pragma Import (Ada, E288, "stmpe811_E");
   E250 : Short_Integer; pragma Import (Ada, E250, "framebuffer_ili9341_E");
   E285 : Short_Integer; pragma Import (Ada, E285, "touch_panel_stmpe811_E");
   E242 : Short_Integer; pragma Import (Ada, E242, "stm32__board_E");
   E130 : Short_Integer; pragma Import (Ada, E130, "motor_E");
   E291 : Short_Integer; pragma Import (Ada, E291, "steering_E");
   E295 : Short_Integer; pragma Import (Ada, E295, "uarts_E");
   E293 : Short_Integer; pragma Import (Ada, E293, "uart_E");

   Sec_Default_Sized_Stacks : array (1 .. 4) of aliased System.Secondary_Stack.SS_Stack (System.Parameters.Runtime_Default_Sec_Stack_Size);

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
           True, False, False, False, True, True, False, True, 
           False, True, False, False, False, True, False, False, 
           False, False, False, False, False, False, False, True, 
           False, True, True, True, True, False, True, False, 
           True, True, True, False, True, True, False, False, 
           True, True, True, False, False, False, False, False, 
           False, False, False, True, False, False, True, False, 
           True, False),
         Count => (0, 0, 0, 1, 0, 0, 3, 0, 7, 0),
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
      Binder_Sec_Stacks_Count := 4;
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
      E162 := E162 + 1;
      System.Finalization_Root'Elab_Spec;
      E169 := E169 + 1;
      Ada.Finalization'Elab_Spec;
      E167 := E167 + 1;
      System.Storage_Pools'Elab_Spec;
      E171 := E171 + 1;
      System.Finalization_Masters'Elab_Spec;
      System.Finalization_Masters'Elab_Body;
      E166 := E166 + 1;
      Ada.Real_Time'Elab_Body;
      E006 := E006 + 1;
      System.Pool_Global'Elab_Spec;
      E173 := E173 + 1;
      System.Tasking.Protected_Objects'Elab_Body;
      E191 := E191 + 1;
      System.Tasking.Protected_Objects.Multiprocessors'Elab_Body;
      E197 := E197 + 1;
      System.Tasking.Restricted.Stages'Elab_Body;
      E210 := E210 + 1;
      HAL.AUDIO'ELAB_SPEC;
      E215 := E215 + 1;
      HAL.BITMAP'ELAB_SPEC;
      E260 := E260 + 1;
      HAL.FRAMEBUFFER'ELAB_SPEC;
      E261 := E261 + 1;
      HAL.GPIO'ELAB_SPEC;
      E164 := E164 + 1;
      HAL.I2C'ELAB_SPEC;
      E187 := E187 + 1;
      HAL.REAL_TIME_CLOCK'ELAB_SPEC;
      E219 := E219 + 1;
      HAL.SPI'ELAB_SPEC;
      E227 := E227 + 1;
      HAL.TIME'ELAB_SPEC;
      E279 := E279 + 1;
      HAL.TOUCH_PANEL'ELAB_SPEC;
      E286 := E286 + 1;
      HAL.UART'ELAB_SPEC;
      E235 := E235 + 1;
      E270 := E270 + 1;
      ILI9341'ELAB_SPEC;
      ILI9341'ELAB_BODY;
      E277 := E277 + 1;
      L3GD20'ELAB_SPEC;
      L3GD20'ELAB_BODY;
      E283 := E283 + 1;
      Ravenscar_Time'Elab_Spec;
      Ravenscar_Time'Elab_Body;
      E281 := E281 + 1;
      Soft_Drawing_Bitmap'Elab_Spec;
      Soft_Drawing_Bitmap'Elab_Body;
      E272 := E272 + 1;
      Memory_Mapped_Bitmap'Elab_Spec;
      Memory_Mapped_Bitmap'Elab_Body;
      E268 := E268 + 1;
      STM32.ADC'ELAB_SPEC;
      E138 := E138 + 1;
      E144 := E144 + 1;
      E203 := E203 + 1;
      E254 := E254 + 1;
      STM32.DMA2D.INTERRUPT'ELAB_BODY;
      E257 := E257 + 1;
      E259 := E259 + 1;
      STM32.DMA2D_BITMAP'ELAB_SPEC;
      STM32.DMA2D_BITMAP'ELAB_BODY;
      E263 := E263 + 1;
      E157 := E157 + 1;
      E246 := E246 + 1;
      STM32.I2S'ELAB_SPEC;
      STM32.I2S'ELAB_BODY;
      E214 := E214 + 1;
      E221 := E221 + 1;
      STM32.RTC'ELAB_SPEC;
      STM32.RTC'ELAB_BODY;
      E218 := E218 + 1;
      E244 := E244 + 1;
      STM32.SPI'ELAB_SPEC;
      STM32.SPI'ELAB_BODY;
      E226 := E226 + 1;
      STM32.SPI.DMA'ELAB_SPEC;
      STM32.SPI.DMA'ELAB_BODY;
      E229 := E229 + 1;
      STM32.GPIO'ELAB_SPEC;
      STM32.I2C'ELAB_SPEC;
      STM32.I2C.DMA'ELAB_SPEC;
      E155 := E155 + 1;
      STM32.GPIO'ELAB_BODY;
      E150 := E150 + 1;
      STM32.USARTS'ELAB_SPEC;
      STM32.DEVICE'ELAB_SPEC;
      E132 := E132 + 1;
      STM32.I2C'ELAB_BODY;
      E184 := E184 + 1;
      STM32.I2C.DMA'ELAB_BODY;
      E189 := E189 + 1;
      STM32.USARTS'ELAB_BODY;
      E233 := E233 + 1;
      STM32.LTDC'ELAB_BODY;
      E274 := E274 + 1;
      Framebuffer_Ltdc'Elab_Spec;
      Framebuffer_Ltdc'Elab_Body;
      E252 := E252 + 1;
      STM32.PWM'ELAB_SPEC;
      STM32.PWM'ELAB_BODY;
      E238 := E238 + 1;
      STMPE811'ELAB_SPEC;
      STMPE811'ELAB_BODY;
      E288 := E288 + 1;
      Framebuffer_Ili9341'Elab_Spec;
      Touch_Panel_Stmpe811'Elab_Spec;
      STM32.BOARD'ELAB_SPEC;
      STM32.BOARD'ELAB_BODY;
      E242 := E242 + 1;
      Framebuffer_Ili9341'Elab_Body;
      E250 := E250 + 1;
      Touch_Panel_Stmpe811'Elab_Body;
      E285 := E285 + 1;
      Motor'Elab_Spec;
      Motor'Elab_Body;
      E130 := E130 + 1;
      Steering'Elab_Spec;
      Steering'Elab_Body;
      E291 := E291 + 1;
      Uarts'Elab_Body;
      E295 := E295 + 1;
      Uart'Elab_Spec;
      Uart'Elab_Body;
      E293 := E293 + 1;
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
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\system_config.o
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\motor.o
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\steering.o
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\uarts.o
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\uart.o
   --   C:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\main.o
   --   -LC:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\
   --   -LC:\Users\Truax\Desktop\Reference Capstone\VDI-ADC-Modular-System\hardware\obj\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\cortex_m_0.5.0_39667d15\476f859549ba1af1d200af7bcb2f01b3ec0faabcfe00cbc7d6402ba313b82772\lib\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\hal_0.3.1_58bb6bd7\45a7230b1521a74e4d762c42ec802710efe319e84e7227b385f535fafd1bbbac\lib\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\stm32f429disco_0.1.0_aa5fe0cb\c82c5fd28bae1b74f73b5bc408fb0d2a6c4a95b458bd5f92faf6278736579e80\lib\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\embedded_components_0.2.0_37c39b23\0bc6b1629f0f1e94583a71ab62faf08ad25f0bb4872a81641612ad56c49d554e\lib\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\adl_middleware_0.2.0_623c6913\b4ec6094b58d9cc456e277cd32fca5ea0d0a72e5eafac9d58bb4557f37b435b9\lib\
   --   -LC:\Users\Truax\AppData\Local\alire\cache\builds\stm32_hal_0.1.0_9276ee80\578fa707396e3337e837799488d7c70c27ce3a691db6465f915ac716a8ea4c9b\lib\
   --   -LC:\users\truax\appdata\local\alire\cache\toolchains\gnat_arm_elf_12.2.1_351564ba\arm-eabi\lib\gnat\embedded-stm32f429disco\adalib\
   --   -static
   --   -lgnarl
   --   -lgnat
--  END Object file/option list   

end ada_main;
