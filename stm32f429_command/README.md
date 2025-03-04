


###  *.gpr Add Runtime
```bash
for Runtime ("Ada") use "embedded-stm32f429disco";
```
### To See List of ARM Runtimes
```bash
ls ~/.local/share/alire/toolchains/gnat_arm_elf_13.1.0*/arm-eabi/lib/gnat/
```

### To Select Toolchain and Version
```bash
alr toolchain --select gnat_arm_elf=12.2.1 --local
```
### To See List of ARM Runtimes
```bash
ls ~/.local/share/alire/toolchains/gnat_arm_elf_13.1.0*/arm-eabi/lib/gnat/
```
### Remove Runtime not Being Used
```Bash
rm -rf ~/.local/share/alire/toolchains/gnat_arm_elf_12.2.1*
```

   -- Hardware constants (board-specific)
   Baud_Rate    : constant := 115200;  -- UART baud rate for command interface
   -- LED pins: On STM32F429I-DISCO, LD3 (green) is PG13, LD4 (red) is PG14.
   -- The Ada Drivers Library provides named constants for these pins, e.g., PG13, PG14.

   -- Task priorities (relative values for illustration; Jorvik uses fixed-priority scheduling).
   UART_Task_Priority  : constant System.Priority := 5;   -- lowest priority (background UART polling)
   Motor_Task_Priority : constant System.Priority := 7;   -- higher than UART for timely motor control
   Servo_Task_Priority : constant System.Priority := 7;   -- same as Motor (both are consumers)
   LED_Task_Priority   : constant System.Priority := 6;   -- middle priority for heartbeat LED



### **Toolchain**
1. **Compiler (GCC)**  
   - Translates Ada (or C, C++) into assembly/machine code.  
   - Example: `arm-eabi-gcc` (C compiler), `arm-eabi-gnat` (Ada compiler).

2. **Assembler**  
   - Converts assembly language into machine code (binary).  
   - Example: `as` (GNU assembler).

3. **Linker**  
   - Combines compiled code into an executable program.  
   - Example: `arm-eabi-ld`.

4. **Libraries**  
   - Provides standard functions (e.g., math, I/O, runtime).  
   - Example: Ada runtime (`ravenscar-stm32f4`).

5. **Debugger & Programmer Tools**  
   - Helps in debugging & flashing firmware.  
   - Example: `arm-eabi-gdb`, `openocd`, `st-flash`.

---

Toolchain **GNAT ARM ELF** (`gnat_arm_elf`) is a **cross-toolchain**

It produces code for **STM32 (ARM Cortex-M4)**  

---

1. **Not All Toolchains Support Embedded Targets**  
   - GNAT **native** is for desktops.  
   - GNAT **ARM ELF** is for embedded microcontrollers.  

2. **Correct Runtime & Libraries Must Match the Toolchain**  
   - `embedded-stm32f4` might work in **12.2.1**, but not in **13.1.0**.

---
