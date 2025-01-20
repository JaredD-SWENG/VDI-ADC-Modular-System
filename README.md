# VDI-ADC-Modular-System
Capstone Project: Modular Autonomous Driving System for VDI Challenge Project 

## Selected PWM Pin Mappings

After reviewing the STM32F429 datasheet and the board documentation, the following pin mappings were selected for PWM control:

| Function  | Timer   | GPIO Pin | Timer Channel | Alternate Function |
|-----------|--------|----------|---------------|---------------------|
| Drive PWM | Timer 4 | PB7      | Channel 2     | GPIO_AF_TIM4_2     |
| Drive Mosfet | N/A | PC8      | N/A     | N/A     |
| Steering PWM | Timer 2 | PA5      | Channel 1     | GPIO_AF_TIM2_1     |



### References:
- **Board Documentation:** "um1670-discovery-kit-with-stm32f429zi-mcu-stmicroelectronics.pdf"  
  - Table 7: STM32 Pin Description vs. Board Functions.
- **Microcontroller Datasheet:** "32bit Arm Cortex M4 MCU - stm32f427vg.pdf"  
  - Table 12: STM32F427xx and STM32F429xx Alternate Function Mapping.

These pins were chosen because they are not connected to other devices on the STM32F429 Discovery board, ensuring they are available for PWM signal output.

