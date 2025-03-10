# VDI-ADC-Modular-System
Capstone Project: Modular Autonomous Driving System for VDI Challenge Project 

## PWM using Ada on STM32F429Disco

Here’s the updated Markdown table with only the two pins, leaving the function column blank for now:

| Function | Timer   | GPIO Pin | Timer Channel | Alternate Function |
|----------|---------|----------|---------------|---------------------|
|          | Timer_4 | PB7      | Channel_2     | GPIO_AF_TIM4_2     |
|          | Timer_2 | PA5      | Channel_1     | GPIO_AF_TIM2_1     |

## Outputs

### 50Hz 1ms/10% Duty Cycle

![50Hz 1ms/10% Duty Cycle](<boundary_test_captures/2025-01-15 50Hz 1ms Ouput.png>)

### 50Hz 2ms/20% Duty Cycle

![50Hz 2ms/20% Duty Cycle](<boundary_test_captures/2025-01-15 50Hz 2ms Ouput.png>)

---

## Hardware Architecture

```mermaid
classDiagram

class stm32f429disco
class cortex-m

class embedded_components

class stm32_hal {
    +GPIO_Alternate_Function
}

class hal {
    <<abstract>>
    +Integer
}

class Timers {
    +Timer
}

class PWM {
    +PWM_Modulator
}

class Device {
    +PA5:GPIO_Point
    +Timer_2:Timer_Channel
    +GPIO_AF_TIM2_1:GPIO_Alternate_Function
}

class GPIO {
    +GPIO_Point
}

class Motor_Base {
    <<abstract>>
    +Set_Frequency(Frequency: PWM.Hertz)
    +Set_Duty_Cycle_Us(Time_Us: PWM.Microseconds)
    +Set_Duty_Cycle_Percentage(Percentage: PWM.Percentage)
    +Enable()
    +Disable()
}

class Motor_Driver {
    +Initialize(Timer: Timers.Timer, Pin: GPIO.GPIO_Point, Channel: Timers.Timer_Channel, GPIO_AF: GPIO_Alternate_Function, Frequency: PWM.Hertz)
    +Set_Frequency(Frequency: PWM.Hertz)
    +Set_Duty_Cycle_Us(Time_Us: PWM.Microseconds)
    +Set_Duty_Cycle_Percentage(Percentage: PWM.Percentage)
    +Enable()
    +Disable()
}

class pwm_control {
    +Initialize(PWM_Pin:PWM, 
    +Enable_Output(PWM_Pin:PWM)
}

class Main {
    +Initialize_Motor()
    +Randomized_Control_Loop()
}

stm32f429disco --> cortex-m : depends_on
stm32f429disco --> embedded_components : depends_on
stm32f429disco --> stm32_hal : depends_on

hal <|-- stm32_hal : extends

stm32_hal *--> Timers
stm32_hal *--> Device
stm32_hal *--> PWM
stm32_hal *--> GPIO

Motor_Base <|-- Motor_Driver : extends
Motor_Driver *--> PWM : uses
Motor_Driver *--> Timers : controls

Main *--> Motor_Driver : instantiates
Main *--> Timers : uses

```

## Useful Information

```bash
    alr index 

    # NAME      URL                                                           
    1 gap       git+https://github.com/GNAT-Academic-Program/alire-index      
    2 community git+https://github.com/alire-project/alire-index#stable-1.3.0 

    alr toolchain --select gnat_arm_elf=12.2.1

    openocd -f "C:/xpack-openocd-0.12.0-4/openocd/scripts/board/stm32f429disc1.cfg" -c "program C:/Code/VDI-ADC-Modular-System/pwm_control/bin/main verify reset exit"
```
                 


