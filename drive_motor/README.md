# Drive Motor Package - VDI-ADC Modular System

**Capstone Project:** Modular Autonomous Driving System for VDI Challenge Project  
**Hardware Platform:** STM32F429 Discovery Board  
**Programming Language:** Ada

---

## 📋 Overview

The **Drive Motor Package** controls motor speed, direction, and power using **PWM signals** on the **STM32F429 Discovery Board**. It supports:

- PWM control with configurable frequency and duty cycle
- Soft stops and emergency stop functionality
- MOSFET-based power control
- UART-based logging for debugging and testing

---

## ⚡ PWM Configuration

**PWM is generated using STM32 Timers and GPIO pins.** Below are the pin configurations for PWM signals:

| Function  | Timer   | GPIO Pin | Timer Channel | Alternate Function |
|-----------|--------|----------|---------------|---------------------|
| Drive PWM | Timer 4 | PB7      | Channel 2     | GPIO_AF_TIM4_2     |
| Drive Mosfet | N/A | PC8      | N/A     | N/A     |

---

## 📊 PWM Output Examples

[Demo Video](https://psu.mediaspace.kaltura.com/media/drive_test_19FEB2025/1_2dv4cfyu)

---

## 🏗️ Hardware Architecture

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

class Motor_Driver {
    +Initialize(Timer: Timers.Timer, Pin: GPIO.GPIO_Point, Channel: Timers.Timer_Channel, GPIO_AF: GPIO_Alternate_Function, Frequency: PWM.Hertz)
    +Set_Frequency(Frequency: PWM.Hertz)
    +Set_Duty_Cycle_Us(Time_Us: PWM.Microseconds)
    +Set_Duty_Cycle_Percentage(Percentage: PWM.Percentage)
    +Set_Speed(Speed_Percentage : Integer);
    +Enable()
    +Disable()
    +Emergency_Stop()
}

class pwm_control {
    +Initialize(PWM_Pin:PWM)
    +Enable_Output(PWM_Pin:PWM)
}

class Main {
    +Initialize_Motor()
}

stm32f429disco --> cortex-m : depends_on
stm32f429disco --> embedded_components : depends_on
stm32f429disco --> stm32_hal : depends_on

hal <|-- stm32_hal : extends

stm32_hal *--> Timers
stm32_hal *--> Device
stm32_hal *--> PWM
stm32_hal *--> GPIO

Motor_Driver *--> PWM : uses
Motor_Driver *--> Timers : controls

Main *--> Motor_Driver : instantiates
Main *--> Timers : uses
```

## Useful Information

### Show all connected communication devices
```bash
ls /dev/tty.*
```

### Connect via COM port using screen
```bash
screen /dev/tty.usbmodem2303 115200
```

### Using OpenOCD to write to STM32
```bash
openocd -f "/openocd/scripts/board/stm32f429discovery.cfg" -c "program /VDI-ADC-Modular-System/drive_motor/bin/main verify reset exit"
```