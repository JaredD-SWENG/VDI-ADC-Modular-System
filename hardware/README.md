# Drive Motor Package - VDI-ADC Modular System

**Capstone Project:** Modular Autonomous Driving System for VDI Challenge Project  
**Hardware Platform:** STM32F429 Discovery Board  
**Programming Language:** Ada

---

##  Overview

The **Drive Motor Package** controls motor speed, direction, and power using **PWM signals** on the **STM32F429 Discovery Board**. It supports:

- PWM control with configurable frequency and duty cycle
- Soft stops and emergency stop functionality
- MOSFET-based power control
- UART-based logging for debugging and testing

---

## Selected Discovery Board Pin Mappings

After reviewing the STM32F429 datasheet and the board documentation, the following pin mappings were selected for PWM control (reference schematic):

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

### Notes

Add the Alire index directory to Git's safe.directory lis due to a a security measure implemented in Git versions 2.35.2 and newer to prevent potential vulnerabilities:

```bash
git config --global --add safe.directory C:/Users/username/AppData/Local/alire/settings/indexes/community/repo
```

```bash
alr with stm32_hal
```

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

### Using OpenOCD to write to STM32

```bash
openocd -f "/openocd/scripts/board/stm32f429discovery.cfg" -c "program /VDI-ADC-Modular-System/drive_motor/bin/main verify reset exit"
```

## Testing

### Test TC-025 Output

```terminal
 === TC-025: Acceleration Response Test ===
Connect scope to PB7. Press [y] to continue or [n] to abort...
yMotor Initialized.
Motor Enabled (via Enable procedure).
Motor Power On - Calibrating...
Calibrated
Setting 5% speed...
TAKE SCOPE CAPTURE NOW (5% Speed). Press y to continue...
yAcceleration time: Time:  0s  3533ns - TAKE 20% CAPTURE NOW
Press ENTER to power down...
```

### Test TC-026 Output

```terminal
 === TC-026: Deceleration Response Test ===
Connect scope to PB7. Press [y] to continue or [n] to abort...
yMotor Initialized.
Motor Enabled (via Enable procedure).
Motor Power On - Calibrating...
Calibrated
Setting 20% speed...
TAKE SCOPE CAPTURE NOW (20% Speed). Press y to decelerate...
yDeceleration time: Time:  0s  3500ns - TAKE 5% SPEED CAPTURE NOW
Press y to power down...
```

### Test TC-027 Output

```terminal
  === TC-027: Emergency Stop Test ===
Connect scope to PB7. Press [y] to continue or [n] to abort...
yMotor Initialized.
Motor Enabled (via Enable procedure).
Motor Power On - Calibrating...
Calibrated
Running at 20% speed...
TAKE SCOPE CAPTURE NOW (20% Speed). Press y to trigger emergency stop...
yEMERGENCY STOP - Motor Power Cut!
Emergency stop time: Time:  0s  100556150ns - VERIFY MOTOR STOPPED IMMEDIATELY
Press y to power down...
```

### Test TC-028 Output

```terminal
 === TC-028: Speed Limit Enforcement Test ===
Connect scope to PB7. Follow prompts to verify results.
Motor Initialized.
Motor Enabled (via Enable procedure).
Motor Power On - Calibrating...
Calibrated
Applying speed: 10% Below Bottom Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: Bottom Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: 10% Above Bottom Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: Mid-range (50%)
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: 10% Below Upper Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: Upper Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Applying speed: 10% Above Upper Limit
Verify scope reflects expected result.
Did the driver correctly handle this input? (y/n)
yResult: PASS
Motor Stop invoked (min duty).
Motor Disabled (via Disable procedure).
Test complete. Power off scope.
```

### Test TC-029 Output

```terminal
 === TC-029: Frequency Stability Test ===
Connect scope to PB7. Verify PWM frequency remains constant.
Motor Initialized.
Motor Enabled (via Enable procedure).
Motor Power On - Calibrating...
Calibrated
Setting speed to 10%Verify PWM frequency remained constant.
Did frequency remain stable? (y/n)
yResult: PASS
Setting speed to 30%Verify PWM frequency remained constant.
Did frequency remain stable? (y/n)
yResult: PASS
Setting speed to 50%Verify PWM frequency remained constant.
Did frequency remain stable? (y/n)
yResult: PASS
Setting speed to 70%Verify PWM frequency remained constant.
Did frequency remain stable? (y/n)
yResult: PASS
Setting speed to 90%Verify PWM frequency remained constant.
Did frequency remain stable? (y/n)
yResult: PASS
Motor Stop invoked (min duty).
Motor Disabled (via Disable procedure).
Test complete. Power off scope.
```