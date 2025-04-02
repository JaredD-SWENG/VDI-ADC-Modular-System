# Creating an Ada Project for STM32F429 Discovery on Windows

This guide will walk you through setting up an Ada project from scratch on a Windows computer to program the STM32F429 Discovery board. We'll cover installing necessary tools, configuring environment variables, setting up project files, writing Ada code, building the project, and programming the board using OpenOCD.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Step 1: Install the Ada Toolchain (GNAT)](#step-1-install-the-ada-toolchain-gnat)
3. [Step 2: Install ALR (Ada Library Manager)](#step-2-install-alr-ada-library-manager)
4. [Step 3: Install OpenOCD](#step-3-install-openocd)
5. [Step 4: Install ST-Link Drivers with Zadig](#step-4-install-st-link-drivers-with-zadig)
6. [Step 5: Configure Environment Variables](#step-5-configure-environment-variables)
7. [Step 6: Initialize the Ada Project](#step-6-initialize-the-ada-project)
8. [Step 7: Configure GNAT Project File (`.gpr`)](#step-7-configure-gnat-project-file-gpr)
9. [Step 8: Configure ALR (`alr.toml`)](#step-8-configure-alr-alrtoml)
10. [Step 9: Write Ada Code](#step-9-write-ada-code)
11. [Step 10: Build the Project](#step-10-build-the-project)
12. [Step 11: Program the STM32F429 Discovery Board](#step-11-program-the-stm32f429-discovery-board)
13. [Preparing for PWM Pin Creation](#preparing-for-pwm-pin-creation)
14. [Configuring the IDE](#configuring-the-ide)
15. [Bootloader Integration](#bootloader-integration)
16. [Troubleshooting](#troubleshooting)
17. [Conclusion](#conclusion)

---

## Prerequisites

Before proceeding, ensure you have the following:

- **Windows Computer:** Running Windows 10 or later.
- **STM32F429 Discovery Board:** Ensure it's functional and you have the necessary USB cables.
- **Internet Connection:** Required to download software and dependencies.
- **Administrative Privileges:** Needed for installing software and drivers.

---

## Step 1: Install ALR (Ada Library Manager)

ALR manages Ada project dependencies and configurations.

1. **Download ALR:**
   - Visit the [ALR GitHub Repository](https://gitlab.com/alr/alr).
   - Download the latest Windows release (e.g., `alr-windows-x86_64.zip`).

2. **Install ALR:**
   - Extract the contents of the ZIP file to a directory, e.g., `C:\xpack-alr`.
   - Add `C:\xpack-alr\bin\` to your system PATH.

3. **Verify Installation:**
   - Run:
     ```sh
     alr --version
     ```

---

## Step 2: Install OpenOCD

OpenOCD is used for programming and debugging the STM32F429 Discovery board.

1. **Download OpenOCD:**
   - Visit the [xPack OpenOCD GitHub Releases](https://github.com/xpack-dev-tools/openocd-xpack/releases).

2. **Install OpenOCD:**
   - Extract to `C:\xpack-openocd-0.12.0-4`.
   - Add `C:\xpack-openocd-0.12.0-4\openocd\bin\` to your system PATH.

3. **Verify Installation:**
   - Run:
     ```sh
     openocd --version
     ```

---

## Step 3: Install ST-Link Drivers with Zadig

To communicate with the STM32F429 Discovery board:

1. **Download Zadig:**
   - Visit the [Zadig website](https://zadig.akeo.ie/).

2. **Install the Drivers:**
   - Select your STM32 device in Zadig.
   - Replace its driver with WinUSB (v6.x.x.x).

3. **Verify Installation:**
   - Confirm in Device Manager that the STM32 board is recognized.

---

## Step 5: Configure Environment Variables

Ensure tools like OpenOCD can locate scripts by setting environment variables.

1. Add `OPENOCD_TCL_PATH`:
   ```
   C:\xpack-openocd-0.12.0-4\openocd\scripts\
   ```

---

## Step 6: Initialize the Ada Project

1. Create a project directory, e.g., `C:\Code\alr`.
2. Initialize an Ada project:
   ```sh
   alr init --bin pwm_control
   ```

---

## Step 7: Configure GNAT Project File (`.gpr`)

Modify the `.gpr` file to include:

1. Device target:
   ```ada
   for Target use "arm-eabi";
   ```
2. Runtime:
   ```ada
   for Runtime ("Ada") use "embedded-stm32f429disco";
   ```

---

## Step 8: Configure ALR (`alr.toml`)

Edit `alr.toml` to manage dependencies and pin the cross-compiler version:

```toml
[[depends-on]]
stm32f429disco = "0.1.0"

[[pins]]
gnat_arm_elf = "12.2.1"
```

---

## Step 9: Write Ada Code

Write or update Ada code in the `src` directory.

---

## Step 10: Build the Project

1. Resolve dependencies:
   ```sh
   alr resolve
   ```
2. Build the project:
   ```sh
   alr build
   ```

---

## Step 11: Program the STM32F429 Discovery Board

1. Connect the board and locate the binary (e.g., `bin/main`).
2. Use OpenOCD to flash the firmware:
   ```sh
   openocd -f "path/to/stm32f429disc1.cfg" -c "program bin/main verify reset exit"
   ```

---

## Step 12: Preparing for PWM Pin Creation

Define PWM functionality by specifying pins in the `.gpr` and `.ads` files. Ensure to set:

- GPIO Pin configurations.
- Alternate functions for PWM generation.

---

## Step 13: Configuring the IDE

Use an IDE like GNAT Studio to streamline development:

1. Open the `.gpr` project file.
2. Configure build and debug settings.
3. Integrate OpenOCD for flashing and debugging.

---

## Step 14: Bootloader Integration

Set up a bootloader to enable easy firmware updates:

1. Include the bootloader binary in the build pipeline.
2. Modify `.gpr` to link the bootloader and main application.
3. Ensure flash memory addresses do not overlap.

---

## Useful Information

```bash
alr index
alr toolchain --select gnat_arm_elf=12.2.1
openocd -f "path/to/stm32f429disc1.cfg" -c "program bin/main verify reset exit"
```

