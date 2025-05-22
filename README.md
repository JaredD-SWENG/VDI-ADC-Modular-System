# VDI-ADC-Modular-System
Capstone Project: Modular Autonomous Driving System for VDI Challenge Project

**[Project Website](https://sites.psu.edu/behrendseniordesign/2025/04/28/vdi-autonomous-driving-challenge/)**

## Demo Video
[![VDI-ADC-Modular-System Demo](https://github.com/user-attachments/assets/a2d15290-aac0-46cb-83b5-398e4d8e0d32)](https://www.youtube.com/watch?v=pCQ_vPEtTcA)

## Overview
A modular autonomous driving system developed for the VDI Challenge Project. The system provides a flexible, robust framework for implementing and testing various autonomous driving components using Ada programming language.

## Key Features
- **Event-Driven Architecture**: Prioritized handling of different event types
- **Computer Vision Library**: Ada implementation with comprehensive image processing capabilities
- **Modular Design**: Easily replaceable and testable components
- **Simulation Environment**: Test autonomous driving algorithms in a safe environment

## Repository Structure
- **`/simulation`**: Event-driven architecture simulation for autonomous vehicle systems
- **`/cv_ada`**: Computer vision library implemented in Ada
- **`/camera`**: Library for sequential access to camera frames
- **`/hardware`**: STM32 firmware for vehicle control (steering, motor, UART communication)
- **`/host_serial`**: Host-side serial communication library for PC to STM32 interface

## Getting Started
1. Clone the repository
2. Install Ada compiler (GNAT)
3. Set up the required dependencies
4. Build the system
5. Run the simulation or deploy to your target hardware

## Documentation
- See individual module READMEs for specific documentation
- Implementation details available in code comments
- System architecture overview in `/simulation/README.md`

## Acknowledgments
- Pennsylvania State University
- AdaCore

