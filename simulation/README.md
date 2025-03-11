# Event-Driven Architecture Simulation

## Overview

This project implements a simulation of an autonomous vehicle's event processing system using Ada. The simulation demonstrates how an autonomous vehicle might handle different types of events with varying priorities, such as traffic signal recognition and lane offset detection.

## Features

- **Priority-based Event Handling**: Signal events take precedence over lane offset events
- **Event Queue Management**: Efficiently manages events in a protected queue
- **Concurrent Task Architecture**: Uses Ada's tasking model to simulate real-time event processing
- **Preemptive Processing**: Higher priority events can interrupt the processing of lower priority events

## System Components

### Event Types
- **Signal Events**: Represent traffic signals (Red, Yellow, Green)
- **Offset Events**: Represent lane position offsets (negative = right, positive = left)

### Tasks
- **Lane_Detection**: Generates lane offset events at regular intervals
- **Signal_Recognition**: Generates traffic signal events at regular intervals
- **Event_Broker**: Coordinates event processing and manages task priorities
- **Path_Planning**: Processes events and simulates vehicle responses

### Protected Objects
- **Event_Queue**: Thread-safe queue implementation with priority-based dequeuing

## How It Works

1. Producer tasks (`Lane_Detection` and `Signal_Recognition`) generate events and add them to the shared queue
2. The `Event_Broker` monitors the queue and initiates processing
3. If a high-priority signal event arrives during processing, the broker interrupts the current processing
4. The `Path_Planning` task processes events and simulates appropriate vehicle responses:
   - For signals: Stop (Red), Slow (Yellow), or Go (Green)
   - For lane offsets: Turn Left, Turn Right, or Stay Centered

## Implementation Details

- Uses Ada's protected objects for thread-safe queue operations
- Implements a custom priority mechanism where signal events take precedence
- Utilizes Ada's rendezvous mechanism for task coordination
- Simulates varying processing times for different event types



