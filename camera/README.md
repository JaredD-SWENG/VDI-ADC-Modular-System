# Camera Library

A simple Ada library for sequential access to camera frames stored as QOI image files.

## Overview

The Camera library provides functionality to sequentially access camera frames stored in a specified folder. It handles frame numbering and path construction, making it easy to iterate through a sequence of frames.

## Features

- Automatic frame numbering with proper zero-padding
- Sequential access to frames
- Support for up to 954 frames
- Handles folder path with or without trailing slash

## Usage

### Initialization

Before accessing frames, initialize the library with the folder path containing the frame files:

```ada
Camera.Initialize("/path/to/frames");
```

### Accessing Frames

To get the next frame in sequence, use the `Get_Frame` function:

```ada
Frame_Path : String;
Frame_Path := Camera.Get_Frame;

-- Check if we've reached the end of available frames
if Frame_Path = "" then
   -- No more frames
else
   -- Process the frame at Frame_Path
end if;
```

### Frame Naming Convention

The library expects frames to be named in the format:
- `frame_0000.qoi` through `frame_0953.qoi` (edit the last frame number if you have more or less)

## Limitations
- Frame files must be in QOI format
- Frame numbering must follow the specified pattern
