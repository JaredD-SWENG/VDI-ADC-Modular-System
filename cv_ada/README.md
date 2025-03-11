# CV_Ada Library

Welcome to the **CV_Ada** library! This Ada-based library provides a comprehensive suite of tools for computer vision tasks, image processing, and graphics manipulation. It is designed to be modular, efficient, and easy to use for developers working with image data in Ada.

---

## Features

### Basic Transformations (`CV_Ada.Basic_Transformations`)
Perform essential image transformations such as rotation, flipping, brightness/contrast adjustments, and sharpening.

- **Region_Of_Interest**: Extracts a specific region from an image.
- **Rotate_Image**: Rotates the image by a specified angle.
- **Flip_Image**: Flips the image horizontally or vertically.
- **Adjust_Brightness**: Modifies the brightness of the image.
- **Adjust_Contrast**: Adjusts the contrast of the image.
- **Invert_Colors**: Inverts the colors of the image.
- **Sharpen_Image**: Sharpens the details in the image.

---

### Blur Operations (`CV_Ada.Blur`)
Apply blurring techniques to smooth images.

- **Box_Blur**: Applies a simple box blur to the image.
- **Gaussian_Blur**: Applies a Gaussian blur with configurable sigma (default: 1.4).

---

### Colorspace Manipulation (`CV_Ada.Colorspace`)
Convert images between different colorspaces.

- **Convert_To_Grayscale**: Converts an image to grayscale.
- **Convert_To_Black_And_White**: Converts an image to black-and-white using a threshold (default: 128).

---

### Edge Detection (`CV_Ada.Edge_Detection`)
Detect edges in images using popular algorithms.

- **Sobel_Edge_Detection**: Detects edges using the Sobel operator.
- **Canny_Edge_Detection**: Performs edge detection with configurable thresholds (low and high).

---

### Graphics Utilities (`CV_Ada.Graphics.Pixel` and `CV_Ada.Graphics`)
Work with individual pixels and draw shapes on images.

#### Pixel Operations (`CV_Ada.Graphics.Pixel`)
- Supports arithmetic operations on pixels:
  - Addition (`+`)
  - Subtraction (`-`)
  - Multiplication (`*`)
  - Division (`/`)

#### Graphics Drawing (`CV_Ada.Graphics`)
- **Draw_Line**: Draws a line on an image with specified color and coordinates.
- **Get_Pixel**: Retrieves pixel data from an image at given coordinates.

---

### Hough Transform (`CV_Ada.Hough_Transform`)
Detect shapes such as lines and circles using Hough Transform algorithms.

- **Hough_Line_Transform**: Detects lines in an image using Hough Transform.
- **Hough_Circle_Transform**: Detects circles with configurable radius range and thresholds.

---

### Morphological Operations (`CV_Ada.Morphological_Operations`)
Perform morphological operations on binary images.

- Supported operations:
  - Erosion
  - Dilation
  - Opening
  - Closing
- Configurable structuring element types:
  - Square
  - Circle

---

### IO Operations (`CV_Ada.IO_Operations`)
Read and write images to/from files.

- **Write_To_File**: Saves an image to a file.
- **Load_QOI**: Loads an image from a file in QOI format.

---
