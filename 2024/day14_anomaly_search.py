import os
import numpy as np
from PIL import Image

MATRIX_WIDTH = 101
MATRIX_HEIGHT = 103

def generate_image_from_coords(coords, width, height):
    image = np.zeros((height, width), dtype=np.uint8)

    for x, y in coords:
        if 0 <= x < width and 0 <= y < height:
            image[y, x] = 255

    return Image.fromarray(image)

def read_coords_from_file(file_path):
    coords = []
    with open(file_path, 'r') as file:
        for line in file:
            x, y = map(int, line.strip().split(','))
            coords.append((x, y))
    return coords

def find_long_straight_lines(image_array, min_length=20):
    height, width = image_array.shape

    # Check horizontal lines
    for y in range(height):
        count = 0
        for x in range(width):
            if image_array[y, x] == 255:
                count += 1
                if count > min_length:
                    return True
            else:
                count = 0

    # Check vertical lines
    for x in range(width):
        count = 0
        for y in range(height):
            if image_array[y, x] == 255:
                count += 1
                if count > min_length:
                    return True
            else:
                count = 0

    return False

def process_folder(folder_path):
    for file_name in os.listdir(folder_path):
        if file_name.endswith('.txt'):
            file_path = os.path.join(folder_path, file_name)
            coords = read_coords_from_file(file_path)
            image = generate_image_from_coords(coords, MATRIX_WIDTH, MATRIX_HEIGHT)
            image.save(file_path.replace('.txt', '.png'))

            # Find anomalies (long straight lines) in the image
            image_array = np.array(image)
            if find_long_straight_lines(image_array):
                print(f"Anomaly found in {file_name}: Long straight line longer than 20 white pixels")

folder_path = 'day14_text_files/'
process_folder(folder_path)
