
#!/usr/bin/env python3

import math
from PIL import Image

# --------Functions---------#
def get_pixel(image, row, col):
    """
    Given a row and column returns value at index
    """
    # compute number we have to offset by
    offset = image["width"] * (row)
    # print(image["pixels"][col + offset])
    return image["pixels"][col + offset]

def set_pixel(image, color):
    """
    Sets pixel to a new color
    """
    image["pixels"] += [color]

def apply_per_pixel(image, func):
    """
    Returns image in which the color of each pixel is inverted
    """
    result = {
        "height": image["height"],
        "width": image["width"],
        "pixels": [],
    }
    for row in range(image["height"]):
        for col in range(image["width"]):
            color = get_pixel(image, row, col)
            new_color = func(color)
            set_pixel(result, new_color)
    return result

def inverted(image):
    """
    Returns dictionary with inverted pixels. Does not modify original image.
    """
    return apply_per_pixel(image, lambda color: 255 - color)


# HELPER FUNCTIONS
def get_corr(image, row, col, boundary_behavior):
    """
    Helper function for correlate.
    """
    if boundary_behavior == "zero":
        if row < 0 or row >= image["height"] or col < 0 or col >= image["width"]:
            return 0
        else:
            return get_pixel(image, row, col)
    elif boundary_behavior == "wrap":
        n_row = row % image["height"]
        n_col = col % image["width"]
        return get_pixel(image, n_row, n_col)
    elif boundary_behavior == "extend":
        n_row = row
        n_col = col
        if row < 0:
            n_row = 0
        if row >= image["height"]:
            n_row = image["height"] - 1
        if col < 0:
            n_col = 0
        if col >= image["width"]:
            n_col = image["width"] - 1
        return get_pixel(image, n_row, n_col)

def round_and_clip_image(image):
    """
    Given a dictionary, ensure that the values in the "pixels" list are all
    integers in the range [0, 255].

    All values should be converted to integers using Python's `round` function.

    Any locations with values higher than 255 in the input should have value
    255 in the output; and any locations with values lower than 0 in the input
    should have value 0 in the output.
    """
    round_dict = image.copy()
    for i in range(len(round_dict["pixels"])):
        if round_dict["pixels"][i] > 255:
            round_dict["pixels"][i] = 255
        elif round_dict["pixels"][i] < 0:
            round_dict["pixels"][i] = 0
        elif type(round_dict["pixels"][i]) == float:
            round_dict["pixels"][i] = round(round_dict["pixels"][i])
    return round_dict

def correlate(image, kernel, boundary_behavior):
    """
    Compute the result of correlating the given image with the given kernel.
    `boundary_behavior` will one of the strings "zero", "extend", or "wrap",
    and this function will treat out-of-bounds pixels as having the value zero,
    the value of the nearest edge, or the value wrapped around the other edge
    of the image, respectively.

    if boundary_behavior is not one of "zero", "extend", or "wrap", return
    None.

    Otherwise, the output of this function should have the same form as a 6.101
    image (a dictionary with "height", "width", and "pixels" keys), but its
    pixel values do not necessarily need to be in the range [0,255], nor do
    they need to be integers (they should not be clipped or rounded at all).

    This process should not mutate the input image; rather, it should create a
    separate structure to represent the output.

    DESCRIBE YOUR KERNEL REPRESENTATION HERE
    Kernel is represented in same format as image.
    """
    new_dict = image.copy()
    result = []
    for row in range(image["height"]):
        for col in range(image["width"]):
            total = 0
            for row_k in range(kernel["height"]):
                for col_k in range(kernel["width"]):
                    new_row = row - kernel["height"] // 2 + row_k
                    new_col = col - kernel["width"] // 2 + col_k
                    pixel = get_corr(image, new_row, new_col, boundary_behavior)
                    total += pixel * get_pixel(kernel, row_k, col_k)
            # print(row, col, total)
            result.append(total)
    new_dict["pixels"] = result
    return new_dict


# FILTERS


def blurred(image, kernel_size):
    """
    Return a new image representing the result of applying a box blur (with the
    given kernel size) to the given input image.

    This process should not mutate the input image; rather, it should create a
    separate structure to represent the output.
    """
    # first, create a representation for the appropriate n-by-n kernel
    kernel_dimension = kernel_size ** 2
    kernel_value = 1 / kernel_dimension
    kernel_pixels = [kernel_value] * kernel_dimension
    # print(kernel_pixels)
    new_kernel = {"height": kernel_size, "width": kernel_size, "pixels": kernel_pixels}
    # then compute the correlation of the input image with that kernel
    # and, finally, make sure that the output is a valid image (using the
    # helper function from above) before returning it.
    result = correlate(image, new_kernel, "extend")
    return round_and_clip_image(result)


def sharpened(image, n):
    """
    Sharpens image by subtracting the (blurred) version of the image
    from 2 times the original image
    """
    sharpen_image = image.copy()
    blurred_image = blurred(image, n)
    sharpen_pixles = [
        2 * image["pixels"][i] - blurred_image["pixels"][i]
        for i in range(len(image["pixels"]))
    ]
    sharpen_image["pixels"] = sharpen_pixles
    return round_and_clip_image(sharpen_image)


def edges(image):
    """
    Computes two correlations, and then takes the square root of the squared
    sum of each pixel in the image resulting from the two correlations.
    """
    k_row = {"height": 3, "width": 3, "pixels": [-1, -2, -1, 0, 0, 0, 1, 2, 1]}
    k_col = {"height": 3, "width": 3, "pixels": [-1, 0, 1, -2, 0, 2, -1, 0, 1]}
    row_correlation = correlate(image, k_row, "extend")
    col_correlation = correlate(image, k_col, "extend")
    new_image = image.copy()
    new_pixels = [
        math.sqrt(row_correlation["pixels"][i] ** 2 + col_correlation["pixels"][i] ** 2)
        for i in range(len(row_correlation["pixels"]))
    ]
    new_image["pixels"] = new_pixels
    return round_and_clip_image(new_image)

# VARIOUS FILTERS

# ----Helper Functions for color_filter_from_greyscale----#


def split_rgb_image(image, color):
    """
    0 if red, 1 if green, 2 if blue 
    """
    my_image = image.copy()
    col_pixels = []
    for pixel in range(len(image["pixels"])):
        col_pixels.append(image["pixels"][pixel][color])
    my_image["pixels"] = col_pixels
    return my_image


def combine_split_image(red, green, blue):
    """
    Given three dictionaries,
    """
    combined_image = red.copy()
    combined_pixels = []
    for i in range(len(red["pixels"])):
        pixel = (red["pixels"][i], green["pixels"][i], blue["pixels"][i])
        combined_pixels.append(pixel)
    combined_image["pixels"] = combined_pixels
    return combined_image

# -----------------------------------------------------#

def color_filter_from_greyscale_filter(filt):
    """
    Given a filter that takes a greyscale image as input and produces a
    greyscale image as output, returns a function that takes a color image as
    input and produces the filtered color image.
    """

    def colored_image(image):
        red_image = split_rgb_image(image, 0)
        green_image = split_rgb_image(image, 1)
        blue_image = split_rgb_image(image, 2)
        filt_red = filt(red_image)
        filt_green = filt(green_image)
        filt_blue = filt(blue_image)
        return combine_split_image(filt_red, filt_green, filt_blue)

    return colored_image


# ----------------Blur---------------#

def make_blur_filter(kernel_size):
    def blur_image(image):
        return blurred(image, kernel_size)

    return blur_image

# ----------------Sharpen---------------#

def make_sharpen_filter(kernel_size):
    def sharpen_image(image):
        return sharpened(image, kernel_size)

    return sharpen_image

# ----------------Cascade of Filters---------------#

def filter_cascade(filters):
    """
    Given a list of filters (implemented as functions on images), returns a new
    single filter such that applying that filter to an image produces the same
    output as applying each of the individual ones in turn.
    """

    def apply_filter(image):
        for filter in filters:
            image = filter(image)
        return image

    return apply_filter

# ––––––––––––SEAM CARVING–––––––––––––#

# Main Seam Carving Implementation

def seam_carving(image, ncols):
    """
    Starting from the given image, use the seam carving technique to remove
    ncols (an integer) columns from the image. Returns a new image.
    """
    seamless = image.copy()
    for i in range(ncols):
        # print(i, "col num")
        my_grey_image = greyscale_image_from_color_image(seamless)
        energy = compute_energy(my_grey_image)
        c_energy = cumulative_energy_map(energy)
        seam = minimum_energy_seam(c_energy)
        seamless = image_without_seam(seamless, seam)
    return seamless

# –––––––––––––––HELPER FUNCTIONS FOR SEAM CARVING–––––––––––#


def greyscale_image_from_color_image(image):
    """
    Given a color image, computes and returns a corresponding greyscale image.
    Returns a greyscale image (represented as a dictionary).
    """
    grey_image = image.copy()
    grey_pixels = []
    for pixel in image["pixels"]:
        new_pixel = round(0.299 * pixel[0] + 0.587 * pixel[1] + 0.114 * pixel[2])
        grey_pixels.append(new_pixel)
    grey_image["pixels"] = grey_pixels
    return grey_image

def compute_energy(grey):
    """
    Given a greyscale image, computes a measure of "energy", in our case using
    the edges function from last week.

    Returns a greyscale image (represented as a dictionary).
    """
    return edges(grey)

def find_min_adjacent(energy, row, col):
    """
    Returns the minimum of the cummulative energies from the adjacent pixels in the row above
    """
    if col == 0:
        return min(get_pixel(energy, row - 1, col), get_pixel(energy, row - 1, col + 1))
    elif col == energy["width"] - 1:
        return min(get_pixel(energy, row - 1, col - 1), get_pixel(energy, row - 1, col))
    else:
        return min(
            get_pixel(energy, row - 1, col - 1),
            get_pixel(energy, row - 1, col),
            get_pixel(energy, row - 1, col + 1),
        )

def cumulative_energy_map(energy):
    """
    Given a measure of energy (e.g., the output of the compute_energy
    function), computes a "cumulative energy map" as described in the lab 2
    writeup.

    Returns a dictionary with 'height', 'width', and 'pixels' keys (but where
    the values in the 'pixels' array may not necessarily be in the range [0,
    255].
    """
    energy_map = energy.copy()
    energy_map["pixels"] = []
    for row in range(energy["height"]):
        for col in range(energy["width"]):
            if row == 0:
                energy_map["pixels"].append(get_pixel(energy, row, col))
            else:
                energy_map["pixels"].append(
                    find_min_adjacent(energy_map, row, col)
                    + get_pixel(energy, row, col)
                )
    return energy_map

def find_flatlist_min_adj(image, index):
    """
    Helper function for minimum energy seam.
    """
    # find the index of the value directly above
    above_index = index - image["width"]
    above_value = image["pixels"][above_index]
    adjacent_list = [above_value]
    # if value is in outermost column
    if (index + 1) % image["width"] != 0:
        right_index = above_index + 1
        above_right = image["pixels"][right_index]
    else:
        # assign random high number
        above_right = 100 ** 5
    # if value is in innermost column
    if index % image["width"] != 0:
        left_index = above_index - 1
        above_left = image["pixels"][left_index]
    else:
        # assign random high number
        above_left = 100 ** 5
    adjacent_list.insert(0, above_left)
    adjacent_list.append(above_right)
    # find min value
    min_val = min(adjacent_list)
    if min_val == above_left:
        return left_index
    if min_val == above_value:
        return above_index
    return right_index

def minimum_energy_seam(cem):
    """
    Given a cumulative energy map, returns a list of the indices into the
    'pixels' list that correspond to pixels contained in the minimum-energy
    seam (computed as described in the lab 2 writeup).
    """
    energy_map = cem.copy()
    # find last row in list
    last_row = energy_map["pixels"][(energy_map["width"]) * -1 :]
    # find smallest value in last row
    min_last_val = min(last_row)
    # get the index of the pixel containing smallest value
    last_index = (last_row.index(min_last_val)) + (cem["height"] - 1) * (cem["width"])
    indices = [last_index]
    for x in range(cem["height"] - 1):
        last_index = find_flatlist_min_adj(cem, last_index)
        indices.append(last_index)
    # first find min value in last row
    return indices

def image_without_seam(image, seam):
    """
    Given a (color) image and a list of indices to be removed from the image,
    return a new image (without modifying the original) that contains all the
    pixels from the original image except those corresponding to the locations
    in the given list.
    """
    seamless_dict = {
        "height": image["height"],
        "width": image["width"]-1,
        "pixels": image["pixels"][:],
    }
    seam.sort()
    seam.reverse()
    for index in seam:
        seamless_dict["pixels"].pop(index)
    return seamless_dict

# -----------CUSTOM FUNCTION------------#


def transpose(image):
    """Transposes a one dimensional list"""
    transposed_image = image.copy()
    new_pixels = [get_pixel(image, j, i) for i in range(image["width"]) for j in range(image["height"])]
    transposed_image["pixels"] = new_pixels
    return transposed_image

def image_from_2d_array(image):
    """
    Turns the pixels of an image in a two-dimensional list into a
    one-dimensional list
    """
    result = []
    dict_result = {
        "height": image["height"],
        "width": image["width"],
        "pixels": image["pixels"][:],
    }
    for i in range(image["height"]):
        result += dict_result["pixels"][i]
    dict_result["pixels"] = result
    return dict_result

def image_to_2d_array(image):
    """
    Turns the pixels of an image in a one dimensional list into a
    two-dimensional list
    """
    new_dict = {
        "height": image["height"],
        "width": image["width"],
        "pixels": image["pixels"][:],
    }
    count = 0
    result = []
    for i in range(image["height"]):
        result += [image["pixels"][count : count + image["width"]]]
        count += image["width"]
    new_dict["pixels"] = result
    return new_dict

def custom_feature(image, shift_horiz, shift_vert):
    """
    Function shifts pixels horizontally and then vertically. The way the
    shifting process works is by shifting elements in the pixels list of
    an image to the right by either shift_horiz or shift_vert. Elements
    at the end of the list should now be at the front.
    Param: 
        image: a dictionary representing an image
        shift_horiz: amount to shift values by horizontally
        shift_vert: amount to shift values by vertically
    Invoked as, for example: square = custom_feature(frog, 50, 110)
    """
    if shift_horiz > image["width"] or  shift_vert > image["width"]:
        print("Shift amount cannot be larger than width of image")
        return
    two_dim_image = image_to_2d_array(image)
    two_dim_shifted = {
        "height": two_dim_image["height"],
        "width": two_dim_image["width"],
        "pixels": two_dim_image["pixels"][:],
    }
    for direction in range(2):
        # when direction == 1, shift vertically
        if direction == 1:
            two_dim_shifted = image_from_2d_array(two_dim_shifted)
            # transpose image to shift values vertically
            two_dim_shifted = transpose(two_dim_shifted)
            two_dim_shifted = image_to_2d_array(two_dim_shifted)
        for row in range(image["height"]):
            # if shifting horizontally
            if direction == 0:
                shift = shift_horiz % len(two_dim_shifted["pixels"][row])
            else:
            # if shifting vertically
                shift = shift_vert % len(two_dim_shifted["pixels"][row])
            new_row = two_dim_image["pixels"][row][-shift:] + two_dim_image["pixels"][row][:-shift]
            two_dim_shifted["pixels"][row] = new_row
    return transpose(image_from_2d_array(two_dim_shifted))

# ------------HELPER FUNCTIONS FOR LOADING AND SAVING COLOR IMAGES----------#

def load_color_image(filename):
    """
    Loads a color image from the given file and returns a dictionary
    representing that image.

    Invoked as, for example:
       i = load_color_image('test_images/cat.png')
    """
    with open(filename, "rb") as img_handle:
        img = Image.open(img_handle)
        img = img.convert("RGB")  # in case we were given a greyscale image
        img_data = img.getdata()
        pixels = list(img_data)
        width, height = img.size
        return {"height": height, "width": width, "pixels": pixels}

def save_color_image(image, filename, mode="PNG"):
    """
    Saves the given color image to disk or to a file-like object.  If filename
    is given as a string, the file type will be inferred from the given name.
    If filename is given as a file-like object, the file type will be
    determined by the 'mode' parameter.
    """
    out = Image.new(mode="RGB", size=(image["width"], image["height"]))
    out.putdata(image["pixels"])
    if isinstance(filename, str):
        out.save(filename)
    else:
        out.save(filename, mode)
    out.close()

def load_greyscale_image(filename):
    """
    Loads an image from the given file and returns an instance of this class
    representing that image.  This also performs conversion to greyscale.

    Invoked as, for example:
       i = load_greyscale_image('test_images/cat.png')
    """
    with open(filename, "rb") as img_handle:
        img = Image.open(img_handle)
        img_data = img.getdata()
        if img.mode.startswith("RGB"):
            pixels = [
                round(0.299 * p[0] + 0.587 * p[1] + 0.114 * p[2]) for p in img_data
            ]
        elif img.mode == "LA":
            pixels = [p[0] for p in img_data]
        elif img.mode == "L":
            pixels = list(img_data)
        else:
            raise ValueError(f"Unsupported image mode: {img.mode}")
        width, height = img.size
        return {"height": height, "width": width, "pixels": pixels}

def save_greyscale_image(image, filename, mode="PNG"):
    """
    Saves the given image to disk or to a file-like object.  If filename is
    given as a string, the file type will be inferred from the given name.  If
    filename is given as a file-like object, the file type will be determined
    by the 'mode' parameter.
    """
    out = Image.new(mode="L", size=(image["width"], image["height"]))
    out.putdata(image["pixels"])
    if isinstance(filename, str):
        out.save(filename)
    else:
        out.save(filename, mode)
    out.close()

if __name__ == "__main__":

    # ------INVERTED-----#
    # cat_color = load_color_image("/image_processing_2/test_images/cat.png")
    # color_inverted = color_filter_from_greyscale_filter(inverted)
    # save_color_image(color_inverted(cat_color), "cat_color_inverted.png", mode="PNG")

    # ------BLUR-----#
    python = load_color_image("/image_processing_2/test_images/python.png")
    # python = color_filter_from_greyscale_filter(make_blur_filter(9))(python)
    # save_color_image(python, "python_f.png", mode="PNG")

    # ------SHARPEN-----#
    # sc = load_color_image("/image_processing_2/test_images/sparrowchick.png")
    # sc = color_filter_from_greyscale_filter(make_sharpen_filter(7))(sc)
    # save_color_image(sc, "sc_sharp.png", mode="PNG")

    # ------CASCADE------#
    frog = load_color_image("/image_processing_2/test_images/frog.png")
    # filter1 = color_filter_from_greyscale_filter(edges)
    # filter2 = color_filter_from_greyscale_filter(make_blur_filter(5))
    # filt = filter_cascade([filter1, filter1, filter2, filter1])
    # save_color_image(filt(frog), "frog_cascade.png", mode="PNG")

    #–––––––––––––––SEAM––––––––––––––––#
    # twoc = load_color_image("/image_processing_2/test_images/twocats.png")
    # save_color_image(seam_carving(twoc, 100), "twocat_seam.png", mode="PNG")

    # ––––––––––––––CUSTOM–––––––––––––#
    # print(python["width"])
    # square = custom_feature(python, 10, 20)
    # save_color_image(square, "python_custom2.png", mode="PNG")
    pass
