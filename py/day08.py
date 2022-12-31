import sys

class Image():
    def __init__(self, encoded_string):
        self.width = 25
        self.height = 6
        self.layers = len(encoded_string) // 150
        self.string = encoded_string

    def get_pixel(self, x, y, z):
        assert(x in range(self.width))
        assert(y in range(self.height))
        assert(z in range(self.layers))
        return self.string[z*150+y*25+x]

    def pixel_color(self, x, y):
        pixel_seq = map(lambda l: self.get_pixel(x, y, l), range(self.layers))
        for pixel in pixel_seq:
            match pixel:
                case '2':
                    pass
                case '1' | '0' :
                    return pixel
                case _:
                    raise RuntimeError("Found a pixel not in '012': {pixel}")
        raise RuntimeError(f"Fully transparent pixel position: {x},{y}")

    def print(self):
        for y in range(self.height):
            for x in range(self.width):
                if self.pixel_color(x, y) == '0':
                    sys.stdout.write(' ')
                else:
                    sys.stdout.write('@')
            sys.stdout.write("\n")

def smallest_0_layer(image):
    min0s = 150
    min0layer = 0
    for layer in range(image.layers):
        zeros = image.string[150*layer:150*(layer+1)].count('0')
        if (zeros < min0s):
            min0s = zeros
            min0layer = layer
    num1s = 0
    num2s = 0
    for x in range(image.width):
        for y in range(image.height):
            match image.get_pixel(x, y, min0layer):
                case '1':
                    num1s = num1s + 1
                case '2':
                    num2s = num2s + 1
    return num1s * num2s


def main(filename):
    with open(filename) as f:
        image = Image(f.read().rstrip("\n"))
        print(smallest_0_layer(image))
        image.print()

if __name__ == '__main__' and len(sys.argv) > 1:
    sys.exit(main(sys.argv[1]))
