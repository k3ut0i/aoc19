from intcode import Intcode, intcode_run_with_input
import sys

irwi = intcode_run_with_input

def main(filename):
    with open(filename) as f:
        prog_string = f.read().strip("\n")
        print(irwi(prog_string, 1), irwi(prog_string, 5))
        return 0

if __name__ == '__main__' and len(sys.argv) > 1:
    sys.exit(main(sys.argv[1]))
