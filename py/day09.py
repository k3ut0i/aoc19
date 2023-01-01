from intcode import Intcode, intcode_run_with_input
import sys

def main(filename):
    with open(filename) as f:
        prog = f.read().rstrip("\n")
        print(intcode_run_with_input(prog, 1),
              intcode_run_with_input(prog, 2))

if __name__ == '__main__' and len(sys.argv) > 1:
    sys.exit(main(sys.argv[1]))
