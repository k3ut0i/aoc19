from enum import Enum
from intcode import Opcode, Intcode, Addressing
import sys
import array

def main(filename) -> int:
    with open(filename) as f:
        contents = f.read()
        contents.rstrip("\n\r")
        i1 = Intcode(contents)
        i2 = Intcode(contents)
        print(part1(i1),part2(contents, 19690720))
    return 0

def part1(ic) -> int:
    ic.memory.set_value(1, Addressing.POSITIONAL, 12)
    ic.memory.set_value(2, Addressing.POSITIONAL, 2)
    ic.run()
    return ic.memory.get_value(0, Addressing.IMMEDIATE)

def run_with(noun, verb, ic) -> int:
    ic.memory.set_value(1, Addressing.POSITIONAL, noun)
    ic.memory.set_value(2, Addressing.POSITIONAL, verb)
    ic.run()
    return ic.memory.get_value(0, Addressing.IMMEDIATE)

def part2(prog_string, required_output) -> int:
    for noun, verb in [(n, v) for n in range(100) for v in range(100)]:
        if (run_with(noun, verb, Intcode(prog_string)) == required_output):
            return 100* noun + verb
    raise RuntimeError("could not find the any noun, verb pairs that give the required output.")

if __name__ == '__main__':
    sys.exit(main(sys.argv[1]))
