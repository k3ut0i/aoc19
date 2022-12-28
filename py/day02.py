from enum import Enum
import sys
import array

class Opcode(Enum):
    ADD = 1
    MUL = 2
    HALT = 99

class Intcode():
    def __init__(self, programstring):
        self.prog_array = array.array('i', map(int, programstring.split(",")))
        self.ip = 0
    def step(self):
        oc = Opcode(self.prog_array[self.ip])
        match oc:
            case Opcode.ADD:
                arg1 = self.prog_array[self.ip+1]
                arg2 = self.prog_array[self.ip+2]
                arg3 = self.prog_array[self.ip+3]
                self.prog_array[arg3] = self.prog_array[arg1] + self.prog_array[arg2]
                self.ip += 4
            case Opcode.MUL:
                arg1 = self.prog_array[self.ip+1]
                arg2 = self.prog_array[self.ip+2]
                arg3 = self.prog_array[self.ip+3]
                self.prog_array[arg3] = self.prog_array[arg1] * self.prog_array[arg2]
                self.ip += 4
            case Opcode.HALT:
                pass
        return oc
    def run(self):
        while (status := self.step()) != Opcode.HALT:
            pass
    def __str__(self):
        return "Intcode [IP: " + str(self.ip) + " ," + str(self.prog_array) + " ]"

def main(filename) -> int:
    with open(filename) as f:
        contents = f.read()
        contents.rstrip("\n\r")
        i1 = Intcode(contents)
        i2 = Intcode(contents)
        print(part1(i1), part2(contents, 19690720))
    return 0

def part1(ic) -> int:
    ic.prog_array[1] = 12
    ic.prog_array[2] = 2
    ic.run()
    return ic.prog_array[0]

def run_with(noun, verb, ic) -> int:
    ic.prog_array[1] = noun
    ic.prog_array[2] = verb
    ic.run()
    return ic.prog_array[0]

def part2(prog_string, required_output) -> int:
    for noun, verb in [(n, v) for n in range(100) for v in range(100)]:
        if (run_with(noun, verb, Intcode(prog_string)) == required_output):
            return 100* noun + verb
    raise RuntimeError("could not find the any noun, verb pairs that give the required output.")

if __name__ == '__main__':
    sys.exit(main(sys.argv[1]))
