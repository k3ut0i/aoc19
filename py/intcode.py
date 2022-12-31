from enum import Enum
import array

class Opcode(Enum):
    ADD = 1
    MUL = 2
    INPUT = 3
    OUTPUT = 4
    JIT = 5
    JIF = 6
    LT = 7
    EQ = 8
    HALT = 99

oplen = {
    Opcode.ADD: 4,
    Opcode.MUL: 4,
    Opcode.INPUT: 2,
    Opcode.OUTPUT: 2,
    Opcode.JIT: 3,
    Opcode.JIF: 3,
    Opcode.LT: 4,
    Opcode.EQ: 4,
    Opcode.HALT: 1
    }

def binary_op(oc, a, b):
    match oc:
        case Opcode.ADD:
            return a + b
        case Opcode.MUL:
            return a * b
        case Opcode.LT:
            return 1 if a < b else 0
        case Opcode.EQ:
            return 1 if a == b else 0
        case _:
            raise RuntimeError("Not a binary op: " + str(oc))

class Addressing(Enum):
    POSITIONAL = 0
    IMMEDIATE = 1

def parse_op(n):
    digits = []
    num = n // 100
    opnum = n % 100
    for i in range(3):
        digits.append(num % 10)
        num = num // 10
    return (Opcode(opnum), list(map(lambda x: Addressing(x), digits)))

class Memory():
    def __init__(self, csvline):
        self.mem = array.array('i', map(int, csvline.split(",")))

    def get_value(self, address, mode):
        match mode:
            case Addressing.IMMEDIATE:
                return self.mem[address]
            case Addressing.POSITIONAL:
                return self.mem[self.mem[address]]

    def set_value(self, address, value):
        self.mem[address] = value

    def __str__(self):
        return f"mem:<{self.mem}>"


class Intcode():
    """
    >>> i = Intcode('1, 0, 0, 0, 99')
    >>> i.run()
    >>> print(i.memory)
    mem:<array('i', [2, 0, 0, 0, 99])>
    """

    def __init__(self, programstring):
        self.memory = Memory(programstring)
        self.ip = 0
        self.input = None
        self.output = None

    def step(self):
        (oc, modes) = parse_op(self.memory.get_value(self.ip, Addressing.IMMEDIATE))
        match oc:
            case Opcode.ADD | Opcode.MUL | Opcode.LT | Opcode.EQ:
                arg1 = self.memory.get_value(self.ip+1, modes[0])
                arg2 = self.memory.get_value(self.ip+2, modes[1])
                arg3 = self.memory.get_value(self.ip+3, Addressing.IMMEDIATE)
                self.memory.set_value(arg3, binary_op(oc, arg1, arg2))
                self.ip += oplen[oc]
            case Opcode.INPUT:
                arg = self.memory.get_value(self.ip+1, Addressing.IMMEDIATE)
                self.memory.set_value(arg, self.input)
                self.ip += oplen[oc]
            case Opcode.OUTPUT:
                self.output = self.memory.get_value(self.ip+1, modes[0])
                self.ip += oplen[oc]
            case Opcode.JIT | Opcode.JIF:
                arg1 = self.memory.get_value(self.ip+1, modes[0])
                arg2 = self.memory.get_value(self.ip+2, modes[1])
                if  (oc == Opcode.JIT and arg1 != 0) or (oc == Opcode.JIF and arg1 == 0):
                    self.ip = arg2
                else:
                    self.ip += oplen[oc]
            case Opcode.HALT:
                self.ip += oplen[oc]
        # print(self.ip, "\n")
        return oc

    def run(self):
        while (status := self.step()) != Opcode.HALT:
            pass

    def run_async(self):
        "yields for before input and after output operations"
        status = None
        while (status != Opcode.HALT):
            (oc, g) = parse_op(self.memory.get_value(self.ip, Addressing.IMMEDIATE))
            if oc == Opcode.INPUT:
                self.input = yield oc
                status = self.step()
            elif oc == Opcode.OUTPUT:
                status = self.step()
                yield (oc, self.output)
            else:
                status = self.step()
        return status

    def __str__(self):
        return "Intcode [IP: " + str(self.ip) + " ," + str(self.memory) + " ]"

def intcode_run_with_input(progstring, input_value):
    i = Intcode(progstring)
    i.input = input_value
    i.run()
    return i.output

if __name__ == '__main__':
    import doctest
    doctest.testmod()
