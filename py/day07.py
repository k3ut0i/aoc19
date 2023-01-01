import sys
from intcode import Intcode, Opcode
from itertools import permutations

class ThrusterAmp():
    def __init__(self, prog_string, phase_setting):
        self.system = Intcode(prog_string)
        self.runtime = self.system.run_async()
        assert (next(self.runtime) == Opcode.INPUT)
        assert (self.runtime.send(phase_setting) == Opcode.INPUT)

    def run(self, inp):
        match self.runtime.send(inp):
            case (Opcode.OUTPUT, out):
                return out
            case Opcode.HALT:
                pass
            case Opcode.INPUT: # reaches here if amp yielded after output
                return self.run(inp)

class ThrusterSystem():
    """
    >>> ThrusterSystem('3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0').set_phases([4, 3, 2, 1, 0]).run_with_input(0)
    43210
    """
    def __init__(self, prog_string):
        self.prog = prog_string
        self.amps = []

    def set_phases(self, phases):
        assert (len(phases) == 5)
        self.amps = []
        for phase in phases:
            self.amps.append(ThrusterAmp(self.prog, phase))
        return self

    def run_with_input(self, inp):
        out = inp
        for amp in self.amps:
            out = amp.run(out)
        return out

    def run_in_loop(self, inp):
        out = inp
        try:
            while True:
                out = self.run_with_input(out)
        except StopIteration:
            return out
            

def find_max_signal(prog_string):
    ts = ThrusterSystem(prog_string)
    all_phases = permutations(range(5))
    mphase = max(all_phases, key=lambda p: ts.set_phases(p).run_with_input(0))
    return ts.set_phases(mphase).run_with_input(0)

def find_max_signal_loop(prog_string):
    ts = ThrusterSystem(prog_string)
    all_phases = permutations([5, 6, 7, 8, 9])
    mphase = max(all_phases, key=lambda p: ts.set_phases(p).run_in_loop(0))
    return ts.set_phases(mphase).run_in_loop(0)

def main(filename):
    with open(filename) as f:
        prog_string = f.read().rstrip("\n")
        print(find_max_signal(prog_string),
              find_max_signal_loop(prog_string))
        
    
if __name__ == '__main__' and len(sys.argv) > 1:
    import doctest
    doctest.testmod()
    sys.exit(main(sys.argv[1]))
