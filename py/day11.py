import sys
from intcode import Intcode, Opcode
from enum import Enum

class Direction(Enum):
    UP = 0
    DOWN = 1
    LEFT = 2
    RIGHT = 3
    def turn(self, t):
        match self:
            case self.UP:
                return Direction.LEFT if (t == 0) else Direction.RIGHT
            case self.DOWN:
                return Direction.RIGHT if (t == 0) else Direction.LEFT
            case self.LEFT:
                return Direction.DOWN if (t == 0) else Direction.UP
            case self.RIGHT:
                return Direction.UP if (t == 0) else Direction.DOWN

def next_pos(current_pos, direction):
    (x, y) = current_pos
    npos = None
    match direction:
        case Direction.UP:
            npos = (x, y+1)
        case Direction.DOWN:
            npos = (x, y-1)
        case Direction.LEFT:
            npos = (x-1, y)
        case Direction.RIGHT:
            npos = (x+1, y)
    return npos

class Color(Enum):
    BLACK = 0
    WHITE = 1

class Grid():
    def __init__(self):
        self.panels = dict()

    def get(self, xy):
        if xy in self.panels:
            return self.panels[xy]
        else:
            self.panels[xy] = Color.BLACK
            return Color.BLACK

    def set(self, xy, color):
        self.panels[xy] = color

    def print(self):
        (xmin, xmax, ymin, ymax) = xybounds(self.panels)
        for y in range(ymax, ymin-1, -1):
            for x in range(xmin, xmax+1):
                 pchar = None
                 match self.get((x,y)):
                     case Color.WHITE:
                         pchar = '#'
                     case Color.BLACK:
                         pchar = ' '
                 sys.stdout.write(pchar)
            sys.stdout.write('\n')

def xybounds(d):
    (xmin, xmax, ymin, ymax) = (0, 0, 0, 0)
    for (x, y) in d.keys():
        if x < xmin:
            xmin = x
        if x > xmax:
            xmax = x
        if y < ymin:
            ymin = y
        if y > ymax:
            ymax = y
    return (xmin, xmax, ymin, ymax)

class Robot():
    def __init__(self, prog_string):
        self.sys = Intcode(prog_string)
        self.runtime = self.sys.run_async()
        assert(self.runtime.send(None) == Opcode.INPUT)

    def step(self, camera_input):
        paint_color = None
        turn = None
        try:
            match self.runtime.send(camera_input):
                case (Opcode.OUTPUT, out):
                    paint_color = out
                    match self.runtime.send(None):
                        case (Opcode.OUTPUT, out):
                            assert(self.runtime.send(None) == Opcode.INPUT)
                            turn = out
                        case _:
                            raise RuntimeError("Robot should give two outputs.")
                    return (paint_color, turn)
                case Opcode.HALT:
                    print("Robot halted unexpected.")
                    return None
                case unknown_case:
                    raise RuntimeError("Robot giving this: " + str(unknown_case))
        except StopIteration:
            return None

class World():
    def __init__(self, prog_string):
        self.robot = Robot(prog_string)
        self.grid = Grid()
        self.pos = (0, 0)
        self.direction = Direction.UP

    def step(self):
        pos_color = self.grid.get(self.pos)
        match self.robot.step(pos_color.value):
            case None:
                return None
            case (new_color, turn):
                self.grid.set(self.pos, Color(new_color))
                self.direction = self.direction.turn(turn)
                self.pos = next_pos(self.pos, self.direction)
                return (new_color, turn)
            case _:
                raise RuntimeError("Should not reach this case in World.step")

    def run(self):
        while self.step() != None:
            pass

def main(filename):
    with open(filename) as f:
        prog_string = f.read().rstrip("\n")
        w = World(prog_string)
        w.run()
        print(len(w.grid.panels))
        w1 = World(prog_string)
        w1.grid.set((0, 0), Color.WHITE)
        w1.run()
        w1.grid.print()

if __name__ == '__main__' and len(sys.argv) > 1:
    sys.exit(main(sys.argv[1]))
