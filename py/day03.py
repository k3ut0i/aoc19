from enum import Enum, auto
import sys

class Direction(Enum):
    UP = 1
    DOWN = 2
    LEFT = 3
    RIGHT = 4

def direction_from_char(c) -> Direction:
    match c:
        case 'U':
            return Direction.UP
        case 'D':
            return Direction.DOWN
        case 'L':
            return Direction.LEFT
        case 'R':
            return Direction.RIGHT
        case _:
            raise RuntimeError("Unrecognized character for direction: " + c)

class Instruction():
    def __init__(self, s):
        self.direction = direction_from_char(s[0])
        self.distance = int(s[1:])
    def __str__(self):
        return "Instruction[" + str(self.direction) + ", " + str(self.distance) + "]"

class Line():
    def __init__(self, pfrom, ins):
        (fromx, fromy) = pfrom
        difx = 0
        dify = 0
        match ins.direction:
            case Direction.UP:
                dify = ins.distance
            case Direction.DOWN:
                dify = -1 * ins.distance
            case Direction.RIGHT:
                difx = ins.distance
            case Direction.LEFT:
                difx = -1 * ins.distance
        self.pfrom = pfrom
        self.pto = (fromx + difx, fromy + dify)
        self.direction = ins.direction 

    def __str__(self):
        return "Line<" + str(self.direction) + ">["\
        + str(self.pfrom) + " -> " + str(self.pto) + "]"

    def on_line(self, point) -> bool:
        (x, y) = point
        (fromx, fromy) = self.pfrom
        (tox, toy) = self.pto
        return between(x, fromx, tox) and between(y, fromy, toy)

    def dis_from_start(self, point) -> int:
        (x, y) = point
        (fromx, fromy) = self.pfrom
        return abs(x-fromx) + abs(y-fromy)

    def length(self) -> int:
        return self.dis_from_start(self.pto)

def trace_lines(instrs) -> list:
    pfrom = (0, 0)
    lines = []
    for ins in instrs:
        new_line = Line(pfrom, ins)
        pfrom = new_line.pto
        lines.append(new_line)
    return lines

def intersects(l1, l2):
    dirs = {l1.direction, l2.direction}
    if len(dirs) == 1 or dirs == {Direction.UP, Direction.DOWN} \
        or dirs == {Direction.RIGHT, Direction.LEFT}:
        return False
    else:
        if l1.direction in {Direction.RIGHT, Direction.LEFT}:
            return intersects_horiz_with_vert(l1, l2)
        else:
            return intersects_horiz_with_vert(l2, l1)

def between(e, x, y) -> bool:
    if (x <= y):
        return x <= e and e <= y
    else:
        return y <= e and e <= x

def intersects_horiz_with_vert(lh, lv):
    (lhfromx, lhfromy) = lh.pfrom
    (lhtox, lhtoy) = lh.pto
    (lvfromx, lvfromy) = lv.pfrom
    (lvtox, lvtoy) = lv.pto
    assert (lhfromy == lhtoy)
    assert (lvfromx == lvtox)
    if between(lvfromx, lhfromx, lhtox) and \
        between(lhfromy, lvfromy, lvtoy):
        return (lvfromx, lhfromy)
    else:
        return False

def intersections(ls1, ls2):
    intersections = []
    for l1 in ls1:
        for l2 in ls2:
            if (point := intersects(l1, l2)):
                intersections.append(point)
    intersections = filter(lambda p: not (p[0] == 0 and p[1] == 0), intersections)
    return list(intersections)

def length_to_point(lines, point) -> int:
    cur_len = 0
    for line in lines:
        if line.on_line(point):
            return cur_len + line.dis_from_start(point)
        else:
            cur_len += line.length()

def main(filename) -> int:
    with open(filename) as f:
        (w1str, w2str) = list(f)
        wire1 = map(lambda s: Instruction(s), w1str.split(","))
        wire2 = map(lambda s: Instruction(s), w2str.split(","))
        lines1 = trace_lines(wire1)
        lines2 = trace_lines(wire2)
        inters = intersections(lines1, lines2)
        nearest_point = min(inters, key=lambda p: abs(p[0])+abs(p[1]))
        f = lambda p: length_to_point(lines1, p) + length_to_point(lines2, p)
        smallest_steps = min(map(f, inters))
        print(nearest_point, smallest_steps)
        return 0

if __name__ == '__main__' and len(sys.argv) > 1: 
    sys.exit(main(sys.argv[1]))
