from math import gcd, atan2, pi
from functools import partial
import sys


class Sector():
    def __init__(self, lines):
        self.lines = lines
        self.xmax = len(lines[0])
        self.ymax = len(lines)
        self.positions = set(self.init_asteroid_positions())
        self.visibility_matrix = self.init_asteroid_matrix()
    def get(self, x, y):
        return self.lines[y][x]

    def init_asteroid_positions(self):
        all_pos = [(i, j) for j in range(self.ymax) for i in range(self.xmax)]
        return filter(lambda p: self.get(p[0], p[1]) == '#', all_pos)

    def init_asteroid_matrix(self):
        matrix = dict()
        for i1 in range(self.xmax):
            for j1 in range(self.ymax):
                for i2 in range(self.xmax):
                    for j2 in range(self.ymax):
                        matrix[((i1, j1), (i2, j2))] = visible(i1, j1, i2, j2,
                                                               self.positions)
        return matrix

    def visible_from(self, x, y):
        return list(filter(lambda p: visible(x, y, p[0], p[1], self.positions),
                           self.positions))

    def num_visible_from(self, x, y):
        "number of asteroids visible from (x, y)"
        return len(self.visible_from(x, y))

    def print_visibility_sum(self):
        for y in range(self.ymax):
            for x in range(self.xmax):
                if (x, y) in self.positions:
                    sys.stdout.write(str(self.num_visible_from(x, y)))
                else:
                    sys.stdout.write('.')
            sys.stdout.write('\n')

    def max_visibility(self):
        return max(self.positions, key=lambda p: self.num_visible_from(p[0], p[1]))

def visible(x1, y1, x2, y2, posset):
    "Find if (x1, y1) is visible from (x2, y2), given asteroids at POSSET"
    if x1 == x2 and y1 == y2:
        return False
    g = gcd((x1-x2), (y1-y2))
    dx = (x1 - x2) // g
    dy = (y1 - y2) // g
    for i in range(1, g):
        xc = x2 + i*dx
        yc = y2 + i*dy
        if (xc, yc) in posset:
            return False
    return True

def clock(origin, point):
    """
Return angle of point w.r.t to 12-oclock in radians.
"""
    dx = point[0] - origin[0]
    dy = point[1] - origin[1]
    anglex = -1 * atan2(dx, dy)
    return anglex

def main(filename):
    with open(filename) as f:
        lines = map(lambda l: l.rstrip("\n"), list(f))
        s = Sector(list(lines))
        p = s.max_visibility()
        max_num = s.num_visible_from(p[0], p[1])
        visible_asteroids = s.visible_from(p[0], p[1])
        visible_asteroids.sort(key=partial(clock, p))
        #Just one rotation of laser is sufficient as we have 303 visible
        print(max_num, visible_asteroids[199])

if __name__ == '__main__' and len(sys.argv) > 1:
    import doctest
    doctest.testmod()
    sys.exit(main(sys.argv[1]))
