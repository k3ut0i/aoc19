import sys
from aocutils import between

puzzle_input = "108457-562041"
(start, end) = map(int, puzzle_input.split("-"))

class Password():
    def __init__(self, n):
        assert (n > 99999 and n < 1000000)
        self.digits = []
        nr = n
        while nr > 0:
            self.digits.append(nr % 10)
            nr = nr // 10
        self.digits.reverse()

    def to_num(self):
        n = 0
        for d in self.digits:
            n = n * 10 + d
        return n

    def __str__(self):
        return str(self.to_num())

    def is_increasing(self) -> bool:
        for i in range(0, len(self.digits)-1):
            if self.digits[i] > self.digits[i+1]:
                return False
        return True

    def is_two_adjacent(self) -> bool:
        for i in range(0, len(self.digits)-1):
            if self.digits[i] == self.digits[i+1]:
                return True
        return False
    
    def in_range(self, small, big) -> bool:
        return between(self.to_num(), small, big)

    def valid_pass1(self, small, big):
        return self.is_increasing() and self.is_two_adjacent()\
        and self.in_range(small, big)
    
    def exact_two_adjacent(self) -> bool:
        for i in range(0, len(self.digits)-1):
            if self.digits[i] == self.digits[i+1]:
                b1 = self.digits[i+2] != self.digits[i] if i+2 < len(self.digits) else True
                b2 = self.digits[i-1] != self.digits[i] if i > 0 else True
                if b1 and b2:
                    return True
        return False

    def valid_pass2(self, small, big):
        return (self.valid_pass1(small, big) and self.exact_two_adjacent())

def main():
    f1 = lambda i: 1 if Password(i).valid_pass1(start, end) else 0
    f2 = lambda i: 1 if Password(i).valid_pass2(start, end) else 0
    print(sum(map(f1, range(start, end+1))), sum(map(f2, range(start, end+1))))
    return 0

# if __name__ == '__main__':
#     sys.exit(main())
