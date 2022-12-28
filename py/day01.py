import sys

def main () -> int:
    filename = sys.argv[1]
    with open(filename) as f:
        lines = list(f)
        ans1 = sum(map(lambda line: fuel(int(line)), lines))
        ans2 = sum(map(lambda line: fuel_c(int(line)), lines))
        print(ans1, ans2)
        return 0

def fuel(mass) -> int:
    """
    >>> fuel(14)
    2
    >>> fuel(12)
    2
    >>> fuel(1969)
    654
    >>> fuel(100756)
    33583
    """
    return (mass // 3) - 2

def fuel_c(mass) -> int:
    """
    >>> fuel_c(14)
    2
    >>> fuel_c(1969)
    966
    >>> fuel_c(100756)
    50346
    """
    total = fuel(mass)
    subfuel = total
    while (subfuel := fuel(subfuel)) > 0:
        total = total + subfuel
    return total
    
def fuel_total(fileobj) -> int:
    ans = 0
    for line in fileobj:
        ans = ans + fuel(int(line))
    return ans

if __name__ == '__main__':
    import doctest
    doctest.testmod()
    sys.exit(main())
