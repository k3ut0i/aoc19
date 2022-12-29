def between(e, x, y) -> bool:
    if (x <= y):
        return x <= e and e <= y
    else:
        return y <= e and e <= x
