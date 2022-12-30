import sys

def total_cumulative_orbits(om):
    q = ['COM'] # create queue for breadth-first traversal of the tree
    start = 0
    end = 1
    nl = parent_map_to_node_list(om)
    while (start < end):
        parent = q[start]
        children = nl[parent] if parent in nl else []
        start += 1
        end += len(children)
        q = q + children
    corbits = {'COM':0}
    for node in q[1:]: # iterate over the queue and fill up the orbits
        corbits[node] = corbits[om[node]]+1
    return corbits

def path_to(pm, node, root):
    "Given parent map PM, find the path from NODE to ROOT"
    cnode = node
    path = []
    while cnode != root:
        path.append(cnode)
        cnode = pm[cnode]
    path.append(root)
    return path
        
def parent_map_to_node_list(pm):
    nl = dict()
    for child, parent in pm.items():
        if parent in nl:
            nl[parent].append(child)
        else:
            nl[parent] = [child]
    return nl

def common_prefix(ls1, ls2):
    """Given two lists LS1 and LS2 return the common prefix.
    >>> common_prefix([1, 2, 3, 4, 5], [1, 2, 3, 9, 10])
    [1, 2, 3]
    >>> common_prefix([1, 2, 3], ['a', 'b', 'c'])
    []
    >>> common_prefix([], [1, 2, 3])
    []
    """
    max_len = min(len(ls1), len(ls2))
    common = []
    for i in range(max_len):
        if ls1[i] == ls2[i]:
            common.append(ls1[i])
        else:
            return common
    return common

def find_nearest_orbit(pm):
    path_to_san = path_to(pm, 'SAN', 'COM')
    path_to_you = path_to(pm, 'YOU', 'COM')
    path_to_san.reverse()
    path_to_you.reverse()
    common = common_prefix(path_to_san, path_to_you)
    return common.pop()

def main(filename):
    with open(filename) as f:
        lines = list(f)
        orbitmap = dict()
        for line in lines:
            (center, moon) = line.rstrip("\n").split(")")
            orbitmap[moon] = center
        print(sum(total_cumulative_orbits(orbitmap).values()))
        nearest = find_nearest_orbit(orbitmap)
        lsan = len(path_to(orbitmap, 'SAN', nearest)) - 1
        lyou = len(path_to(orbitmap, 'YOU', nearest)) - 1
        print(lsan+lyou-2)
    return 0

if __name__ == '__main__' and len(sys.argv) > 1:
    import doctest
    doctest.testmod()
    sys.exit(main(sys.argv[1]))
