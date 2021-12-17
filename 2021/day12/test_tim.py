lines = open('test1', 'r').readlines()

symbols = set()
for line in lines:
    l, r = line.strip().split('-')
    symbols.add(l)
    symbols.add(r)

symbols = list(symbols)

class Cave:
    def __init__(self, sym):
        self.sym = sym
        self.nbs = []
        self.size = "small" if sym == sym.lower() else "big"

caves = {sym: Cave(sym) for sym in symbols}

for line in lines:
    l, r = line.strip().split('-')
    caves[l].nbs.append(caves[r])
    caves[r].nbs.append(caves[l])

def can_explore_a(cave, path):
    not_start = cave.sym != 'start'
    not_end = cave.sym != 'end'
    big_criteria = cave.size == 'big'
    small_criteria = cave.size == 'small' and cave.sym not in path
    return not_start and not_end and (big_criteria or small_criteria)

def can_explore_b(cave, path):
    not_start = cave.sym != 'start'
    not_end = cave.sym != 'end'

    if cave.size == 'big':
        size_criteria = True
    elif cave.size == 'small':
        used_chance = any([path.count(c) > 1 for c in path if caves[c].size == 'small'])

        if used_chance:
            size_criteria = cave.sym not in path
        else:
            size_criteria = True

    return not_start and not_end and size_criteria

def explore(path, can_explore_rule):
    print(path)
    for nb_cave in caves[path[-1]].nbs:
        if can_explore_rule(nb_cave, path):
            explore(path.copy() + [nb_cave.sym], can_explore_rule)
        elif nb_cave.sym == 'end':
            new_path = ','.join(path.copy() + ['end'])
            ALL_PATHS.append(new_path)

ALL_PATHS = []
explore(['start'], can_explore_a)
res_a = len(ALL_PATHS)
print(f"A ::: Number of paths = {res_a}")

ALL_PATHS = []
explore(['start'], can_explore_b)
res_b = len(ALL_PATHS)
print(f"B ::: Number of paths = {res_b}")
