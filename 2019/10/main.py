from math import gcd, atan, pi

def main(input):
    with open(input, 'r') as f:
        raw = f.readlines()

    meteors = []
    for y, line in enumerate(raw):
        for x, char in enumerate(line):
            if char == '#':
                meteors.append((x, y))

    def count_rocks(x, y):
        x_offset, y_offset = x, y
        count = 0
        for x_m, y_m in (m for m in meteors if m != (x, y)):
            d = gcd(x_m - x_offset, y_m - y_offset)
            x_step = (x_m - x_offset) // d
            y_step = (y_m - y_offset) // d

            if not any([(x + z*x_step, y + z*y_step) in meteors for z in range(1, d)]):
                count += 1

        return count

    rock_counts = [(x, y, count_rocks(x, y)) for x, y in meteors]

    x_best, y_best, c_best = max(rock_counts, key=lambda t: t[2])
    print(f"A:: The most number of rocks seen is {c_best} at ({x_best}, {y_best})")

    meteors = []
    for y, line in enumerate(raw):
        for x, char in enumerate(line):
            if char == '#':
                meteors.append((x, y))

    def register_meteors(x, y):
        register = []
        x_offset, y_offset = x, y
        count = 0
        for x_m, y_m in (m for m in meteors if m != (x, y)):
            d = gcd(x_m - x_offset, y_m - y_offset)
            x_step = (x_m - x_offset) // d
            y_step = (y_m - y_offset) // d

            if not any([(x + z*x_step, y + z*y_step) in meteors for z in range(1, d)]):
                register.append((x_m, y_m))

        return register

    meteors_seen = register_meteors(x_best, y_best)
    meteors_seen_adj = [(x - x_best, y_best - y) for x, y in meteors_seen]

    def get_angle(dx, dy):
        if dx == 0:
            if dy > 0:
                return 0
            else:
                return pi

        if dy == 0:
            if dx < 0:
                return pi/2
            else:
                return 3*pi/2

        # Quadrant 1
        if dx > 0 and dy > 0:
            return atan(abs(dx/dy))

        # Quadrant 2
        elif dx < 0 and dy > 0:
            return 3*pi/2 + atan(abs(dy/dx))

        # Quadrant 3
        elif dx < 0 and dy < 0:
            return pi + atan(abs(dx/dy))

        # Quadrant 4
        elif dx > 0 and dy < 0:
            return pi/2 + atan(abs(dy/dx))

    meteors_angle = [(x + x_best, y_best - y, get_angle(x, y)) for x, y in meteors_seen_adj]
    meteors_angle.sort(key=lambda t: t[2])

    # x200, y200, _ = meteors_angle[199]
    # res = 100*x200 + y200
    # print(f"B:: The 200th rock destroyed is at ({x200}, {y200}), and so solution = {res}")

main('input')
main('sample1')
main('sample2')
main('sample3')
main('sample4')
main('sample5')
