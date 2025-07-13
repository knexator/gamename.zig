import z3

solver = z3.Solver()

board_size = [8, 8]
forbidden_cells = [
    [3, 3], [3, 4], [4, 3], [4, 4],
]
line_lens = list(range(4, 4 + 8))
line_starts = [
    [4, 5],
    [3, 5],
    [2, 4],
    [2, 3],
    [3, 2],
    [4, 2],
    [5, 3],
    [5, 4],
]

lines = [
    [z3.IntVector(f"line_{line_id}_cell_{k}", 2) for k in range(line_len)]
    for line_id, line_len in enumerate(line_lens)
]

# all positions are different
solver.add(z3.Distinct([cell[0] + cell[1] * board_size[0]
           for line in lines
           for cell in line]))

# all positions are in the grid
for line in lines:
    for cell in line:
        for coord in [0, 1]:
            solver.add(z3.And(0 <= cell[coord],
                       cell[coord] < board_size[coord]))

# no forbidden positions
for line in lines:
    for cell in line:
        for forbidden_cell in forbidden_cells:
            solver.add(z3.Not(z3.And(
                cell[0] == forbidden_cell[0],
                cell[1] == forbidden_cell[1]
            )))

# line starts at the given positions
for line, line_start in zip(lines, line_starts):
    for coord in [0, 1]:
        solver.add(line[0][coord] == line_start[coord])

# lines are continuous
for line in lines:
    for (a, b) in zip(line[:-1], line[1:]):
        delta_x = b[0] - a[0]
        delta_y = b[1] - a[1]
        solver.add(z3.Or(
            z3.And(delta_x == 0, z3.Abs(delta_y) == 1),
            z3.And(delta_y == 0, z3.Abs(delta_x) == 1)
        ))


def evalInt(model, expr) -> int:
    v = model.eval(expr)
    if isinstance(v, z3.IntNumRef):
        return v.as_long()
    else:
        raise ValueError()


if solver.check() == z3.sat:
    model = solver.model()
    visual_board = [['.' for _ in range(board_size[0])]
                    for _ in range(board_size[1])]
    for line_id, line in enumerate(lines):
        for cell in line:
            visual_board[evalInt(model, cell[1])][evalInt(
                model, cell[0])] = str(line_id)
    print('\n'.join([''.join(row) for row in visual_board]))
    # import code
    # code.interact(local=locals())
else:
    print("No solution found.")
