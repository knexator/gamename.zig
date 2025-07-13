from ortools.sat.python import cp_model

def addVec(a, b):
    return [a[0] + b[0], a[1] + b[1]]

model = cp_model.CpModel()

board_size = [9, 8]
octopus_position = [3, 3]
forbidden_cells = [
    [3, 1], [8, 3], [5, 7], [8, 5], [1, 2], [7, 6] # [6, 7], [1, 2], [6, 1],
] + [addVec(octopus_position, vec) for vec in [[0, 0], [0, 1], [1, 0], [1, 1]]]
line_lens = list(range(4, 4 + 8))
line_starts_raw = [
    [3, 2],
    [4, 2],
    [5, 3],
    [5, 4],
    [4, 5],
    [3, 5],
    [2, 4],
    [2, 3],
]
line_starts = [line_starts_raw["456789AB".index(
    hex(line_len)[2:].upper())] for line_len in line_lens]

# 456789AB => yes
# 546789AB => no
# B456789A => yes
# 485B6A79 => yes
# 9485B6A7 => no
# 456879AB => yes
# 468A579B => yes
# B468A579 => yes
# in a 8x8 board, even corners leave/take a gap
# full rule: even corner must have an odd neighbour

# Single solution!
line_ends = [
    None,  # [5, 1],
    None,  # [7, 3],
    [6, 5],
    [6, 7],
    None,  # [5, 7],
    None,  # [0, 4],
    [1, 2],
    [6, 1],
]

#   01234567
#
# 0 AABBBBBB
# 1 AAB444BB
# 2 AAB45555
# 3 AAB..665
# 4 9AA..766
# 5 99998766
# 6 99888777
# 7 99888877

lines = [[[
    model.NewIntVar(0, board_size[0] - 1, f"line_{line_id}_cell_{k}_x"),
    model.NewIntVar(0, board_size[1] - 1, f"line_{line_id}_cell_{k}_y")
] for k in range(line_len)] for line_id, line_len in enumerate(line_lens)]

# All positions are different
# Convert 2D coordinates to 1D indices for the AllDifferent constraint
position_vars = []
for line_id, line in enumerate(lines):
    for cell_id, cell in enumerate(line):
        # Create a variable for the 1D position (x + y * board_width)
        pos_var = model.NewIntVar(0, board_size[0] * board_size[1] - 1,
                                  f"line_{line_id}_cell_{cell_id}_both")
        model.Add(pos_var == cell[0] + cell[1] * board_size[0])
        position_vars.append(pos_var)
model.AddAllDifferent(position_vars)

# No forbidden positions
for pos in position_vars:
    for forbidden_cell in forbidden_cells:
        cell_pos = forbidden_cell[0] + forbidden_cell[1] * board_size[0]
        model.Add(pos != cell_pos)

# Line starts at the given positions
for line, line_start in zip(lines, line_starts):
    model.Add(line[0][0] == line_start[0])
    model.Add(line[0][1] == line_start[1])

# # Line ends at the given positions
# for line, line_end in zip(lines, line_ends):
#     if line_end is None:
#         continue
#     model.Add(line[-1][0] == line_end[0])
#     model.Add(line[-1][1] == line_end[1])

# Lines are continuous (adjacent cells are connected horizontally or vertically)
all_deltas = []
for line_id, line in enumerate(lines):
    line_deltas = []
    for i in range(len(line) - 1):
        a = line[i]
        b = line[i + 1]

        # Create variables for the deltas
        delta_x = model.NewIntVar(-1, 1, f"delta_x_{line_id}_{i}")
        delta_y = model.NewIntVar(-1, 1, f"delta_y_{line_id}_{i}")

        model.Add(delta_x == b[0] - a[0])
        model.Add(delta_y == b[1] - a[1])

        line_deltas.append((delta_x, delta_y))

        valid_deltas = [(-1, 0), (1, 0), (0, -1), (0, 1)]

        # Reified constraints for (dx, dy) being one of valid_deltas
        bool_vars = []
        for (vx, vy) in valid_deltas:
            b = model.NewBoolVar(f'delta_{vx}_{vy}')
            model.Add(delta_x == vx).OnlyEnforceIf(b)
            model.Add(delta_y == vy).OnlyEnforceIf(b)
            bool_vars.append(b)

        # Exactly one valid delta must be chosen
        model.AddExactlyOne(bool_vars)
    all_deltas.append(line_deltas)

# # All twists
# for line_deltas in all_deltas:
#     for i in range(len(line_deltas) - 1):
#         dx1, dy1 = line_deltas[i]
#         dx2, dy2 = line_deltas[i + 1]
#         model.Add(dx1 != dx2)
#         model.Add(dy1 != dy2)


# Solve the model
class SolutionCounter(cp_model.CpSolverSolutionCallback):
    def __init__(self):
        cp_model.CpSolverSolutionCallback.__init__(self)
        self.solution_count = 0

    def OnSolutionCallback(self):
        self.solution_count += 1
        if self.solution_count % 100 == 0:
            print('Solution #', self.solution_count)


solver = cp_model.CpSolver()
counter = SolutionCounter()
solver.SearchForAllSolutions(model, counter)
print('Number of solutions:', counter.solution_count)

status = solver.Solve(model)
if status == cp_model.OPTIMAL or status == cp_model.FEASIBLE:
    visual_board = [['.' for _ in range(board_size[0])]
                    for _ in range(board_size[1])]
    for line_id, line in enumerate(lines):
        for cell in line:
            x = solver.Value(cell[0])
            y = solver.Value(cell[1])
            visual_board[y][x] = hex(line_lens[line_id])[2:].upper()
    for (x, y) in forbidden_cells:
        visual_board[y][x] = '#'
    print('\n'.join([''.join(row) for row in visual_board]))
else:
    print("No solution found.")
