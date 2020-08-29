from z3 import *

s = Solver()


# board = [
#     "01?10001?", 
#     "01?100011",
#     "011100000", 
#     "000000000", 
#     "111110011", 
#     "????1001?", 
#     "????3101?", 
#     "?????211?",
#     "?????????"]

# board = [
#     "0000001?", 
#     "0111001?",
#     "12?10011", 
#     "?2110000", 
#     "?1000000"
# ]

# board = [
#     "?20",
#     "?2?",
#     "1??"
# ]

board = [
    "?22??21?",
    "??2222?1",
    "22100111",
    "00000000",
    "00000000",
    "11122111",
    "?12??11?",
    "????????"
]

brows = len(board)
bcols = len(board[0])

def get_neighbors(row, col):
    neighbors = []
    for i in [-1, 0, 1]:
        for j in [-1, 0, 1]:
            candidate = (row + i, col + j)
            valid_row = (candidate[0] < len(board)) and (candidate[0] >= 0)
            valid_col = (candidate[1] < len(board[0])) and (candidate[1] >= 0)
            if valid_row and valid_col and candidate != (row, col):
                neighbors.append(candidate)
    
    return neighbors


def checkbomb(row, col):
    s = Solver()

    symbolic_vars = []
    for r in range(brows):
        symbolic_vars.append([])
    
    for r in range(brows):
        for c in range(bcols):
            symbolic_vars[r].append(Int(str((r,c))))

    # print(symbolic_vars)

    bomb_constraint = (symbolic_vars[row][col] == 1)
    s.add(bomb_constraint)

    for r in range(brows):
        for c in range(bcols):
            indicator_constraint = Or(symbolic_vars[r][c] == 0, symbolic_vars[r][c] == 1)
            s.add(indicator_constraint)

            # if digit, no bomb present, but we have a number of neighboring bombs
            if board[r][c].isdigit():
                nobomb_constraint = (symbolic_vars[r][c] == 0)
                s.add(nobomb_constraint)

                neighbor_coords = get_neighbors(r,c)
                neighbor_symbols = [symbolic_vars[ny][nx] for ny, nx in neighbor_coords]
    
                sum_constraint = sum(neighbor_symbols) == int(board[r][c])
                s.add(sum_constraint)
    
    if s.check() == unsat:
        print ("No bomb at", (row, col))

def mn():
    for row in range(brows):
        for col in range(bcols):
            if board[row][col] == "?":
                checkbomb(row, col)

def printboard():
    for row in board:
        row_str = "".join(["[" + i + "]" for i in row])
        print(row_str)

if __name__ == "__main__":
    mn()