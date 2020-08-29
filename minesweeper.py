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

board = [
    "?20",
    "?2?",
    "1??"
]

board_x = len(board)
board_y = len(board[0])


def get_neighbors(row, col):
    def is_adjacent(tx, ty):
        return ((abs(row - tx) <= 1) and (abs(col - ty) <= 1)) and ((row, col) != (tx, ty))

    neighbors = []
    for x in range(board_x):
        for y in range(board_y):
            if is_adjacent(x,y):
                neighbors.append((x, y))
    
    return neighbors

def check_bomb(row, col):
    # 1. (Symbols)
    # each symbol is either 0 or 1 depending on if we know a bomb is on that square or not
    symbols = [[Int(str(x) + ", " + str(y)) for x in range(board_x)] for y in range(board_y)] 

    # initialize whatever variables we can
    for x in range(board_x):
        for y in range(board_y):
            # if its a digit, then we know there isn't a bomb there, so we initialize to zero
            if board[x][y].isdigit():
                s.add(symbols[x][y] == 0)
            # otherwise, it's a ? and we don't know if there is a bomb there or not
            # obviously we don't have any uncovered cells that are bombs (because then the game would be over)
    


    # # 2. (Clauses)
    # # construct the system of equations
    for x in range(board_x):
        for y in range(board_y):
            # we can only construct an equation if the cell is uncovered and has a number (bc if it has a ? then the ? doesn't give us info)
            if board[x][y].isdigit():
                val = int(board[x][y])

                neighbor_coordinates = get_neighbors(x, y)
                neighbor_symbols = []
                for n in neighbor_coordinates:
                    nx = n[0]
                    ny = n[1]
                    neighbor_symbols.append(symbols[nx][ny])

                curr_clause = sum(neighbor_symbols) == val
                s.add(curr_clause)

    # clause for placing bomb at candidate spot
    s.add(symbols[row][col] == 1)

    result = s.check()
    if result == unsat:
        print("No bomb at", row, col)
    elif result == sat:
        print("There is a bomb at ", row, col)
    


def mn():
    for x in range(board_x):
        for y in range(board_y):
            # only uncovered cells are candidates for bombs
            if board[x][y] == "?":
                check_bomb(x, y)





# References
# http://www.cs.toronto.edu/~victorn/tutorials/z3_SAT_2019/index.html