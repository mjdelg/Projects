"""
6.1010 Spring '23 Lab 7: Mines
"""

#!/usr/bin/env python3

import typing
import doctest

# NO ADDITIONAL IMPORTS ALLOWED!


def dump(game):
    """
    Prints a human-readable version of a game (provided as a dictionary)
    """
    for key, val in sorted(game.items()):
        if isinstance(val, list) and val and isinstance(val[0], list):
            print(f"{key}:")
            for inner in val:
                print(f"    {inner}")
        else:
            print(f"{key}:", val)


# 2-D IMPLEMENTATION


def new_game_2d(num_rows, num_cols, bombs):
    """
    Start a new game.

    Return a game state dictionary, with the 'dimensions', 'state', 'board' and
    'hidden' fields adequately initialized.

    Parameters:
       num_rows (int): Number of rows
       num_cols (int): Number of columns
       bombs (list): List of bombs, given in (row, column) pairs, which are
                     tuples

    Returns:
       A game state dictionary

    >>> dump(new_game_2d(2, 4, [(0, 0), (1, 0), (1, 1)]))
    board:
        ['.', 3, 1, 0]
        ['.', '.', 1, 0]
    dimensions: (2, 4)
    hidden:
        [True, True, True, True]
        [True, True, True, True]
    safe: 5
    state: ongoing
    """
    dim = (num_rows, num_cols)
    return new_game_nd(dim, bombs)


def dig_2d(game, row, col):
    """
    Reveal the cell at (row, col), and, in some cases, recursively reveal its
    neighboring squares.

    Update game['hidden'] to reveal (row, col).  Then, if (row, col) has no
    adjacent bombs (including diagonally), then recursively reveal (dig up) its
    eight neighbors.  Return an integer indicating how many new squares were
    revealed in total, including neighbors, and neighbors of neighbors, and so
    on.

    The state of the game should be changed to 'defeat' when at least one bomb
    is revealed on the board after digging (i.e. game['hidden'][bomb_location]
    == False), 'victory' when all safe squares (squares that do not contain a
    bomb) and no bombs are revealed, and 'ongoing' otherwise.

    Parameters:
       game (dict): Game state
       row (int): Where to start digging (row)
       col (int): Where to start digging (col)

    Returns:
       int: the number of new squares revealed

    >>> game = {'dimensions': (2, 4),
    ...         'board': [['.', 3, 1, 0],
    ...                   ['.', '.', 1, 0]],
    ...         'hidden': [[True, False, True, True],
    ...                  [True, True, True, True]],
    ...         'safe': 5,
    ...         'state': 'ongoing'}
    >>> dig_2d(game, 0, 3)
    4
    >>> dump(game)
    board:
        ['.', 3, 1, 0]
        ['.', '.', 1, 0]
    dimensions: (2, 4)
    hidden:
        [True, False, False, False]
        [True, True, False, False]
    safe: 1
    state: ongoing

    >>> game = {'dimensions': [2, 4],
    ...         'board': [['.', 3, 1, 0],
    ...                   ['.', '.', 1, 0]],
    ...         'hidden': [[True, False, True, True],
    ...                  [True, True, True, True]],
    ...         'safe': 5,
    ...         'state': 'ongoing'}
    >>> dig_2d(game, 0, 0)
    1
    >>> dump(game)
    board:
        ['.', 3, 1, 0]
        ['.', '.', 1, 0]
    dimensions: [2, 4]
    hidden:
        [False, False, True, True]
        [True, True, True, True]
    safe: 5
    state: defeat
    """
    return dig_nd(game, (row, col))


def render_2d_locations(game, xray=False):
    """
    Prepare a game for display.

    Returns a two-dimensional array (list of lists) of '_' (hidden squares),
    '.' (bombs), ' ' (empty squares), or '1', '2', etc. (squares neighboring
    bombs).  game['hidden'] indicates which squares should be hidden.  If
    xray is True (the default is False), game['hidden'] is ignored and all
    cells are shown.

    Parameters:
       game (dict): Game state
       xray (bool): Whether to reveal all tiles or just the that are not
                    game['hidden']

    Returns:
       A 2D array (list of lists)

    >>> render_2d_locations({'dimensions': (2, 4),
    ...         'state': 'ongoing',
    ...         'board': [['.', 3, 1, 0],
    ...                   ['.', '.', 1, 0]],
    ...         'hidden':  [[True, False, False, True],
    ...                   [True, True, False, True]]}, False)
    [['_', '3', '1', '_'], ['_', '_', '1', '_']]

    >>> render_2d_locations({'dimensions': (2, 4),
    ...         'state': 'ongoing',
    ...         'board': [['.', 3, 1, 0],
    ...                   ['.', '.', 1, 0]],
    ...         'hidden':  [[True, False, True, False],
    ...                   [True, True, True, False]]}, True)
    [['.', '3', '1', ' '], ['.', '.', '1', ' ']]
    """
    return render_nd(game, xray)


def render_2d_board(game, xray=False):
    """
    Render a game as ASCII art.

    Returns a string-based representation of argument 'game'.  Each tile of the
    game board should be rendered as in the function
        render_2d_locations(game)

    Parameters:
       game (dict): Game state
       xray (bool): Whether to reveal all tiles or just the ones allowed by
                    game['hidden']

    Returns:
       A string-based representation of game

    >>> render_2d_board({'dimensions': (2, 4),
    ...                  'state': 'ongoing',
    ...                  'board': [['.', 3, 1, 0],
    ...                            ['.', '.', 1, 0]],
    ...                  'hidden':  [[False, False, False, True],
    ...                            [True, True, False, True]]})
    '.31_\\n__1_'
    """
    board = render_2d_locations(game, xray)
    result = ""
    for row in board:
        result += "".join(row) + "\n"
    return result[:-1]


# N-D IMPLEMENTATION


# -----------------HELPER FUNCTIONS--------------------#
def build_nd_board(dimensions, value):
    """
    Given a list of dimensions and a value, creates a new N-d array 
    with those dimensions, where each value in the array is the given 
    value.
    """
    if len(dimensions) == 1:
        return [value] * dimensions[0]

    else:
        return [build_nd_board(dimensions[1:], value) for d in range(dimensions[0])]


def copy(board, dim):
    """
    Makes copy of n-dimensional list
    """
    if len(dim) == 1:
        return [board[i] for i in range(dim[0])]
    else:
        return [copy(board[i], dim[1:]) for i in range(dim[0])]


def get_value_nd(board, coord):
    """
    Given an N-d array and a tuple/list of coordinates, returns the value at those 
    coordinates in the array.
    """
    if len(coord) == 0:
        return board
    else:
        index = coord[0]
        return get_value_nd(board[index], coord[1:])


# print(get_value_nd([[0, 0, 0, 10], [0, 0, 0, 0]], [0,3])) # expect 10
# print(get_value_nd([[[10, 1, 2], [3,2,1]], [[1, 1, 2], [3,2,1]]], [0,0,1])) # expect 1


def get_state(game):
    """
    Returns state of game
    """
    return game["state"]


def replace_value_nd(board, coord, value):
    """
    Given an N-d array, a tuple/list of coordinates, and a value, replaces 
    the value at those coordinates in the array with the given value.
    """
    # ITERATIVE
    val_to_replace = board
    for i in range(len(coord) - 1):
        val_to_replace = val_to_replace[coord[i]]
    val_to_replace[coord[-1]] = value
    return board


# print(replace_value_nd([[0, 0, 0, 1], [0, 0, 0, 0]], [0,3], 0))


def get_neighbors(coord, dimension):
    """
    Returns all the neighbors of a given set of coordinates in a given game
    """
    directions = (1, 0, -1)
    neighbors = []

    if len(coord) == 1:  # base case
        neighbors = [
            (d + coord[0],)
            for d in directions
            if (d + coord[0] >= 0 and d + coord[0] < dimension[0])
        ]
        return neighbors
    else:
        next_r = [
            (d + coord[0],)
            for d in directions
            if (d + coord[0] >= 0 and d + coord[0] < dimension[0])
        ]
        next_c = get_neighbors(coord[1:], dimension[1:])
        temp = []
        for current in next_r:
            for digit in next_c:
                temp.append(current + digit)
        neighbors = temp
        return neighbors


def get_all_coord(dim):  # is this function for get neighbors?
    """
    Returns all possible coordinates in a given board
    """
    all_c = []
    if len(dim) == 1:
        all_c = [(num,) for num in range(dim[0])]
        return all_c
    else:
        next_i = [(num,) for num in range(dim[0])]
        next_j = get_all_coord(dim[1:])
        temp = []
        for current in next_i:
            for digit in next_j:
                temp.append(current + digit)
        all_c = temp
        return all_c


def new_game_nd(dimensions, bombs):
    """
    Start a new game.

    Return a game state dictionary, with the 'dimensions', 'state', 'board' and
    'hidden' fields adequately initialized.


    Args:
       dimensions (tuple): Dimensions of the board
       bombs (list): Bomb locations as a list of tuples, each an
                     N-dimensional coordinate

    Returns:
       A game state dictionary

    >>> g = new_game_nd((2, 4, 2), [(0, 0, 1), (1, 0, 0), (1, 1, 1)])
    >>> dump(g)
    board:
        [[3, '.'], [3, 3], [1, 1], [0, 0]]
        [['.', 3], [3, '.'], [1, 1], [0, 0]]
    dimensions: (2, 4, 2)
    hidden:
        [[True, True], [True, True], [True, True], [True, True]]
        [[True, True], [True, True], [True, True], [True, True]]
    safe: 13
    state: ongoing
    """
    s_spaces = 1
    for num in dimensions:
        s_spaces = s_spaces * num
    s_spaces = s_spaces - len(bombs)

    # build board with only zeros (no bombs)
    board = build_nd_board(dimensions, 0)
    # add bombs
    for bomb in bombs:
        board = replace_value_nd(board, bomb, ".")
    hidden = build_nd_board(dimensions, True)
    # change values that are blank
    all_coord = get_all_coord(dimensions)
    # print(all_coord, "all")
    for coord in all_coord:
        bombs = 0
        if get_value_nd(board, coord) == 0:
            # find neighbors
            neighbors = get_neighbors(coord, dimensions)
            for neigh in neighbors:
                if get_value_nd(board, neigh) == ".":
                    bombs += 1
            board = replace_value_nd(board, coord, bombs)

    return {
        "dimensions": dimensions,
        "board": board,
        "hidden": hidden,
        "safe": s_spaces,
        "state": "ongoing",
    }


def dig_nd(game, coordinates):
    """
    Recursively dig up square at coords and neighboring squares.

    Update the hidden to reveal square at coords; then recursively reveal its
    neighbors, as long as coords does not contain and is not adjacent to a
    bomb.  Return a number indicating how many squares were revealed.  No
    action should be taken and 0 returned if the incoming state of the game
    is not 'ongoing'.

    The updated state is 'defeat' when at least one bomb is revealed on the
    board after digging, 'victory' when all safe squares (squares that do
    not contain a bomb) and no bombs are revealed, and 'ongoing' otherwise.

    Args:
       coordinates (tuple): Where to start digging

    Returns:
       int: number of squares revealed

    >>> g = {'dimensions': (2, 4, 2),
    ...      'board': [[[3, '.'], [3, 3], [1, 1], [0, 0]],
    ...                [['.', 3], [3, '.'], [1, 1], [0, 0]]],
    ...      'hidden': [[[True, True], [True, False], [True, True],
    ...                [True, True]],
    ...               [[True, True], [True, True], [True, True],
    ...                [True, True]]],
    ...      'safe': 13,
    ...      'state': 'ongoing'}
    >>> dig_nd(g, (0, 3, 0))
    8
    >>> dump(g)
    board:
        [[3, '.'], [3, 3], [1, 1], [0, 0]]
        [['.', 3], [3, '.'], [1, 1], [0, 0]]
    dimensions: (2, 4, 2)
    hidden:
        [[True, True], [True, False], [False, False], [False, False]]
        [[True, True], [True, True], [False, False], [False, False]]
    safe: 5
    state: ongoing
    >>> g = {'dimensions': (2, 4, 2),
    ...      'board': [[[3, '.'], [3, 3], [1, 1], [0, 0]],
    ...                [['.', 3], [3, '.'], [1, 1], [0, 0]]],
    ...      'hidden': [[[True, True], [True, False], [True, True],
    ...                [True, True]],
    ...               [[True, True], [True, True], [True, True],
    ...                [True, True]]],
    ...      'safe': 13,
    ...      'state': 'ongoing'}
    >>> dig_nd(g, (0, 0, 1))
    1
    >>> dump(g)
    board:
        [[3, '.'], [3, 3], [1, 1], [0, 0]]
        [['.', 3], [3, '.'], [1, 1], [0, 0]]
    dimensions: (2, 4, 2)
    hidden:
        [[True, False], [True, False], [True, True], [True, True]]
        [[True, True], [True, True], [True, True], [True, True]]
    safe: 13
    state: defeat
    """
    hidden_squares = game["safe"]

    def recursive_dig(game, coordinates, hidden_squares, check):
        # print("hi")
        if (
            get_state(game) == "defeat"
            or get_state(game) == "victory"
            or get_value_nd(game["hidden"], coordinates) is False
        ):
            return 0

        game["hidden"] = replace_value_nd(game["hidden"], coordinates, False)
        revealed = 1
        # hidden_squares = hidden_squares - 1
        # game['safe'] = hidden_squares
        # print(hidden_squares, "hidden")

        if get_value_nd(game["board"], coordinates) == ".":
            game["state"] = "defeat"
            return 1

        # finds number of neighbors that do not have mines in them
        # print(get_value_nd(game['board'], coordinates))
        if get_value_nd(game["board"], coordinates) == 0:
            neighbors = get_neighbors(coordinates, game["dimensions"])
            for neigh in neighbors:
                if get_value_nd(game["board"], neigh) != ".":
                    revealed += recursive_dig(game, neigh, hidden_squares, check=False)
        hidden_squares = hidden_squares - revealed
        if check is True:
            game["safe"] = hidden_squares
        # print(hidden_squares, "hidden")
        if hidden_squares == 0:
            game["state"] = "victory"
        # print(game)
        return revealed

    return recursive_dig(game, coordinates, hidden_squares, check=True)


def render_nd(game, xray=False):
    """
    Prepare the game for display.

    Returns an N-dimensional array (nested lists) of '_' (hidden squares), '.'
    (bombs), ' ' (empty squares), or '1', '2', etc. (squares neighboring
    bombs).  The game['hidden'] array indicates which squares should be
    hidden.  If xray is True (the default is False), the game['hidden'] array
    is ignored and all cells are shown.

    Args:
       xray (bool): Whether to reveal all tiles or just the ones allowed by
                    game['hidden']

    Returns:
       An n-dimensional array of strings (nested lists)

    >>> g = {'dimensions': (2, 4, 2),
    ...      'board': [[[3, '.'], [3, 3], [1, 1], [0, 0]],
    ...                [['.', 3], [3, '.'], [1, 1], [0, 0]]],
    ...      'hidden': [[[True, True], [True, False], [False, False],
    ...                [False, False]],
    ...               [[True, True], [True, True], [False, False],
    ...                [False, False]]],
    ...      'safe': 13,
    ...      'state': 'ongoing'}
    >>> render_nd(g, False)
    [[['_', '_'], ['_', '3'], ['1', '1'], [' ', ' ']],
     [['_', '_'], ['_', '_'], ['1', '1'], [' ', ' ']]]

    >>> render_nd(g, True)
    [[['3', '.'], ['3', '3'], ['1', '1'], [' ', ' ']],
     [['.', '3'], ['3', '.'], ['1', '1'], [' ', ' ']]]
    """
    board = copy(game["board"], game["dimensions"])  # recursive function to make copy
    # board = [row[:] for row in game['board']]
    all_coords = get_all_coord(game["dimensions"])
    for coord in all_coords:
        cur_val = get_value_nd(board, coord)
        if xray is True:
            if cur_val != 0:
                board = replace_value_nd(board, coord, str(cur_val))
            else:
                board = replace_value_nd(board, coord, " ")
        else:
            if get_value_nd(game["hidden"], coord) is True:
                board = replace_value_nd(board, coord, "_")
            else:
                if cur_val != 0:
                    board = replace_value_nd(board, coord, str(cur_val))
                else:
                    board = replace_value_nd(board, coord, " ")
    return board


if __name__ == "__main__":
    # Test with doctests. Helpful to debug individual lab.py functions.
    _doctest_flags = doctest.NORMALIZE_WHITESPACE | doctest.ELLIPSIS
    doctest.testmod(optionflags=_doctest_flags)  # runs ALL doctests
    # new_game_nd((2, 4, 2), [(0, 0, 1), (1, 0, 0), (1, 1, 1)])
    g = {
        "dimensions": (2, 4, 2),
        "board": [
            [[3, "."], [3, 3], [1, 1], [0, 0]],
            [[".", 3], [3, "."], [1, 1], [0, 0]],
        ],
        "hidden": [
            [[True, True], [True, False], [False, False], [False, False]],
            [[True, True], [True, True], [False, False], [False, False]],
        ],
        "state": "ongoing",
    }
    # print(render_nd(g, False))
    # print(render_nd(g, True))
    # print(dig_nd(g, (0, 3, 0)))

    # Alternatively, can run the doctests JUST for specified function/methods,
    # e.g., for render_2d_locations or any other function you might want.  To
    # do so, comment out the above line, and uncomment the below line of code.
    # This may be useful as you write/debug individual doctests or functions.
    # Also, the verbose flag can be set to True to see all test results,
    # including those that pass.
    #
    # doctest.run_docstring_examples(
    #    render_2d_locations,
    #    globals(),
    #    optionflags=_doctest_flags,
    #    verbose=False
    # )
    pass
