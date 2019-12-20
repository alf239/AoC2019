import networkx as nx

class Maze:
    def __init__(self, s):
        self.maze = s.split('\n')

    def get(self, x, y):
        if x < 0 or y < 0 or y >= len(self.maze):
            return ' '
        if x >= len(self.maze[y]):
            return ' '
        return self.maze[y][x]


def load_maze(data):
    with open(data, 'r') as f:
        maze = Maze(f.read())
    return maze

def read_portals(maze):
    portals = {}

    for y in range(len(maze.maze)):
        for x in range(len(maze.maze[y])):
            tile = maze.get(x, y)
            if tile.isupper():
                right = maze.get(x + 1, y)
                if right.isupper(): # horisontal portal
                    name = tile + right
                    if maze.get(x + 2, y) == '.':
                        portals.setdefault(name, []).append((x + 2, y))
                    if maze.get(x - 1, y) == '.':
                        portals.setdefault(name, []).append((x - 1, y))
                down = maze.get(x, y + 1)
                if down.isupper(): # horisontal portal
                    name = tile + down
                    if maze.get(x, y + 2) == '.':
                        portals.setdefault(name, []).append((x, y + 2))
                    if maze.get(x, y - 1) == '.':
                        portals.setdefault(name, []).append((x, y - 1))
    return portals


data = "day20.in"

maze = load_maze(data)

portals = read_portals(maze)

G = nx.Graph()

for y in range(len(maze.maze)):
    for x in range(len(maze.maze[y])):
        tile = maze.get(x, y)
        if tile != '.':
            continue
        if maze.get(x + 1, y) == '.':
            G.add_edge((x, y), (x + 1, y))
        if maze.get(x, y + 1) == '.':
            G.add_edge((x, y), (x, y + 1))

for name, link in portals.items():
    if len(link) == 2:
        G.add_edge(*link)

path = nx.shortest_path(G, portals['AA'][0], portals['ZZ'][0])

print(len(path) - 1, path)