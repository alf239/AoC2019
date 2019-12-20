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

    def read_portals(self):
        external = {}
        internal = {}

        height = len(self.maze)
        for y in range(height):
            width = len(self.maze[y])
            for x in range(width):
                tile = self.get(x, y)
                if tile.isupper():
                    right = self.get(x + 1, y)
                    if right.isupper(): # horisontal portal
                        name = tile + right
                        if self.get(x + 2, y) == '.':
                            if x < width // 2: 
                                external[name] = (x + 2, y)
                            else:
                                internal[name] = (x + 2, y)
                        if self.get(x - 1, y) == '.':
                            if x < width // 2: 
                                internal[name] = (x - 1, y)
                            else:
                                external[name] = (x - 1, y)
                    down = self.get(x, y + 1)
                    if down.isupper(): # vertical portal
                        name = tile + down
                        if self.get(x, y + 2) == '.':
                            if y < width // 2:
                                external[name] = (x, y + 2)
                            else:
                                internal[name] = (x, y + 2)
                        if self.get(x, y - 1) == '.':
                            if y < width // 2:
                                internal[name] = (x, y - 1)
                            else:
                                external[name] = (x, y - 1)
        return (external, internal)

    def base_edges(self):
        for y in range(len(self.maze)):
            for x in range(len(self.maze[y])):
                tile = self.get(x, y)
                if tile != '.':
                    continue
                if self.get(x + 1, y) == '.':
                    yield ((x, y), (x + 1, y))
                if self.get(x, y + 1) == '.':
                    yield ((x, y), (x, y + 1))


    def task1_graph(self):
        portals = self.read_portals()

        G = nx.Graph()

        for edge in self.base_edges():
            G.add_edge(*edge)

        external, internal = self.read_portals()
        for name in internal:
            G.add_edge(internal[name], external[name])

        return G

    def task1_start(self):
        return self.read_portals()[0]['AA']

    def task1_end(self):
        return self.read_portals()[0]['ZZ']

    def task2_graph(self, n):
        portals = self.read_portals()

        G = nx.Graph()

        external, internal = self.read_portals()

        for l in range(n):
            for a, b in self.base_edges():
                G.add_edge((a, l), (b, l))

            for name in internal:
                G.add_edge((internal[name], l), (external[name], l + 1))

        return G

    def task2_start(self):
        return (self.read_portals()[0]['AA'], 0)

    def task2_end(self):
        return (self.read_portals()[0]['ZZ'], 0)

def load_maze(data):
    with open(data, 'r') as f:
        maze = Maze(f.read())
    return maze


data = "day20.in"

maze = load_maze(data)

print("=== Task 1 ===")
G = maze.task1_graph()

path = nx.shortest_path(G, maze.task1_start(), maze.task1_end())

print(len(path) - 1)

print("=== Task 2 ===")
G = maze.task2_graph(200)

path = nx.shortest_path(G, maze.task2_start(), maze.task2_end())

print(len(path) - 1)
