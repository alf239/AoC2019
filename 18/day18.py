import matplotlib.pyplot as plt
import networkx as nx

import itertools
import queue
import pprint
from string import ascii_lowercase
from math import factorial
import numpy as np

a_lot = 999999999999999999999

# https://www.python-course.eu/python3_memoization.php
class Memoize:
    def __init__(self, fn):
        self.fn = fn
        self.memo = {}

    def __call__(self, *args):
        if args not in self.memo:
            self.memo[args] = self.fn(*args)
        return self.memo[args]

def read_map(name): 
    with open(name) as f:
        read_data = f.read()
        return read_data.split("\n")

def bfs(name, start, mm):
    seen = set()
    work = queue.SimpleQueue()
    work.put((start, name, 0))

    while not work.empty():
        ((x, y), prefix, d) = work.get()
        if (x, y) in seen: 
            continue
        seen.add((x, y))

        tile = mm[y][x]
        if tile == '#':
            continue
        elif tile.islower() or tile.isupper():
            prefix = prefix + tile
            yield (prefix, d)
        work.put(((x + 1, y), prefix, d + 1))
        work.put(((x - 1, y), prefix, d + 1))
        work.put(((x, y + 1), prefix, d + 1))
        work.put(((x, y - 1), prefix, d + 1))

def alldeps(deps, k):
    res = set()
    res |= deps[k]
    for kk in deps[k]:
        res |= alldeps(deps, kk)
    return res

def make_trie(words):
    byproduct = set()
    deps = {}
    for word, d in words:
        if word[-1].islower():
            path = deps.setdefault(word[-1], set())
            keys = set()
            for c in word[1:-1]:
                if c.islower():
                    byproduct.add(c)
                    keys.add(c)
                if c.isupper() and c.lower() not in keys:
                    path.add(c.lower())
    important = set()
    for goal, keys in deps.items():
        important.update(keys)
    for k in byproduct:
        if k not in important:
            del deps[k]
    changes = True
    while changes:
        changes = False
        for goal, keys in deps.items():
            ks = len(keys)
            tr = set()
            for k in keys:
                tr |= alldeps(deps, k)
            keys -= tr
            if len(keys) != ks:
                changes = True

    return deps

@Memoize
def dist(a, b):
    for p, d in bfs('', loc[a], mm):
        if p[-1] == b:
            return d

def price(s, p, a, mx):
    if not p:
        return a
    if a > mx:
        return a_lot
    # print("%s -> %s = %d" % (s, p[0], dist(s, p[0])))
    return price(p[0], p[1:], a + dist(s, p[0]), mx)

def min_price(ss):
    md = a_lot
    mp = "------"
    i = 0
    for s in ss:
        p = price('@', s, 0, md)
        if p < md:
            md = p
            mp = "".join(s)
            print("better: ", p, "".join(s))
        i = i + 1
        if i % 100000 == 0:
            print("iteration: ", i, "".join(s))
    return (md, mp)

mm = read_map("day18.in")
loc = {}
for y in range(len(mm)):
    for x in range(len(mm[y])):
        curr = mm[y][x]
        if curr == '@' or curr.islower() or curr.isupper(): 
            loc[curr] = (x, y)

pp = pprint.PrettyPrinter(indent=4)
trie = make_trie(bfs("@", loc['@'], mm))
pp.pprint(trie)

G = nx.DiGraph()

curated = {
    'a': {'u', 't'},
    'b': set(),
    'c': {'j'},
    'g': {'n'},
    'j': {'p'},
    'l': {'r'},
    'm': {'n'},
    'n': {'x'},
    'o': {'u', 'l'},
    'p': {'a', 'v', 'm', 'b', 'r'},
    'r': set(),
    's': {'z'},
    't': set(),
    'u': set(),
    'v': set(),
    'x': set(),
    'z': {'c'}}

for goal, deps in curated.items():
    for d in deps:
        G.add_edge(d, goal)

# plt.subplot(121)
# nx.draw_circular(G, with_labels=True)
# plt.show()

print(min_price(nx.all_topological_sorts(G)))

print(price('@', 'xvbturnloamgpcjzs', 0, 9999))
