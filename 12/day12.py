from math import gcd

def sign(x):
    if (x > 0):
        return 1
    elif (x == 0): 
        return 0
    else: 
        return -1

def period(xs):
    dxs = [0, 0, 0, 0]

    seen = {}
    count = 0
    while (tuple(xs), tuple(dxs)) not in seen:
        seen[(tuple(xs), tuple(dxs))] = count
        for i in range(0, 4):
            for j in range(0, 4):
                dxs[i] += sign(xs[j] - xs[i])
        for i in range(0, 4):
            xs[i] += dxs[i]
        count = count + 1

    return count, seen[(tuple(xs), tuple(dxs))]

# <x=-5, y=6, z=-11>
# <x=-8, y=-4, z=-2>
# <x=1, y=16, z=4>
# <x=11, y=11, z=-4>

(x, dx) = period([-5, -8, 1, 11])
(y, dy) = period([6, -4, 16, 11])
(z, dz) = period([-11, -2, 4, -4])

print(x, y, z, dx, dy, dz)

xy = x * y // gcd(x, y)
xyz = xy * z // gcd(xy, z)
print(xyz)