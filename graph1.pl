%graph1

succ(nil, nil, 0, a).
succ(a, ax, 1, x).
succ(a, ay, 2, y).
succ(x, xz, 1, z).
succ(y, yz, 1, z).
succ(z, zb, 10, b).

hScore(a, 10).
hScore(x, 8).
hScore(y, 8).
hScore(z, 6).
hScore(b, 0).

goal(b).

