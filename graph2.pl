%graph2

succ(nil, nil, 0, a).
succ(a, ab, 1, b).
succ(a, ac, 2, c).
succ(a, ad, 3, d).
succ(a, ai, 3, i).
succ(b, be, 1, e).
succ(c, ce, 2, e).
succ(c, cg, 4, g).
succ(d, df, 1, f).
succ(d, dg, 2, g).
succ(d, dj, 4, j).
succ(e, eh, 10, h).
succ(f, fh, 9, h).
succ(g, gh, 5, h).

hScore(a, 15).
hScore(b, 11).
hScore(c, 10).
hScore(d, 11).
hScore(e, 6).
hScore(f, 5).
hScore(g, 6).
hScore(h, 0).
hScore(i, 20).
hScore(j, 20).

goal(h).

/**
*		 a ______3_____ i
*	   / | \
*	  1  2   3	
*	 /   |    \	
*	b	 c 	   d_____4____ j
*	|   / \   /|
*	1  2   \ / 2
*	| /    1 4 |
*	|/    /   \|
*	e	 f	   g
*	 \   |    /
*	 10  9   5
*	   \ |  /
*		  h
*/
