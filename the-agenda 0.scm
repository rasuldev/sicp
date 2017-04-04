the-agenda 0
---------
(3, a and b -> c)
(3, a and b -> c)
(5, a or b -> d)
(5, a or b -> d)
========
a 0
add-agenda(a and b -> c)
add-agenda(a or b -> d)
=========
b 0
add-agenda(a and b -> c)
add-agenda(a or b -> d)
=========
d 0
=========
e 0
=========
s 0
=========
c 0
=========
(half-adder a b s c)
	(or-gate a b d)
  	(and-gate a b c)
    (inverter c e)
    (and-gate d e s)

(set-signal a 1)
