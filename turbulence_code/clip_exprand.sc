Array.exprand(100, 0.01, 6).mean

Array.exprand(100, 0.03, 6).max(3).plot(discrete: true)

Array.exprand(100, 0.05, 6).max(1).plot(discrete: true)

5.0e-4 * 15 / 10

0.00075e0
7.5e-4

~test = { arg min;
	Array.exprand(100, min * 0.05, (min * 10).min(12)).max(min).plot(discrete: true)
}

~test.(0.1)
~test.(1)
~test.(3)
