(-2 * math.Pi - 0.0001).wrap2(math.Pi)

15 * 24
90/15.0

(2 / math.sqrt(3)) * 92

3 / math.sqrt(2)
53.33333f * 2


92*2.0

300.0/46 * 100

300.0/184

math.atan2(0, 2)

(1.5, 0.0) --> -1.2 dB

(1.5, 1.0) ->   -5 dB

(1.5, 2.0) -> -11.6 dB

(1.5, 3.0) -> -16.0 dB

(1.5, 4.0) -> -18.4 dB

(1.5, 5.0) -> -21.2 dB

(1.5, 6.0) -> -22.1 dB

def dist(h: Double) = (h * h + 1.5 * 1.5).sqrt

dist(6)

val x = Vector(
  1.5  ->  -1.2,
  1.8  ->  -5.0,
  2.5  -> -11.6,
  3.35 -> -16.0,
  4.27 -> -18.4,
  5.22 -> -21.2,
  6.18 -> -22.1
)

// http://www.had2know.com/academics/quadratic-regression-calculator.html
// y = 1.1034x^2 - 12.6433x + 14.3775

def approx(h: Double): Double = {
  val d = dist(h)
  1.1034 * d.squared - 12.6433 * d + 14.3775
}

(0.0 to 6.0 by 0.5).map(h => dist(h) -> approx(h)).plot()

// x.map { case (x, db) => (x, db.dbamp) } .plot()
x.plot()