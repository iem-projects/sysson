import at.iem.sysson.turbulence._
import Turbulence._

val MatrixNew = Map[DymGrid, Spk](
  DymGrid( 4, 0) -> Spk(17),
  DymGrid( 6, 0) -> Spk(29),
  DymGrid( 8, 0) -> Spk(30),
  DymGrid( 5, 0) -> Spk(18),
  DymGrid( 7, 0) -> Spk(31),
  DymGrid( 6, 1) -> Spk(19),
  DymGrid( 3, 1) -> Spk(21),
  DymGrid( 5, 1) -> Spk(20),
  DymGrid( 7, 1) -> Spk(32),
  DymGrid( 9, 1) -> Spk(13),
  DymGrid(11, 1) -> Spk( 5),
  DymGrid(13, 1) -> Spk( 3),
  DymGrid( 4, 2) -> Spk(22),
  DymGrid( 6, 2) -> Spk(23),
  DymGrid( 8, 2) -> Spk(14),
  DymGrid(12, 2) -> Spk( 4),
  DymGrid( 5, 2) -> Spk(24),
  DymGrid( 7, 2) -> Spk(25),
  DymGrid( 9, 2) -> Spk(15),
  DymGrid(11, 2) -> Spk( 6),
  DymGrid( 0, 3) -> Spk(41),
  DymGrid( 2, 3) -> Spk(42),
  DymGrid( 4, 3) -> Spk(33),
  DymGrid( 6, 3) -> Spk(34),
  DymGrid( 8, 3) -> Spk(26),
  DymGrid(10, 3) -> Spk( 9),
  DymGrid(12, 3) -> Spk( 7),
  DymGrid( 1, 3) -> Spk(43),
  DymGrid( 3, 3) -> Spk(39),
  DymGrid( 5, 3) -> Spk(35),
  DymGrid( 7, 3) -> Spk(27),
  DymGrid( 9, 3) -> Spk(16),
  DymGrid(11, 3) -> Spk( 8),
  DymGrid( 2, 4) -> Spk(38),
  DymGrid( 4, 4) -> Spk(37),
  DymGrid(10, 4) -> Spk(10),
  DymGrid( 1, 4) -> Spk(44),
  DymGrid( 3, 4) -> Spk(40),
  DymGrid( 5, 4) -> Spk(36),
  DymGrid( 7, 4) -> Spk(28),
  DymGrid( 9, 4) -> Spk(11),
  DymGrid(11, 4) -> Spk(12)
)

val MatrixOld = Map[DymGrid, Spk](
  DymGrid( 4, 0) -> Spk( 3),
  DymGrid( 6, 0) -> Spk( 5),
  DymGrid( 8, 0) -> Spk( 4),
  DymGrid( 5, 0) -> Spk( 6),
  DymGrid( 7, 0) -> Spk( 7),
  DymGrid( 6, 1) -> Spk( 8),
  DymGrid( 3, 1) -> Spk(21),
  DymGrid( 5, 1) -> Spk(25),
  DymGrid( 7, 1) -> Spk(26),
  DymGrid( 9, 1) -> Spk(33),
  DymGrid(11, 1) -> Spk(41),
  DymGrid(13, 1) -> Spk(45),
  DymGrid( 4, 2) -> Spk(22),
  DymGrid( 6, 2) -> Spk(27),
  DymGrid( 8, 2) -> Spk(34),
  DymGrid(12, 2) -> Spk(42),
  DymGrid( 5, 2) -> Spk(23),
  DymGrid( 7, 2) -> Spk(29),
  DymGrid( 9, 2) -> Spk(35),
  DymGrid(11, 2) -> Spk(43),
  DymGrid( 0, 3) -> Spk(10),
  DymGrid( 2, 3) -> Spk(13),
  DymGrid( 4, 3) -> Spk(17),
  DymGrid( 6, 3) -> Spk(28),
  DymGrid( 8, 3) -> Spk(30),
  DymGrid(10, 3) -> Spk(37),
  DymGrid(12, 3) -> Spk(46),
  DymGrid( 1, 3) -> Spk(11),
  DymGrid( 3, 3) -> Spk(14),
  DymGrid( 5, 3) -> Spk(24),
  DymGrid( 7, 3) -> Spk(31),
  DymGrid( 9, 3) -> Spk(36),
  DymGrid(11, 3) -> Spk(44),
  DymGrid( 2, 4) -> Spk(15),
  DymGrid( 4, 4) -> Spk(18),
  DymGrid(10, 4) -> Spk(38),
  DymGrid( 1, 4) -> Spk(16),
  DymGrid( 3, 4) -> Spk(19),
  DymGrid( 5, 4) -> Spk(20),
  DymGrid( 7, 4) -> Spk(32),
  DymGrid( 9, 4) -> Spk(39),
  DymGrid(11, 4) -> Spk(40)
)

val m = MatrixOld.map { case (key, old) =>
  old.num -> MatrixNew(key).num
}

m.foreach { case (old, nu) =>
  println(s"cp conv_OLD/fsm${old}conv.aif conv/fsm${nu}conv.aif")
}

//////////////////////////////

val gainsOld = Map[Int, Double](
  3 -> -14.8,  4 -> -19.3,  5 -> -21.6,  6 -> -20.8,  7 -> -17.7,  8 -> -20.1,
  10 -> -21.1, 11 -> -21.6, 13 -> -16.3, 14 -> -19.2, 15 -> -16.7, 16 -> -18.9,
  17 -> -20.0, 18 -> -17.5, 19 -> -18.8, 20 -> -18.7, 21 -> -11.5, 22 -> -18.7,
  23 -> -12.3, 24 -> -15.3, 25 -> -18.1, 26 -> -21.3, 27 -> -23.6, 28 -> -22.6,
  29 -> -10.9, 30 -> -18.7, 31 -> -17.4, 33 -> -26.2, 34 -> -14.4, 35 -> -19.0,
  36 -> -24.0, 38 -> -17.2, 40 -> -16.0, 41 -> -13.3, 42 -> -21.0, 43 -> -21.4,
  44 -> -14.6, 45 -> -21.7, 46 -> -13.1)

val gainsNew = gainsOld.map { case (oldNum, value) =>
  m(oldNum) -> value
}
