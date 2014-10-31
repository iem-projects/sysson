import de.sciss.synth.swing.Plotting.Implicits._
val p1 = "/home/hhrutz/CBE_Data/radiation/avg_hfls_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc"
val f1 = open(p1)
val d1 = f1.variableMap("hfls").read().float1D
// d1.plot()
val p2 = "/home/hhrutz/CBE_Data/radiation/avg_hfls_Amon_MPI-ESM-LR_rcp45_r1i1p1_200601-230012.nc"
val f2 = open(p2)
val d2 = f2.variableMap("hfls").read().float1D
// d2.plot()
val p3 = "/home/hhrutz/Documents/temp/concat_test5.nc"
val f3 = open(p3)
val d3 = f3.variableMap("hfls").read().float1D
// d3.plot()

assert(d1 ++ d2 == d3)