package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._
import scalation.plot.PlotM
import scala.collection.mutable.Set
import scalation.util.banner
import scalation.stat.Statistic
import scalation.random.CDF.studentTCDF
import scalation.random._
import scalation.math.double_exp

object ResponseSurface extends App
{
	val auto = ExampleAutoMPG
	val autoX = auto.x
	val autoY = auto.y


	// val rs = new ResponseSurface(autoX,autoY)
	// rs.train().eval()
	// println("Auto MPG:")
	// println(rs.report)

	// val lrgBike = new LassoRegression(BikeSharing.ox/*.selectCols(Array(1,20,21,22,23,24,25,26,27,28))*/, BikeSharing.y)
	// lrgBike.train().eval()
	// println("Bike:")
	// println(lrgBike.report)

	// val lrgComputer = new LassoRegression(ComputerHardware.ox,ComputerHardware.y)
	// lrgComputer.train().eval()
	// println("Computer:")
	// println(lrgComputer.report)

	// val rsElectricGrid = new LassoRegression(ElectricalGrid.ox,ElectricalGrid.y)
	// rsElectricGrid.train().eval()
	// println("Electrical Grid:")
	// println(rsElectricGrid.report)

	// val rsEnergyEff = new LassoRegression(EnergyEff.ox,EnergyEff.y)
	// rsEnergyEff.train().eval()
	// println("Energy Eff:")
	// println(rsEnergyEff.report)

	// val rsForestFires = new LassoRegression(ForestFires.ox,ForestFires.y)
	// rsForestFires.train().eval()
	// println("ForestFires:")
	// println(rsForestFires.report)

	//val rsOptical = new LassoRegression(optical.ox,optical.y)
	//rsOptical.train().eval()
	//println("Optical:")
	//println(rsOptical.report)


	ForwardSelection(WineQuality.x,
                 WineQuality.y)


  def ForwardSelection(argX: MatrixD, argY: VectorD): Unit = {

    val rrg = new ResponseSurface(argX, argY)
    rrg.train().eval()
    val x = rrg.getX
    val y = argY

    var fcols = Set[Int]() // start with no variable
    var r2 = new VectorD(x.dim2)
    var r2A = new VectorD(x.dim2)
    var cvR = new VectorD(x.dim2)
    var flag = true
    var tcol = 0

    for (l <- 0 until x.dim2) {
      if (flag) {
        val (x_j, b_j, fit_j) = rrg.forwardSel(fcols) // add most predictive variable
        fcols += x_j
        cvR(l) = crossVal((x: MatriD, y: VectoD) => new Regression(x,y), new MatrixD(x.selectCols(fcols.toArray)), argY)
        r2(l) = fit_j(0)
        r2A(l) = fit_j(7)
        tcol = tcol + 1
        if (fit_j(7) < 0) flag = false
      }
    } // for

    println("max r2 is:")
    println(r2.max())
    println("max r2A is:")
    println(r2A.max())
    println("n* for adj r2: "+(r2A.argmax()+1))
    println("max cv R2 is:")
    println(cvR.max())
    println("n* for cv r2: " +(cvR.argmax()+1))
    println(r2)
    println(r2A)
    println(cvR)
    val t = VectorD.range(0, tcol)
    val all3 = new MatrixD(3,tcol)
    all3.update(0,r2.slice(0, tcol))
    all3.update(1,r2A.slice(0, tcol))
    all3.update(2,cvR.slice(0, tcol))
    new PlotM(t,
             all3,
             Array("R2","R2 Adj", "CV R2"),
             "R square vs R bar square", true)
  }

  def crossVal(algor: (MatriD, VectoD) => PredictorMat,
               argX: MatrixD, argY: VectorD, k: Int = 10): Double = {
    //val DEBUG = false
    var sumR = 0.0
    /*val fLabel = algor(new MatrixD(1,1), new VectorD(1)).fitLabel // labels for qof measures
    val stats = Array.ofDim[Statistic](fLabel.length)
    for (i <- stats.indices) stats(i) = new Statistic(fLabel(i))*/
    val indices = PermutedVecI (VectorI.range (0, argX.dim1), 0).igen.split(k) // k groups of indices
    for (idx <- indices) {
      val x_te = argX(idx) // test data matrix
      val y_te = argY(idx) // test response vector
      val x_tr = argX.selectRowsEx(idx) // training data matrix
      val y_tr = argY.selectEx(idx) // training response vector

      val model = algor(x_tr, y_tr) // construct next model using training dataset
      model.train() // train model on the training dataset

      val e = y_te - x_te*model.parameter
      val sse = e dot e
      val sst = (y_te dot y_te) - y_te.sum~^2 / y_te.dim
      val ssr = sst-sse
      val rSq = ssr/sst
      sumR = sumR + rSq
      /*model.eval(x_te, y_te) // evaluate model on the test dataset
      val qof = model.fit // get quality of fit (qof) measures
      println(model.report)
      if (DEBUG) println(s"crossValidate: qof = $qof")
      for (q <- qof.range) stats(q).tally(qof(q)) // tally these qof measures
      */
    } // for

/*
    if (DEBUG) {
      banner("crossValidate: Statistical Table for QoF")
      println(Statistic.labels)
      for (i <- stats.indices) println(stats(i))
    } // if*/
    sumR/k
  }

}
