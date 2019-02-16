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

object LinRegress extends App
{
/*
 val qrg = new Regression(VectorD.one (ExampleAutoMPG.xy.dim1) +^: ExampleAutoMPG.x,ExampleAutoMPG.y)
 qrg.train().eval()
 println("Auto MPG:")
 println(qrg.report)

 val qrgBike = new Regression(BikeSharing.ox, BikeSharing.y)
 qrgBike.train().eval()
 println("Bike:")
 println(qrgBike.report)

 val qrgComputer = new Regression(ComputerHardware.ox,ComputerHardware.y)
 qrgComputer.train().eval()
 println("Computer:")
 println(qrgComputer.report)

 val qrgElectricGrid = new Regression(ElectricalGrid.ox,ElectricalGrid.y)
 qrgElectricGrid.train().eval()
 println("Electrical Grid:")
 println(qrgElectricGrid.report)

 val qrgEnergyEff = new Regression(EnergyEff.ox,EnergyEff.y)
 qrgEnergyEff.train().eval()
 println("Energy Eff:")
 println(qrgEnergyEff.report)


 val qrgForestFires = new Regression(ForestFires.ox,ForestFires.y)
 qrgForestFires.train().eval()
 println("ForestFires:")
 println(qrgForestFires.report)


 val qrgoptical = new Regression(optical.ox,optical.y)
 qrgoptical.train().eval()
 println("optical:")
 println(qrgoptical.report)

 val qrgProteinTertiary = new Regression(ProteinTertiary.ox,ProteinTertiary.y)
 qrgProteinTertiary.train().eval()
 println("ProteinTertiary:")
 println(qrgProteinTertiary.report)

 val qrgWineQuality = new Regression(WineQuality.ox,WineQuality.y)
 qrgWineQuality.train().eval()
 println("WineQuality:")
 println(qrgWineQuality.report)

val qrgCon= new Regression(ConcreteData.ox,ConcreteData.y)
qrgCon.train().eval()
println("Concrete_Data:")
println(qrgCon.report)
*/
ForwardSelection(WineQuality.ox,
                 WineQuality.y)


  def ForwardSelection(argX: MatrixD, argY: VectorD): Unit = {

    val rrg = new Regression(argX, argY)
    rrg.train().eval()
    val x = argX
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
        cvR(l) = crossVal((x: MatriD, y: VectoD) => new Regression(x,y), x.selectCols(fcols.toArray), argY)
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
