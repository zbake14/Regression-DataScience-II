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

  ForwardSelection(WineQuality.x,WineQuality.y)
  ForwardSelection(ProteinTertiary.x,ProteinTertiary.y) //run time error solved
  ForwardSelection(EnergyEff.x,EnergyEff.y) //runtime error solved
  ForwardSelection(ForestFires.x,ForestFires.y)
  ForwardSelection(ElectricalGrid.x,ElectricalGrid.y)
  ForwardSelection(ComputerHardware.x,ComputerHardware.y) //runtime error solved
  ForwardSelection(BikeSharing.x,BikeSharing.y) //runtime solved
  ForwardSelection(ExampleAutoMPG.x,ExampleAutoMPG.y)
  ForwardSelection(optical.x,optical.y) //runtime solved
  ForwardSelection(ConcreteData.x,ConcreteData.y)

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

        if (fit_j(7) < 0 || fit_j(0)<0) flag = false

        if(flag)
        {
          fcols += x_j
          cvR(l) = crossVal((x: MatriD, y: VectoD) => new Regression(x,y), new MatrixD(x.selectCols(fcols.toArray)), argY)
          r2(l) = fit_j(0)
          r2A(l) = fit_j(7)
          tcol = tcol + 1
        }

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
