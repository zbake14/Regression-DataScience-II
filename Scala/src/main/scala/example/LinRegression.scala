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
  //Calling ForwardSelection method for each dataset.
  //ForwardSelection method takes Matrix X (data) and Vector Y (variable)
  //ForwardSelection applies forward selection and crossvalidation along with the plots


  println("WineQuality")
  ForwardSelection(WineQuality.ox,WineQuality.y, "WineQuality")
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("ProteinTertiary")
  ForwardSelection(ProteinTertiary.ox,ProteinTertiary.y,"ProteinTertiary") //run time error solved
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("EnergyEff")
  ForwardSelection(EnergyEff.ox,EnergyEff.y,"EnergyEff") //runtime error solved
  println("_____________________________________________________")
  println("_____________________________________________________")


  println("ElectricalGrid")
  ForwardSelection(ElectricalGrid.ox,ElectricalGrid.y,"ElectricalGrid")
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("ComputerHardware")
  ForwardSelection(ComputerHardware.ox,ComputerHardware.y,"ComputerHardware") //runtime error solved
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("BikeSharing")
  ForwardSelection(BikeSharing.ox,BikeSharing.y,"BikeSharing") //runtime solved
  println("_____________________________________________________")
  println("_____________________________________________________")

  //ExampleAutoMPG did not have a defintion of ox. Hence added one column manually.
  println("ExampleAutoMPG")
  ForwardSelection(VectorD.one(ExampleAutoMPG.xy.dim1) +^: ExampleAutoMPG.x,ExampleAutoMPG.y,"ExampleAutoMPG")
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("Optical")
  ForwardSelection(optical.ox,optical.y,"Optical") //runtime solved
  println("_____________________________________________________")
  println("_____________________________________________________")

  println("ConcreteData")
  ForwardSelection(ConcreteData.ox,ConcreteData.y,"ConcreteData")

  println("_____________________________________________________")
  println("_____________________________________________________")

  println("ForestFires")
  ForwardSelection(ForestFires.ox,ForestFires.y, "ForestFires")
  println("_____________________________________________________")
  println("_____________________________________________________")


  def ForwardSelection(argX: MatrixD, argY: VectorD, datasetName:String): Unit = {

    //Create object of Regression and train and evaluate the model.

    val rrg = new Regression(argX, argY)
    rrg.train().eval()
    val x = argX
    val y = argY

  //Initialize variables for performance evaluation.
    var fcols = Set[Int]() // start with no variable
    var r2 = new VectorD(x.dim2) //R-Square matrix
    var r2A = new VectorD(x.dim2) //Adjusted R-Square matrix
    var cvR = new VectorD(x.dim2) //cross validation matrix
    var flag = true
    var tcol = 0 //length of fcols. Essentially the number of columns we end up using.

    //add a column iteratively

    for (l <- 0 until x.dim2) {
      if (flag) {
        val (x_j, b_j, fit_j) = rrg.forwardSel(fcols) // add most predictive variable

        //break the loop if indices are negative to avoid run time errors.
        if (fit_j(7) < 0 || fit_j(0)<0) flag = false
        if(l<3) flag = true

        if(flag)
        {
          //Call cross validation method with new fcols and get R-square and adjusted R square values.

          fcols += x_j
          cvR(l) = crossVal((x: MatriD, y: VectoD) => new Regression(x,y), new MatrixD(x.selectCols(fcols.toArray)), argY)
          r2(l) = fit_j(0)
          r2A(l) = fit_j(7)
          tcol = tcol + 1
        }

      }
    } // for

//Print the results.
    println("max r2 is:")
    println(r2.slice(0,tcol).max())
    println("max r2A is:")
    println(r2A.slice(0,tcol).max())
    println("n* for adj r2: "+(r2A.argmax()+1))
    println("max cv R2 is:")
    println(cvR.slice(0,tcol).max())
    println("n* for cv r2: " +(cvR.slice(0,tcol).argmax()+1))

    val t = VectorD.range(1, tcol)
    val all3 = new MatrixD(3,tcol) //Get all evaluation metrics into single matrix.
    all3.update(0,r2.slice(0, tcol))
    all3.update(1,r2A.slice(0, tcol))
    all3.update(2,cvR.slice(0, tcol))

  //Plot the graphs.
    new PlotM(t,
             all3*100,
             Array("R2","R2 Adj", "CV R2"),
             datasetName+" R square vs R bar square", true)
  }

//Cross validation method
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

  //calculate errors, sse, sst and rsquare.
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
