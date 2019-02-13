package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object ConcreteData extends App
{
    /** the names of the predictor variables and the response variable at the end
     */
    // val fname = Array ("LP","v","GTT","GTn","GGN","Ts","Tp","HP","T1","T2",
    //                     "P48","P1","P2","Pexh","TIC","mf","GTCdec")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/Concrete_Data.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
       // println("t calculating")
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
       // println("xy calculating")
    xy = xy.slice(1,xy.dim1)

/** the separation of the combine data matrix 'xy' into
     *  a data/input matrix 'x' and a response/output vector 'y'
     */

    val (x, y) = pullResponse_ (xy)

    /** the combined data matrix 'xy' with a column of all ones prepended
     *  for intercept models
     */
    val oxy = VectorD.one (xy.dim1) +^: xy
    val ox = VectorD.one (xy.dim1) +^: x

  
    //
    // val qrgCon= new Regression(Concrete_Data.ox,Concrete_Data.y)
    // qrgCon.train().eval()
    // println("Concrete_Data:")
    // println(qrgCon.report)

} // Naval object
