package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object EnergyEff
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("X1", "X2","X3",
                       "X4","X5","X6","X7","X8","Y1")
     //Is VendorName categorical??? Do we even need it???

    val fname1 = System.getProperty("user.dir")+"/RawData/Energy Effeciency.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
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

    // println("x is:")
    // println(x)
    //
    //
    // println("y is:")
    // println(y)
    //
    // println("ox is:")
    // println(ox)
    //
    //
    // println("oxy is:")
    // println(oxy)

} // BikeSharing object
