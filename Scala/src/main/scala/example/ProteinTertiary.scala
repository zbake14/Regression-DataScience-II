package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object ProteinTertiary
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("F1", "F2", "F3", "F4", "F5", "F6",
                        "F7", "F8", "F9", "RMSD")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/Protein Tertiary.csv"
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


    println("oxy is:")
    println(oxy)

} // Naval object
