package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object ElectricalGrid
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("tau1", "tau2", "tau3", "tau4", "p1", "p2",
                        "p3", "p4", "g1", "g2", "g3", "g4", "stab")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/Electrical Grid Stability.csv"
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


} // Naval object


