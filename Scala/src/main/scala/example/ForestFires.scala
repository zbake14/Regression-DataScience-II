package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object ForestFires
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("X-space","Y-space",
                       "mnth_0","mnth_1","mnth_2","mnth_3","mnth_4","mnth_5","mnth_6","mnth_7","mnth_8","mnth_9","mnth_10",
                       "weekday_0", "weekday_1", "weekday_2", "weekday_3","weekday_4", "weekday_5",  
                       "FFMC",  "DMC",  "DC",   "ISI",  "temp", "RH",   "wind", "rain", "area")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/forestfires.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
    xy = xy.slice(1,xy.dim1)

    val dummyMnth = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(2))-1)) //col 2
    val dummyWkdy = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(3))-1)) //col 3

    xy = dummyMnth ++^ dummyWkdy ++^ xy.selectCols(Array(0,1,4,5,6,7,8,9,10,11,12))

/** the separation of the combine data matrix 'xy' into
     *  a data/input matrix 'x' and a response/output vector 'y'
     */
    val (x, y) = pullResponse_ (xy)

    /** the combined data matrix 'xy' with a column of all ones prepended
     *  for intercept models
     */
    val oxy = VectorD.one (xy.dim1) +^: xy
    val ox = VectorD.one (xy.dim1) +^: x


} // ForestFires object



