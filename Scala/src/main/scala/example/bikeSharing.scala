package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object BikeSharing
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("season_0", "season_1","season_2", 
                       "mnth_0","mnth_1","mnth_2","mnth_3","mnth_4","mnth_5","mnth_6","mnth_7","mnth_8","mnth_9","mnth_10",
                       "weekday_0", "weekday_1", "weekday_2", "weekday_3","weekday_4", "weekday_5",  
                       "weathersit_0", "weathersit_1",
                       "yr",  
                       "holiday",
                        "temp",  "atemp", "hum", "windspeed", "cnt")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/Bike Sharing.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
    xy = xy.slice(1,xy.dim1)

    val dummySeas = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(0))-1)) //col 0
    val dummyMnth = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(2))-1)) //col 2
    val dummyWkdy = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(4)))) //col 4
    val dummyWeat = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(5))-1)) //col 6

    xy = dummySeas ++^ dummyMnth ++^ dummyWkdy ++^ dummyWeat ++^ xy.selectCols(Array(1,3,6,7,8,9,10))

/** the separation of the combine data matrix 'xy' into
     *  a data/input matrix 'x' and a response/output vector 'y'
     */
    val (x, y) = pullResponse_ (xy)

    /** the combined data matrix 'xy' with a column of all ones prepended
     *  for intercept models
     */
    val oxy = VectorD.one (xy.dim1) +^: xy
    val ox = VectorD.one (xy.dim1) +^: x


} // BikeSharing object



