package data

import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector._
import scalation.plot.Plot
import scalation.util.banner

import PredictorMat.{analyze, pullResponse_}

object ComputerHardware
{
    /** the names of the predictor variables and the response variable at the end
     */
    val fname = Array ("vendor", "MYCT","MMIN",
                       "MMAX","CACH","CHMIN","CHMAX","PRP","ERP")

    /** the combined data matrix 'xy'
     */



     //Is VendorName categorical??? Do we even need it???


    val fname1 = System.getProperty("user.dir")+"/RawData/Computer Hardware.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
    xy = xy.slice(1,xy.dim1)

    // val dummySeas = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(0))-1)) //col 0
    // val dummyMnth = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(2))-1)) //col 2
    // val dummyWkdy = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(4)))) //col 4
    // val dummyWeat = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(6))-1)) //col 6

    // xy = dummySeas ++^ dummyMnth ++^ dummyWkdy ++^ dummyWeat ++^ xy.selectCols(Array(1,3,5,7,8,9,10,11))

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
