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
    val fname = Array ("vendor_0",  "vendor_1", "vendor_2", "vendor_3", "vendor_4", "vendor_5", "vendor_6", "vendor_7", "vendor_8", "vendor_9", "vendor_10",    "vendor_11",    "vendor_12",    "vendor_13",    "vendor_14",    "vendor_15",    "vendor_16",    "vendor_17",    "vendor_18",    "vendor_19",    "vendor_20",    "vendor_21",    "vendor_22",    "vendor_23",    "vendor_24",    "vendor_25",    "vendor_26",    "vendor_27",    "vendor_28",
                       "MYCT","MMIN",
                       "MMAX","CACH","CHMIN","CHMAX","PRP","ERP")

    /** the combined data matrix 'xy'
     */
    val fname1 = System.getProperty("user.dir")+"/RawData/Computer Hardware.csv"
    var xy = MatrixD(fname1)

    /** index for the data points (instances)
     */
    val t = VectorD.range (0, xy.dim1)

    /** get rid of header column
    */
    xy = xy.slice(1,xy.dim1)

    val dummyVen = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(0))-1)) //col 0
    // val dummyMnth = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(2))-1)) //col 2
    // val dummyWkdy = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(4)))) //col 4
    // val dummyWeat = new MatrixD(ANCOVA.dummyVars(xy.toInt.selectCols(Array(6))-1)) //col 6

    xy = dummyVen ++^ xy.selectCols(Array(1,2,3,4,5,6,7,8))

/** the separation of the combine data matrix 'xy' into
     *  a data/input matrix 'x' and a response/output vector 'y'
     */
    val (x, y) = pullResponse_ (xy)

    /** the combined data matrix 'xy' with a column of all ones prepended
     *  for intercept models
     */
    val oxy = VectorD.one (xy.dim1) +^: xy
    val ox = VectorD.one (xy.dim1) +^: x


} // Computer Hardware object
