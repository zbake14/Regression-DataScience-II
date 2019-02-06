package example
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import regression._
import data._
// import example.Ridge
// import example.WLS

object MainRegression extends App
{
println("main is running")
// val lass = new Lasso()
println(BikeSharing.dummySeas.dim2)
println(BikeSharing.dummyMnth.dim2)
println(BikeSharing.dummyWkdy.dim2)
println(BikeSharing.dummyWeat.dim2)
//println(BikeSharing.x.selectCols(Array(0)).map(_.toInt))
//println(System.getProperty("user.dir")+"/Raw Data/Computer Hardware.csv")
///Users/zachbaker/Documents/Senior Year/Spring Semester/DS2/Regression-DataScience-II/AllRegressions/Scala/RawData/Bike Sharing.csv
}
