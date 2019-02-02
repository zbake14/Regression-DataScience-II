package example
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr

object quadRegressAutoMpg extends App
{

val auto = ExampleAutoMPG
val X = auto.x
val Y = auto.y


val rg = new QuadRegression(X,Y)
rg.train().eval()
println(rg.report)
}
