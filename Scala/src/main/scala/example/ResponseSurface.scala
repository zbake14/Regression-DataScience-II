package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._

object ResponseSurface extends App
{
	val auto = ExampleAutoMPG
	val autoX = auto.x
	val autoY = auto.y


	val rsrg = new QuadRegression(autoX,autoY)
	rsrg.train().eval()
	println("Auto MPG:")
	println(rsrg.report)

 	val rsrgBike = new Regression(BikeSharing.ox, BikeSharing.y)
 	rsrgBike.train().eval()
 	println("Bike:")
 	println(rsrgBike.report)


}
