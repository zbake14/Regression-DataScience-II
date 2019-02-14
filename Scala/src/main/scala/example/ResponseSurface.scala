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


	val rsrg = new ResponseSurface(autoX,autoY)
	rsrg.train().eval()
	println("Auto MPG:")
	println(rsrg.report)

	val rsrgBike = new ResponseSurface(BikeSharing.ox,BikeSharing.y)
	rsrgBike.train().eval()
	println("Bike:")
	println(rsrgBike.report)

	val rsrgComputer = new ResponseSurface(ComputerHardware.ox,ComputerHardware.y)
	rsrgComputer.train().eval()
	println("Computer:")
	println(rsrgComputer.report)


}
