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


	// val rs = new ResponseSurface(autoX,autoY)
	// rs.train().eval()
	// println("Auto MPG:")
	// println(rs.report)

	// val lrgBike = new LassoRegression(BikeSharing.ox/*.selectCols(Array(1,20,21,22,23,24,25,26,27,28))*/, BikeSharing.y)
	// lrgBike.train().eval()
	// println("Bike:")
	// println(lrgBike.report)

	// val lrgComputer = new LassoRegression(ComputerHardware.ox,ComputerHardware.y)
	// lrgComputer.train().eval()
	// println("Computer:")
	// println(lrgComputer.report)

	// val rsElectricGrid = new LassoRegression(ElectricalGrid.ox,ElectricalGrid.y)
	// rsElectricGrid.train().eval()
	// println("Electrical Grid:")
	// println(rsElectricGrid.report)

	// val rsEnergyEff = new LassoRegression(EnergyEff.ox,EnergyEff.y)
	// rsEnergyEff.train().eval()
	// println("Energy Eff:")
	// println(rsEnergyEff.report)

	// val rsForestFires = new LassoRegression(ForestFires.ox,ForestFires.y)
	// rsForestFires.train().eval()
	// println("ForestFires:")
	// println(rsForestFires.report)

	val rsOptical = new LassoRegression(optical.ox,optical.y)
	rsOptical.train().eval()
	println("Optical:")
	println(rsOptical.report)


}
