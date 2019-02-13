package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._

object quadRegression extends App
{

val auto = ExampleAutoMPG
val autoX = auto.x
val autoY = auto.y


val qrg = new QuadRegression(autoX,autoY)
qrg.train().eval()
println("Auto MPG:")
println(qrg.report)


val bike = BikeSharing
val bikeX = bike.x
val bikeY = bike.y

val qrgBike = new QuadRegression(bikeX,bikeY)
qrgBike.train().eval()
println("Bike:")
println(qrgBike.report)

val qrgComputer = new QuadRegression(ComputerHardware.x,ComputerHardware.y)
qrgComputer.train().eval()
println("Computer:")
println(qrgComputer.report)


val qrgElectricGrid = new QuadRegression(ElectricalGrid.x,ElectricalGrid.y)
qrgElectricGrid.train().eval()
println("Electrical Grid:")
println(qrgElectricGrid.report)


val qrgEnergyEff = new QuadRegression(EnergyEff.x,EnergyEff.y)
qrgEnergyEff.train().eval()
println("Energy Eff:")
println(qrgEnergyEff.report)


val qrgForestFires = new QuadRegression(ForestFires.x,ForestFires.y)
qrgForestFires.train().eval()
println("ForestFires:")
println(qrgForestFires.report)

val qrgNaval = new QuadRegression(Naval.x,Naval.y)
qrgNaval.train().eval()
println("Naval:")
println(qrgNaval.report)


val qrgoptical = new QuadRegression(optical.x,optical.y)
qrgoptical.train().eval()
println("optical:")
println(qrgoptical.report)

val qrgProteinTertiary = new QuadRegression(ProteinTertiary.x,ProteinTertiary.y)
qrgProteinTertiary.train().eval()
println("ProteinTertiary:")
println(qrgProteinTertiary.report)

// val qrgWineQuality = new QuadRegression(WineQuality.x,WineQuality.y)
// qrgWineQuality.train().eval()
// println("WineQuality:")
// println(qrgWineQuality.report)



}
