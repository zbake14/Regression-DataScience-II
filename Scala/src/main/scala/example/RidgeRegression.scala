package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._

object RidgeRegression extends App
{

val rrg = new RidgeRegression(ExampleAutoMPG.x-ExampleAutoMPG.x.mean, ExampleAutoMPG.y- ExampleAutoMPG.y.mean)
rrg.train().eval()
println("Auto MPG:")
println(rrg.report)

val rrgBike = new RidgeRegression(BikeSharing.x- BikeSharing.x.mean, BikeSharing.y- BikeSharing.y.mean)
rrgBike.train().eval()
println("Bike:")
println(rrgBike.report)

val rrgComputer = new RidgeRegression(ComputerHardware.x- ComputerHardware.x.mean,ComputerHardware.y-ComputerHardware.y.mean)
rrgComputer.train().eval()
println("Computer:")
println(rrgComputer.report)


val rrgElectricGrid = new RidgeRegression(ElectricalGrid.x- ElectricalGrid.x.mean,ElectricalGrid.y-ElectricalGrid.y.mean)
rrgElectricGrid.train().eval()
println("Electrical Grid:")
println(rrgElectricGrid.report)


val rrgEnergyEff = new RidgeRegression(EnergyEff.x- EnergyEff.x.mean,EnergyEff.y-EnergyEff.y.mean)
rrgEnergyEff.train().eval()
println("Energy Eff:")
println(rrgEnergyEff.report)


val rrgForestFires = new RidgeRegression(ForestFires.x- ForestFires.x.mean,ForestFires.y-ForestFires.y.mean)
rrgForestFires.train().eval()
println("ForestFires:")
println(rrgForestFires.report)


val rrgOptical = new RidgeRegression(optical.x- optical.x.mean,optical.y-optical.y.mean)
rrgOptical.train().eval()
println("Optical:")
println(rrgOptical.report)

val rrgProteinTertiary = new RidgeRegression(ProteinTertiary.x- ProteinTertiary.x.mean,ProteinTertiary.y-ProteinTertiary.y.mean)
rrgProteinTertiary.train().eval()
println("ProteinTertiary:")
println(rrgProteinTertiary.report)

val rrgWineQuality = new RidgeRegression(WineQuality.x- WineQuality.x.mean,WineQuality.y-WineQuality.y.mean)
rrgWineQuality.train().eval()
println("WineQuality:")
println(rrgWineQuality.report)



}
