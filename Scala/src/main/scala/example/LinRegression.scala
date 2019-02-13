package data
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}

object LinRegressAutoMpg extends App
{


// val qrg = new Regression(ExampleAutoMPG.ox,ExampleAutoMPG.y)
// qrg.train().eval()
// println("Auto MPG:")
// println(qrg.report)

val qrgBike = new Regression(BikeSharing.ox, BikeSharing.y)
qrgBike.train().eval()
println("Bike:")
println(qrgBike.report)

val qrgComputer = new Regression(ComputerHardware.ox,ComputerHardware.y)
qrgComputer.train().eval()
println("Computer:")
println(qrgComputer.report)


val qrgElectricGrid = new Regression(ElectricalGrid.ox,ElectricalGrid.y)
qrgElectricGrid.train().eval()
println("Electrical Grid:")
println(qrgElectricGrid.report)


val qrgEnergyEff = new Regression(EnergyEff.ox,EnergyEff.y)
qrgEnergyEff.train().eval()
println("Energy Eff:")
println(qrgEnergyEff.report)


val qrgForestFires = new Regression(ForestFires.ox,ForestFires.y)
qrgForestFires.train().eval()
println("ForestFires:")
println(qrgForestFires.report)

val qrgNaval = new Regression(Naval.ox,Naval.y)
qrgNaval.train().eval()
println("Naval:")
println(qrgNaval.report)


val qrgoptical = new Regression(optical.ox,optical.y)
qrgoptical.train().eval()
println("optical:")
println(qrgoptical.report)

val qrgProteinTertiary = new Regression(ProteinTertiary.ox,ProteinTertiary.y)
qrgProteinTertiary.train().eval()
println("ProteinTertiary:")
println(qrgProteinTertiary.report)

// val qrgWineQuality = new Regression(WineQuality.ox,WineQuality.y)
// qrgWineQuality.train().eval()
// println("WineQuality:")
// println(qrgWineQuality.report)



}
