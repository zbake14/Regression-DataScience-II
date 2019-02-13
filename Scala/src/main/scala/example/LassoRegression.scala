package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._

object LassoRegression extends App
{

val lrg = new LassoRegression(VectorD.one (ExampleAutoMPG.xy.dim1) +^: ExampleAutoMPG.x, ExampleAutoMPG.y)
lrg.train().eval()
println("Auto MPG:")
println(lrg.report)

val lrgBike = new LassoRegression(BikeSharing.ox/*.selectCols(Array(1,20,21,22,23,24,25,26,27,28))*/, BikeSharing.y)
lrgBike.train().eval()
println("Bike:")
println(lrgBike.report)

val lrgComputer = new LassoRegression(ComputerHardware.ox,ComputerHardware.y)
lrgComputer.train().eval()
println("Computer:")
println(lrgComputer.report)


val lrgElectricGrid = new LassoRegression(ElectricalGrid.ox,ElectricalGrid.y)
lrgElectricGrid.train().eval()
println("Electrical Grid:")
println(lrgElectricGrid.report)


val lrgEnergyEff = new LassoRegression(EnergyEff.ox,EnergyEff.y)
lrgEnergyEff.train().eval()
println("Energy Eff:")
println(lrgEnergyEff.report)


val lrgForestFires = new LassoRegression(ForestFires.ox,ForestFires.y)
lrgForestFires.train().eval()
println("ForestFires:")
println(lrgForestFires.report)

val lrgNaval = new LassoRegression(Naval.ox,Naval.y)
lrgNaval.train().eval()
println("Naval:")
println(lrgNaval.report)


val lrgOptical = new LassoRegression(optical.ox,optical.y)
lrgOptical.train().eval()
println("Optical:")
println(lrgOptical.report)

val lrgProteinTertiary = new LassoRegression(ProteinTertiary.ox,ProteinTertiary.y)
lrgProteinTertiary.train().eval()
println("ProteinTertiary:")
println(lrgProteinTertiary.report)

val lrgWineQuality = new LassoRegression(WineQuality.ox,WineQuality.y)
lrgWineQuality.train().eval()
println("WineQuality:")
println(lrgWineQuality.report)



}
