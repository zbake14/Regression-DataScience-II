package regression
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}
import data._
import scalation.plot.Plot
import scala.collection.mutable.Set

object quadRegression extends App
{

ForwardSelection(ForestFires.x,ForestFires.y)
// ForwardSelection(ComputerHardware.x,ComputerHardware.y)
// ForwardSelection(ProteinTertiary.x,ProteinTertiary.y)


  def ForwardSelection(argX:MatrixD, argY:VectorD) : Unit = {

      val rrg = new QuadRegression(argX, argY)
    	rrg.train().eval()
    	val x = rrg.getX
    	val y = ForestFires.y
    	println("ForestFires:")

	    var fcols = Set [Int] ()                                // start with no variable
	    var r2 = new VectorD(x.dim2)
	    var r2A = new VectorD(x.dim2)
      var flag = true
      var tcol = 0
	    for (l <- 0 until x.dim2) {
          if(flag)
          {
  	        val (x_j, b_j, fit_j) = rrg.forwardSel (fcols)        // add most predictive variable
  	        fcols += x_j
  	        r2(l) = fit_j(0)
  	        r2A(l) = fit_j(7)
            tcol = tcol+1
            if(fit_j(7)<0) flag = false
          }
	    } // for

      	println("max r2 is:")
      	println(r2A.max())
      	println("max r2A is:")
      	println(r2A.max())
      	println(r2A.argmax())
      	val t = VectorD.range (0, tcol)
      	new Plot(t,r2.slice(0,tcol),r2A.slice(0,tcol),"R square vs R bar square")
    }




// val auto = ExampleAutoMPG
// val autoX = auto.x
// val autoY = auto.y
//
//
// val qrg = new QuadRegression(autoX,autoY)
// qrg.train().eval()
// println("Auto MPG:")
// println(qrg.report)
//
//
// val bike = BikeSharing
// val bikeX = bike.x
// val bikeY = bike.y
//
// val qrgBike = new QuadRegression(bikeX,bikeY)
// qrgBike.train().eval()
// println("Bike:")
// println(qrgBike.report)
//
// val qrgComputer = new QuadRegression(ComputerHardware.x,ComputerHardware.y)
// qrgComputer.train().eval()
// println("Computer:")
// println(qrgComputer.report)
//
//
// val qrgElectricGrid = new QuadRegression(ElectricalGrid.x,ElectricalGrid.y)
// qrgElectricGrid.train().eval()
// println("Electrical Grid:")
// println(qrgElectricGrid.report)
//
//
// val qrgEnergyEff = new QuadRegression(EnergyEff.x,EnergyEff.y)
// qrgEnergyEff.train().eval()
// println("Energy Eff:")
// println(qrgEnergyEff.report)
//
//
// val qrgForestFires = new QuadRegression(ForestFires.x,ForestFires.y)
// qrgForestFires.train().eval()
// println("ForestFires:")
// println(qrgForestFires.report)
//
//
//
// val qrgoptical = new QuadRegression(optical.x,optical.y)
// qrgoptical.train().eval()
// println("optical:")
// println(qrgoptical.report)
//
// val qrgProteinTertiary = new QuadRegression(ProteinTertiary.x,ProteinTertiary.y)
// qrgProteinTertiary.train().eval()
// println("ProteinTertiary:")
// println(qrgProteinTertiary.report)
//
// val qrgWineQuality = new QuadRegression(WineQuality.x,WineQuality.y)
// qrgWineQuality.train().eval()
// println("WineQuality:")
// println(qrgWineQuality.report)



}
