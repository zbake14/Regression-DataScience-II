package example
import scalation.linalgebra._
import scalation.analytics._
import scalation.stat.StatVector.corr
import scalation.analytics.classifier._
import scalation.util.{getFromURL_File, time}

object quadRegressAutoMpg
{

val auto = ExampleAutoMPG
val autoX = auto.x
val autoY = auto.y

val fname = System.getProperty("user.dir")+"/BikeDataset/day.csv"




println(fname)

                                                // 10 for x, 1 for y
       // val xy = MatrixD(fname)
       //
       // val v = xy.sliceCol(2,16)
       //
       // val X = v.sliceCol(0,13)
       // val Y = v.col(13)
       // println(X)
       //
       //  val rg = new Regression(X,Y)
       //  rg.train().eval()
       // println(rg.report)

        val rg1 = new Regression(autoX,autoY)
        rg1.train().eval()
       println(rg1.report)

}
