package freetyle.parsing

import freetyle.ir._
import scala.util.parsing.combinator._

/**
 * @author apinson
 */
object parser extends JavaTokenParsers with PackratParsers{
  
  lazy val 
  
  lazy val map: PackratParser[Layer] = (
      rword("map ")~mapName~rword(" {")~rword("width = ")~>width~rword("height = ")~height~
        rword("origin = ")~origin~layer.+ ^^ { case _~n~_~w~_~_~h~_~o~ls => new Map(w,h,o,ls,n)}
  | failure("Map failed to parse. Check that a name is provided outside map declaration and that width, height, and origin are specified before layers")    
  )
  
  
      
}