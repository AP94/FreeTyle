package freetyle

import java.io.File
import scala.util.parsing.combinator._
import freetyle.parsing.parser
import freetyle.semantics._


/**
 * @author apinson
 */
object tilemap {
  val mapOpt = parameters.named get "map"
  mapOpt match {
    case (Some(mapFile)) =>
      val mapCode = io.Source.fromFile(mapFile).mkString
      parser(mapCode) match {
        case parser.Success(ast, _) =>
          loadAST(ast)
      }
  }
}