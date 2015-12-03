package freetyle

import java.io.File
import scala.util.parsing.combinator._
import scala.io.Source

import freetyle.parsing.parser
import freetyle.semantics._ 


/**
 * @author apinson
 */
object tilemap extends App{
    for (ln <- io.Source.stdin.getLines) {
    // RUNNING in project directory (inner FreeTyle)
    // you MUST save the map file in the src/ directory
    val mapOpt = "src/"+ln+".txt"
    mapOpt match {
      case mapFile => {
        val mapCode = io.Source.fromFile(mapFile).mkString
        parser(mapCode) match {
          case parser.Success(ast, _) =>
            semantics.loadAST(ast)
          // If the parser fails, do error handling here
          case parser.Failure(message, _) => println(message)
        }
      }
    }
  }
}