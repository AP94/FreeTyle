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
    for (ln <- io.Source.stdin.getLines) println(ln)
    val mapOpt = args(0)
    mapOpt match {
      case mapFile =>{
        val mapCode = io.Source.fromFile(mapFile).mkString
        parser(mapCode) match {
          case parser.Success(ast, _) =>
            semantics.loadAST(ast)
          // If the parser fails, do error handling here
          case _ => null
        }
      }
    }
}