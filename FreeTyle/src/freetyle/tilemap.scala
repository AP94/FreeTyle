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
    //for (ln <- io.Source.stdin.getLines) println(ln)
    // RUNNING in project directory (inner FreeTyle)
    // you MUST supply a .txt file.
    val mapOpt = "src/Map1.txt"
//args(0)
    mapOpt match {
      case mapFile =>{
        val mapCode = io.Source.fromFile(mapFile).mkString
        println(mapCode)
        parser(mapCode) match {
          case parser.Success(ast, _) =>
            semantics.loadAST(ast)
          // If the parser fails, do error handling here
          case parser.Failure(message, _) => println(message)
        }
      }
    }
}