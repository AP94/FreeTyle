package freetyle.parsing

import freetyle.ir._
import scala.util.parsing.combinator._
import scala.collection.mutable

/**
 * @author apinson
 */
object parser extends JavaTokenParsers with PackratParsers{

  def apply(s: String): ParseResult[AST] = {parseAll(file, s)}

  /**
   * File: One or more tile specifications, a map, and one or more generate calls
   */
  lazy val file: PackratParser[AST] = (
      (tile.+)~map~(generates.+) ^^ {case ts~m~gs => new AST(ts, m, gs)}
      )

  /**
   * Tile: Either a basic tile (with an optional edge) or a freeform tile (with an optional anchor)
   */
  lazy val tile: PackratParser[(TileName, Tile)] = (
      "tile"~>tilename~"="~path~edge ^^ {case tname~"="~u~e => (tname, new BaseTile(tname, u, e))}
      | "tile"~>tilename~"="~path ^^ {case tname~"="~u => (tname, new  BaseTile(tname, u, ""))}
      | "freeform"~>"tile"~>tilename~"="~path~anchor ^^ {case tname~"="~p~a => (tname, new FreeTile(tname, p, a))}
      | "freeform"~>"tile"~>tilename~"="~path ^^ {case tname~"="~p => (tname, new FreeTile(tname, p, new Point(0,0)))}
      | failure("Improper tile definition")
      )

  /**
   * Edge: specified with a filepath
   */
  lazy val edge: PackratParser[String] = (
      "{"~>"edge"~>"="~>path~"}" ^^ {case p~"}" => p}
      | failure("A proper edge definition was not supplied")
      )

  /**
   * Anchor: specified with a point
   */
  lazy val anchor: PackratParser[Point] = (
      "{"~>"anchor"~>"="~>point~"}" ^^ {case p~"}" => p}
      | failure("A proper anchor point was not supplied")
      )
  
  /**
   * Map: specified with a width, height, and optional origin, in any order, followed by one or more layers
   */
  lazy val map: PackratParser[Map] = (
       "map"~>"{"~>width~height~origin~(layer.+)~"}" ^^ {case w~h~o~l~"}" => new Map(w,h,o,l)}
      | "map"~>"{"~>height~width~origin~(layer.+)~"}" ^^ {case h~w~o~l~"}" => new Map(w,h,o,l) }
      | "map"~>"{"~>width~origin~height~(layer.+)~"}" ^^ {case w~o~h~l~"}" => new Map(w,h,o,l) }
      | "map"~>"{"~>height~origin~width~(layer.+)~"}" ^^ {case h~o~w~l~"}" => new Map(w,h,o,l) }
      | "map"~>"{"~>origin~width~height~(layer.+)~"}" ^^ {case o~w~h~l~"}" => new Map(w,h,o,l) }
      | "map"~>"{"~>origin~height~width~(layer.+)~"}" ^^ {case o~h~w~l~"}" => new Map(w,h,o,l) }
      | "map"~>"{"~>height~width~(layer.+)~"}" ^^ {case h~w~l~"}" => new Map(w,h,topLeft,l) }
      | "map"~>"{"~>width~height~(layer.+)~"}" ^^ {case w~h~l~"}" => new Map(w,h,topLeft,l) }
      | failure("Map specifications must come before layers and must include a width and a height")
      )

  /**
  * Layer: specified with a number and one or more instructions
  */
  lazy val layer: PackratParser[Layer] = (
      "layer"~>wholeNumber~"{"~(instr.+)~"}" ^^ {case num~"{"~is~"}" => new Layer(num.toInt, is)}
      | failure("Improper layer definition")
      )

  /**
   * Instruction: specifies either a place or fill instruction
   */
  lazy val instr: PackratParser[Instr] = (
      placeAt ^^ {case p => p}
      | fillArea ^^ {case f => f}
      )
      
  /**
   * FillArea: either a rectangle or irregular polygon, specified with a list of points and a tilename
   */
  lazy val fillArea: PackratParser[Area] = (
      "fill"~>"rectangle"~>(point.+)~"with"~tilename ^^ {case points~"with"~tname => new Area(tname, points, true)}
      | "fill"~>"area"~>(point.+)~"with"~tilename ^^ {case points~"with"~tname => new Area(tname, points, false)}
      | failure("Improper specification of fill area")
      ) 
      
  /**
   * PlaceAt: specified with a list of points and a tilename
   */
  lazy val placeAt: PackratParser[PlacePoint] = (
      "at"~>(point.+)~"place"~tilename ^^ {case points~"place"~tname => new PlacePoint(tname, points) }
      | failure("Improper specification of at statement")
      )

  /**
   * Generates: generate calls, either for maps or debug maps, with names to write the files as
   */
  lazy val generates: PackratParser[(MapType, String)] = (
      "generate"~>"map"~>"as"~>filename ^^ {case fname => (basic, fname)}
      | "generate"~>"debug"~>"map"~>"as"~>filename ^^ {case fname => (debug, fname)}
      | failure("Improper generate call")
      ) withFailureMessage("FAIL AT GENERATE")

  /**
   * Width, height, and origin specifications
   */
  lazy val width: PackratParser[Int] = (
      "width"~>"="~>wholeNumber ^^ {case num => num.toInt}
      | failure("Improper width specification (all numbers must be ints)")
      )

  lazy val height: PackratParser[Int] = (
      "height"~>"="~>wholeNumber ^^ {case num => num.toInt}
      | failure("Improper height specification (all numbers must be ints)")
      )

  lazy val origin: PackratParser[Origin] = (
      "origin"~>"="~>originKeyword ^^ {case key => key}
      | failure("Improper origin specification")
      )

  lazy val originKeyword: PackratParser[Origin] = ( //ident ^^^ topLeft
      "bottomLeft" ^^^ bottomLeft
      | "topLeft" ^^^ topLeft
      | failure("Improper origin keyword")
      )   
      
  lazy val fileExtention: PackratParser[String] = (
      ".jpg"  ^^^ ".jpg"
      )
  
  lazy val prepath: Parser[String] = (
      ident~"/" ^^ {case i~"/" => i+"/"}
      )

  /**
   * Point parser, as points appear multiple places
   */
  lazy val point: PackratParser[Point] = (
      "("~>wholeNumber~","~wholeNumber~")" ^^ {case x~","~y~")" => new Point(x.toInt, y.toInt)}
      | failure("Improper point definition")
      )

  /**
   * Other words for ident to give parser code more clarity
   */
  def tilename: Parser[TileName] = ident withFailureMessage("tilename failed to parse")
  def path: Parser[String] = (
      (prepath.+)~ident~fileExtention ^^ {case ps~p~f => unfold(ps)+p+f}
      ) withFailureMessage("path failed to parse")
  def filename: Parser[String] = ident withFailureMessage("filename failed to parse")

  def unfold(l: List[String]): String = {
    var str = ""
    for (s <- l) {
      str = str+s
    }
    return str
  }
  
}