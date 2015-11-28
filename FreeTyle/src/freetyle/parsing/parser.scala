package freetyle.parsing

import freetyle.ir._
import scala.util.parsing.combinator._
import scala.collection.mutable

/**
 * @author apinson
 */
object parser extends JavaTokenParsers with PackratParsers{

  def apply(s: String): ParseResult[AST] = parseAll(file, s)

  /**
   * File: One or more tile specifications, a map, and one or more generate calls
   */
  lazy val file: PackratParser[AST] = (
      (tile3.+)~map~(generates.+) ^^ {case ts~m~gs => new AST(ts, m, gs)}
//      | failure("Expected one or more tile specifications")<~map~(generates.+)
//      | (tile.+)~>failure("Expected a map specification")<~(generates.+)
//      | failure("File needs to have one or more tiles and a map specified")
      )

  /**
   * Tile: Either a basic tile (with an optional edge) or a freeform tile (with an optional anchor)
   */
  lazy val tile: PackratParser[(TileName, Tile)] = (
       tile3 ^^ {case returnval => returnval}
//      rword("tile")~>tilename~rword("=")~path~edge ^^ {case tname~"="~u~e => (tname, new BaseTile(tname, u, e))}
//      | rword("tile")~>tilename~rword("=")~path ^^ {case tname~"="~u => (tname, new  BaseTile(tname, u, ""))}
//      | rword("freeform")~>rword("tile")~>tilename~rword("=")~path~anchor ^^ {case tname~"="~p~a => (tname, new FreeTile(tname, p, a))}
//      | rword("freeform")~>rword("tile")~>ident~rword("=")~path ^^ {case tname~"="~p => (tname, new FreeTile(tname, p, new Point(0,0)))}
//      | failure("Improper tile definition")
      )// withFailureMessage("FAIL AT TILE")
      
  lazy val tile1: PackratParser [(TileName, Tile)] = (
      rword("tile")~>tilename~rword("=")~path~edge ^^ {case tname~"="~u~e => (tname, new BaseTile(tname, u, e))}
      ) withFailureMessage("FAIL AT tile1")
      
  lazy val tile2: PackratParser [(TileName, Tile)] = (
      rword("tile")~>tilename~rword("=")~path ^^ {case tname~"="~u => (tname, new  BaseTile(tname, u, ""))}
      ) withFailureMessage("FAIL AT tile2")
      
      lazy val tile4: PackratParser [(TileName, Tile)] = (
      rword("freeform")~>rword("tile")~>tilename~rword("=")~path~anchor ^^ {case tname~"="~p~a => (tname, new FreeTile(tname, p, a))}
      ) withFailureMessage("FAIL AT tile4")
      
      lazy val tile3: PackratParser [(TileName, Tile)] = (
      rword("freeform")~>rword("tile")~>rword("water")~>eq~>path ^^ {case p => ("water", new FreeTile("water", p, new Point(0,0)))}
          //rword("freeform tile water =")~>path ^^ {case p => ("water", new FreeTile("water", p, new Point(0,0)))}
          //rword("freeform")~>rword("tile")~>tilename~rword("=")~path ^^ {case tname~"="~p => (tname, new FreeTile(tname, p, new Point(0,0)))}
      ) //withFailureMessage("FAIL AT tile3")

      
  /**
   * Edge: specified with a filepath
   */
  lazy val edge: PackratParser[String] = (
      rword("{")~>rword("edge")~>rword("=")~>path~rword("}") ^^ {case p~"}" => p}
      | failure("A proper edge definition was not supplied")
      )

  /**
   * Anchor: specified with a point
   */
  lazy val anchor: PackratParser[Point] = (
      rword("{")~>rword("anchor")~>rword("=")~>point~rword("}") ^^ {case p~"}" => p}
      | failure("A proper anchor point was not supplied")
      )
  
  /**
   * Map: specified with a width, height, and optional origin, in any order, followed by one or more layers
   */
  lazy val map: PackratParser[Map] = (
      rword("map")~>rword("{")~>width~height~origin~(layer.+)~rword("}") ^^ {case w~h~o~l~"}" => new Map(w,h,o,l)}
      | rword("map")~>rword("{")~>height~width~origin~(layer.+)~rword("}") ^^ {case h~w~o~l~"}" => new Map(w,h,o,l) }
      | rword("map")~>rword("{")~>width~origin~height~(layer.+)~rword("}") ^^ {case w~o~h~l~"}" => new Map(w,h,o,l) }
      | rword("map")~>rword("{")~>height~origin~width~(layer.+)~rword("}") ^^ {case h~o~w~l~"}" => new Map(w,h,o,l) }
      | rword("map")~>rword("{")~>origin~width~height~(layer.+)~rword("}") ^^ {case o~w~h~l~"}" => new Map(w,h,o,l) }
      | rword("map")~>rword("{")~>origin~height~width~(layer.+)~rword("}") ^^ {case o~h~w~l~"}" => new Map(w,h,o,l) }
      | rword("map")~>rword("{")~>height~width~(layer.+)~rword("}") ^^ {case h~w~l~"}" => new Map(w,h,topLeft,l) }
      | rword("map")~>rword("{")~>width~height~(layer.+)~rword("}") ^^ {case w~h~l~"}" => new Map(w,h,topLeft,l) }
      | failure("Map specifications must come before layers and must include a width and a height")
      ) withFailureMessage("FAIL AT MAP")

  /**
  * Layer: specified with a number and one or more instructions
  */
  lazy val layer: PackratParser[Layer] = (
      rword("layer")~>wholeNumber~rword("=")~rword("{")~(instr.+)~rword("}") ^^ {case num~"="~"{"~is~"}" => new Layer(num.toInt, is)}
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
      rword("fill")~>rword("rectangle")~>(point.+)~rword("with")~tilename ^^ {case points~"with"~tname => new Area(tname, points, true)}
      | rword("fill")~>rword("area")~>(point.+)~rword("with")~tilename ^^ {case points~"with"~tname => new Area(tname, points, false)}
      | failure("Improper specification of fill area")
      ) 
      
  /**
   * PlaceAt: specified with a list of points and a tilename
   */
  lazy val placeAt: PackratParser[PlacePoint] = (
      rword("at")~>(point.+)~rword("place")~tilename ^^ {case points~"place"~tname => new PlacePoint(tname, points) }
      | failure("Improper specification of at statement")
      )

  /**
   * Generates: generate calls, either for maps or debug maps, with names to write the files as
   */
  lazy val generates: PackratParser[(MapType, String)] = (
      rword("generate")~>rword("map")~>rword("as")~>filename ^^ {case fname => (basic, fname)}
      | rword("generate")~>rword("debug")~>rword("map")~>rword("as")~>filename ^^ {case fname => (debug, fname)}
      | failure("Improper generate call")
      ) withFailureMessage("FAIL AT GENERATE")

  /**
   * Width, height, and origin specifications
   */
  lazy val width: PackratParser[Int] = (
      rword("width")~>rword("=")~>wholeNumber ^^ {case num => num.toInt}
      | failure("Improper width specification (all numbers must be ints)")
      )

  lazy val height: PackratParser[Int] = (
      rword("height")~>rword("=")~>wholeNumber ^^ {case num => num.toInt}
      | failure("Improper height specification (all numbers must be ints)")
      )

  lazy val origin: PackratParser[Origin] = (
      rword("origin")~>rword("=")~>originKeyword ^^ {case key => key}
      | failure("Improper origin specification")
      )

  lazy val originKeyword: PackratParser[Origin] = ( //ident ^^^ topLeft
      "bottomLeft" ^^^ bottomLeft
      | "topLeft" ^^^ topLeft
      | failure("Improper origin keyword")
      )   

  /**
   * Reserved word specification
   */
  def rword(word: String): PackratParser[String] = {
    ident filter {_ == word} //withFailureMessage "Expected reserved word <" + word + ">."
  }
  
  //To handle the `='
  def eq: PackratParser[String] = {
    ident filter{_ == "="}
  }

  /**
   * Point parser, as points appear multiple places
   */
  lazy val point: PackratParser[Point] = (
      rword("(")~>wholeNumber~rword(",")~wholeNumber~rword(")") ^^ {case x~","~y~")" => new Point(x.toInt, y.toInt)}
      | failure("Improper point definition")
      )

  /**
   * Other words for ident to give parser code more clarity
   */
  def tilename: Parser[TileName] = ident withFailureMessage("fail at Tilename")
  def path: Parser[String] = ident withFailureMessage("fail at PATH")
  def filename: Parser[String] = ident withFailureMessage("fail at Filename")

}