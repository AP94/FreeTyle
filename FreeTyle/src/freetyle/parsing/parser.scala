package freetyle.parsing

import freetyle.ir._
import scala.util.parsing.combinator._
import scala.collection.mutable;

/**
 * @author apinson
 */
object parser extends JavaTokenParsers with PackratParsers{
  
  /*
   * Started @ 12:45
   * 
   * first, check for tiles:
   * either basicTile or freeTile
   * at least one of either
   * followed by 0 or more other tiles
   * 
   * each basic tile: keyword "tile" 
   * followed by tileName followed by = followed by SRCPATH
   * followed by an optional (0 or 1) "{ (newline) edge = " followed by SRCPATH followed by "}"
   * each freeform tile: keyword "freeform"
   * followed by tileName followed by = followed by SRCPATH
   * followed by an optional "{ (newline) anchor = " followed by anchorPoint followed by "}"
   * 
   * after these, there comes a (one or more?) map specification(s).
   * "map {" followed by "width = " width
   * "height = " height
   * "origin = " originKeyword
   * consider: specify these in any order?
   * followed by one or more layers
   * followed by "}" followed by 0 or more Generate(s)
   * 
   * each layer:
   * "layer " layer# " = {" followed by one or more layerInstr(s)
   * "}"
   * 
   * layerInstrs: "fill " followed by optional Shape followed by "with " followed by tileName
   * OR "at" followed by 1 or more place(s) followed by "place" tileName
   * both followed by 0 or more layerInstrs
   * 
   * Generate:
   * "generate map as " FILENAME
   * OR "generate debug map as " FILENAME
   * 
   * for basic thing: handle freeform tile
   * handle map
   * handle at/place
   * handle generate
   * 
   */
  
  def apply(s: String): ParseResult[AST] = parseAll(file, s)
  
  lazy val file: PackratParser[AST] = (
      (tile.+)~(map.+)~generates.? ^^ {case ts~m~gs => new AST(ts, m, gs)}
      | (map.+)~generates.?~>failure("Expected one or more tile specifications")
      | (tile.+)~generates.?~>failure("Expected a map specification")
      | failure("File needs to have one or more tiles and a map specified")
      )
      
  lazy val tile: PackratParser[mutable.HashMap[TileName, Tile]] = (
      (baseTile.+) ^^ {case (bt, bn) => if (tTable.contains(bn) == false) {tTable(bn) = bt} }
      | (freeTile.+) ^^ {case (ft, fn) => if (tTable.contains(fn) == false) {tTable(fn) = ft} }
      )
      
  lazy val baseTile: PackratParser[(BaseTile, TileName)]
  
//  lazy val tile: PackratParser[Tile] = (
//      rword("tile ")~tileName~rword(" = ")~srcPath~tileParams.?
//      
//  
//  lazy val map: PackratParser[Layer] = (
//      rword("map ")~mapName~rword(" {")~rword("width = ")~>width~rword("height = ")~height~
//        rword("origin = ")~origin~layer.+ ^^ { case _~n~_~w~_~_~h~_~o~ls => new Map(w,h,o,ls,n)}
//  | failure("Map failed to parse. Check that a name is provided outside map declaration and that width, height, and origin are specified before layers")    
//  )
//  
  def rword(word: String): PackratParser[String] = {
    ident filter {_ == word} withFailureMessage "Expected reserved word <" + word + ">."
  }

}