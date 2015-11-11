package freetyle.ir
import scala.collection.mutable;

import scala.language.postfixOps

/**
 * @author apinson
 */


/**
 * Points: Will be declared as tuples (x,y)
 */
class Point(xCoord: Int, yCoord: Int) {
  val x = xCoord
  val y = yCoord
}

/** 
 *  Tiles: Basic tiles are for tiling; presumably, they can be repeated.
 *   Basic tiles also may have edges, while freeform tiles cannot.
 */

class Tile(tileName: TileName) {
  val name = tileName
}

class BaseTile(tileName: TileName, url: String, edgeUrl: String) extends Tile(tileName) {
  val fileName = url
  if (edgeUrl == "") {
     val edgeFile = null
  } else {
     val edgeFile = new java.io.File(url)
  }
}

/**
 * Freeform tiles don't extend from Tile; their functionality is separate
 * Freeform tiles always have an anchorPoint; the standard is the top left of the image file
 * Freeform tiles never have an edge.
 */
class FreeTile(tileName: TileName, url: String, anchorPoint: Point) extends Tile(tileName) {
  val file = new java.io.File(url)
  val anchor = anchorPoint
  
}

/**
 * The Origin specifies how the map is oriented.
 */
abstract sealed class Origin
case object topLeft extends Origin
case object bottomLeft extends Origin

abstract sealed class MapType
case object basic extends MapType
case object debug extends MapType

/**
 * Areas are (currently) specified by right angles and will be filled by a specified tile.
 */
class Area(t: TileName, pointPairs: List[(Point, Point)]){
  val tile = t
  val zones = pointPairs
}

/**
 * Layers contain areas to be tiled and/or a list of FreeTiles and where to place them.
 * Layer precedence is given by a number, where the highest number specifies the topmost layer.
 */
class Layer(prec: LayerNum, as: List[Area], tps: List[(TileName, Point)]) {
  val precedence = prec
  val areas = as
  val tilePoints = tps
}

/**
 * Maps are given a set size and name and are composed of layers. The origin specifies the orientation of the map.
 */
class Map(w: Int, h: Int, orig: Origin, lays: List[Layer]) {
  val width = w
  val height = h
  val origin = orig
  val layers = lays
}

class Table(list: List[TileName, Tile]) {
  val hash = new mutable.HashMap[TileName, Tile]
  // Hash Construction Goes Here
}

class AST(tTable: Table, m: Map, gens: List[(MapType, String)]) {
  val tileTable = tTable
  val map = m
  val genCalls = gens
}

