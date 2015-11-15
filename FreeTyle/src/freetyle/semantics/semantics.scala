package freetyle.semantics

import freetyle.ir._

import scala.collection.mutable;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._

/**
 * @author apinson
 */

private object semantics {
  import freetyle.{semantics => lib}
  
  type tileHashMap = mutable.HashMap[TileName, BufferedImage];
  
    def loadAST(ast: AST) {
      val astHash = ast.tileTable.hash
      val tileHash = new tileHashMap
      // Only do the work if there are maps to generate
      // (Potentially change this if errors won't be caught)
      if (ast.genCalls.length != 0) {
        // Check if the "error" case happened (not sure how to implement)
        if (astHash.contains("error")) {
          //error handling
        } else {
          // Make the tile hashmap from the ast's tiletable
          for (tName <- astHash.keys) {
            tileHash(tName) = ImageIO.read(astHash(tName).file)
          }
        }
        for (genCall <- ast.genCalls) {
          if (genCall._1 == basic) {
            makeMap(ast.map, tileHash, astHash, genCall._2)
          } else {
            makeDebugMap(ast.map, tileHash, astHash, genCall._2)
          }
        }
      }
      if (ast.map.layers.length == 0) {
        //error handling for empty list of layers (how?)
      }
    }
   
  
    def makeMap(map: Map, tiles: tileHashMap, tTable: mutable.HashMap[TileName, Tile], mName: String) {
    val layers = map.layers
    val canvas = new BufferedImage(map.width, map.height, BufferedImage.TYPE_INT_RGB)
    var graphics = canvas.createGraphics()
    
    // set the background color
    graphics.setColor(Color.WHITE)
    graphics.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    
    
    for(layer <- layers) {
      //Each layer has a list of instructions
      //Write onto the canvas given the list of instructions
      for(instr <- layer.instructions) {
        val tile = tiles(instr.tileName)
        
        // Later: for clipping purposes
        val tWidth = tile.getWidth()
        val tHeight = tile.getHeight()
        
        instr match {
          // TODO: Implement areas
          case a: freetyle.ir.Area => null
           
          // Place freeform tiles
          case p: PlacePoint => {
            val pointList = p.points
            // The only way to get it to accept "anchor" is this way
            tTable(instr.tileName) match {
              case freeTile: FreeTile => {
                val anchor = freeTile.anchor
                // Finally, at each point in the pointList, draw the tile
                
                 // If the orientation's normal, just draw it at pos
                 if (map.origin == topLeft) {
                  for (point <- pointList) {
                    graphics.drawImage(tile, point.x, point.y, null)
                  }
                 }
                 // Otherwise, it's bottomLeft, so adjust for this
                 else {
                   for (point <- pointList) {
                    graphics.drawImage(tile, point.x, canvas.getHeight-point.y, null)
                  }
                 }
              }
              case b: BaseTile => {
                //ERROR: This is impossible
              }
            }
          }
        }
      }
      graphics.dispose()
      graphics = canvas.createGraphics()
    }
    graphics.dispose()
    val outputfile = new File(mName + ".png")
    ImageIO.write(canvas, "png", outputfile)
  }
  
  def makeDebugMap(map: Map, tiles: tileHashMap, tTable: mutable.HashMap[TileName, Tile], mName: String) {
    // TODO: Implement this
  }
}