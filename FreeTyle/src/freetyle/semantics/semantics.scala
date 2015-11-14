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
            makeMap(ast.map, tileHash, genCall._2)
          } else {
            makeDebugMap(ast.map, tileHash, genCall._2)
          }
        }
      }
      if (ast.map.layers.length == 0) {
        //error handling for empty list of layers (how?)
      }
    }
   
  
    def makeMap(map: Map, tiles: tileHashMap, mName: String) {
    val layers = map.layers
    val canvas = new BufferedImage(map.width, map.height, BufferedImage.TYPE_INT_RGB)
    var graphics = canvas.createGraphics()
    
    // set the background color
    graphics.setColor(Color.WHITE)
    graphics.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    
    
    for(layer <- layers) {
      //Each layer has a list of instructions
      //Write onto the canvas given the list of instructions
      
      
      
      
      for(tilePoint <- layer.tilePoints) {
        val tile = tilePoint._1
        val pos = tilePoint._2
        val anch = tile.anchor

        // Later: for clipping purposes
        val tWidth = tiles(tile.name).getWidth()
        val tHeight = tiles(tile.name).getHeight()
        
        // If the orientation's normal, just draw it at pos
        if (map.origin == topLeft) {
          graphics.drawImage(tiles(tile.name), pos.x, pos.y, null)
        }
        // Otherwise, it's bottomLeft, so adjust for this
        else {
          graphics.drawImage(tiles(tile.name), pos.x, canvas.getHeight-pos.y, null)
        }
      }
    graphics.dispose()
    graphics = canvas.createGraphics()
    }
    
    graphics.dispose()
    val outputfile = new File(map.name + ".png")
    ImageIO.write(canvas, "png", outputfile)
  }
  
  def makeDebugMap(map: Map, tiles: tileHashMap, mName: String) {
    
  }
}