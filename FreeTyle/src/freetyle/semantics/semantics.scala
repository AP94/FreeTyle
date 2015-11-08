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
  
  type loadedMap = mutable.HashMap[TileName, Boolean];
  type tileHashMap = mutable.HashMap[TileName, BufferedImage];
  
    def makeMap(map: Map) {
    val layers = map.layers
    val canvas = new BufferedImage(map.width, map.height, BufferedImage.TYPE_INT_RGB)
    var graphics = canvas.createGraphics()
    
    // set the background color
    graphics.setColor(Color.WHITE)
    graphics.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    
    // make the hashmaps
    val loaded = new loadedMap
    val tiles = new tileHashMap
    
    for(layer <- layers) {
      for(tilePoint <- layer.tilePoints) {
        val tile = tilePoint._1
        val pos = tilePoint._2
        val anch = tile.anchor
        // If the tile hasn't been loaded into an image, make an image file
        // and put it in the tiles hashmap
        if (!loaded(tile.name)) {
          tiles(tile.name) = ImageIO.read(tile.file)
          loaded(tile.name) = true
        }
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
  
  def makeDebugMap(map: Map) {
    
  }
}