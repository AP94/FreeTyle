package freetyle.semantics

import freetyle.ir._

import scala.collection.mutable
import java.io.File
import java.io.IOException
import javax.imageio.ImageIO
import java.awt._
import java.awt.image._
import java.awt.{geom => geometry}
import java.util.Random

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
          case a: freetyle.ir.Area => {
            //assume origin is topLeft
              var top, bottom, left, right = 0
              for (point <- a.points) {
                if (point.y > bottom) {
                  bottom = point.y
                } else {
                  top = point.y
                }
                if (point.x > left) {
                  left = point.x
                } else {
                  right = point.x
                }
              }
              //get the rectangle coords for the area that will contain the tiles
              val TR = new freetyle.ir.Point(right, top)
              val TL = new freetyle.ir.Point(left, top)
              val BR = new freetyle.ir.Point(right, bottom)
              val BL = new freetyle.ir.Point(left, bottom)
            if (a.rect) {
              //make a rectangle to clip to
              val rect = new Rectangle(TL.x, TL.y, (TR.x-TL.x), (BR.y-TR.y))
              val clip = new geometry.Area(rect)
              graphics.clip(clip)
              
              //draw the tiles
              for (i <- TR.x until TL.x
                  if ((i - (TR.x))%tWidth == 0)) {
                for (j <- TR.y until BR.y
                  if ((j - (TR.y))%tHeight == 0)) {
                    graphics.drawImage(tile, i, j, null)
                }
              }
              
            } else {
              //make a polygon object
              var xPoints = new Array[Int](a.points.length)
              var yPoints = new Array[Int](a.points.length)
              var i = 0
              for (point <- a.points) {
                xPoints(i) = point.x
                yPoints(i) = point.y
                i += 1
              }
              val polygon = new Polygon(xPoints, yPoints, a.points.length)
              val clip = new geometry.Area(polygon)
              graphics.clip(clip)
              
              //draw the tiles
              for (i <- TR.x until TL.x
                  if ((i - (TR.x))%tWidth == 0)) {
                for (j <- TR.y until BR.y
                  if ((j - (TR.y))%tHeight == 0)) {
                    graphics.drawImage(tile, i, j, null)
                }
              }
            }
          }
           
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
    val layers = map.layers
    val canvas = new BufferedImage(map.width, map.height, BufferedImage.TYPE_INT_RGB)
    var graphics = canvas.createGraphics()
    
    // set the background color
    graphics.setColor(Color.WHITE)
    graphics.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
    
    var r = 0
    var g = 0
    var b = 0
    var col = new Color(0,0,0)
    val rand = new Random
    for(layer <- layers) {
      //Randomly generate a color for each layer
      r = rand.nextInt((255 - 0) + 1) + 0 // red component 0...255
      g = rand.nextInt((255 - 0) + 1) + 0// green component 0...255
      b = rand.nextInt((255 - 0) + 1) + 0// blue component 0...255
      col = new Color(r, g, b)
      graphics.setColor(col)
      
      //Each layer has a list of instructions
      //Write onto the canvas given the list of instructions
      for(instr <- layer.instructions) {
        val tile = tiles(instr.tileName)
        
        // Later: for clipping purposes
        val tWidth = tile.getWidth()
        val tHeight = tile.getHeight()
        
        instr match {
          case a: freetyle.ir.Area => {
            //assume origin is topLeft
              var top, bottom, left, right = 0
              for (point <- a.points) {
                if (point.y > bottom) {
                  bottom = point.y
                } else {
                  top = point.y
                }
                if (point.x > left) {
                  left = point.x
                } else {
                  right = point.x
                }
              }
              //get the rectangle coords for the area that will contain the tiles
              val TR = new freetyle.ir.Point(right, top)
              val TL = new freetyle.ir.Point(left, top)
              val BR = new freetyle.ir.Point(right, bottom)
              val BL = new freetyle.ir.Point(left, bottom)
            if (a.rect) {
              //draw the rectangle
              val rect = new geometry.Rectangle2D.Double(left, top, (right-left), (bottom-top))
              graphics.draw(rect)
              
            } else {
              //draw the polygon
              var xPoints = new Array[Int](a.points.length)
              var yPoints = new Array[Int](a.points.length)
              var i = 0
              xPoints(0) = a.points(0).x
              yPoints(0) = a.points(0).y
              for (point <- a.points) {
                i += 1
                xPoints(i) = point.x
                yPoints(i) = point.y
              }
              
              var line = new geometry.Line2D.Double(0,0,0,0)
              for (j <- 1 to i) {
                line = new geometry.Line2D.Double(xPoints(j-1), yPoints(j-1), xPoints(j), yPoints(j))
                graphics.draw(line)
              }
              line = new geometry.Line2D.Double(xPoints(0), yPoints(0), xPoints(i), yPoints(i))
              graphics.draw(line)
            }
          }
           
          // Draw freeform tiles + mark where the anchorpoints are
          case p: PlacePoint => {
            val pointList = p.points
            // The only way to get it to accept "anchor" is this way
            tTable(instr.tileName) match {
              case freeTile: FreeTile => {
                val anchorX = freeTile.anchor.x
                val anchorY = freeTile.anchor.y
                val offset = 3
                // Finally, at each point in the pointList, draw the tile
               
                 // If the orientation's normal, just draw it at pos
                 if (map.origin == topLeft) {
                  for (point <- pointList) {
                     graphics.draw(new geometry.Rectangle2D.Double(point.x, point.y, tWidth, tHeight));
                     
                     //draw the anchorpoint 
                     val shiftedAX = point.x + anchorX
                     val shiftedAY = point.y + anchorY
                     graphics.draw(new geometry.Line2D.Double(shiftedAX - offset, shiftedAY - offset, shiftedAX + offset, shiftedAY + offset))
                     graphics.draw(new geometry.Line2D.Double(shiftedAX + offset, shiftedAY - offset, shiftedAX - offset, shiftedAY + offset))
                     }
                  }
                 // Otherwise, it's bottomLeft, so adjust for this
                 else {
                   for (point <- pointList) {
                     graphics.draw(new geometry.Rectangle2D.Double(point.x, canvas.getHeight-point.y, tWidth, tHeight))
                     
                     //draw the anchorpoint
                     val shiftedAX = point.x + anchorX
                     val shiftedAY = canvas.getHeight-(point.y + anchorY)
                     graphics.draw(new geometry.Line2D.Double(shiftedAX - offset, shiftedAY - offset, shiftedAX + offset, shiftedAY + offset))
                     graphics.draw(new geometry.Line2D.Double(shiftedAX + offset, shiftedAY - offset, shiftedAX - offset, shiftedAY + offset))
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
}