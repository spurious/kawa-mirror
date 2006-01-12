package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import gnu.mapping.WrappedException;
import java.net.URL;
import gnu.text.URI_utils;

public class DrawImage extends Model
  implements Paintable, java.io.Serializable
{
  BufferedImage image;
  /* #ifdef use:java.net.URI */
  java.net.URI src;
  /* #else */
  // String src;
  /* #endif */
  String description;

  public DrawImage ()
  {
  }

  public void makeView (Display display, Object where)
  {
    display.addImage(this, where);
  }

  void loadImage ()
  {
    if (image == null)
      {
        try
          {
            image = javax.imageio.ImageIO.read(URI_utils.getInputStream(src));
          }
        catch (Throwable ex)
          {
            throw WrappedException.wrapIfNeeded(ex);
          }
      }
  }

  public DrawImage (BufferedImage image)
  {
    this.image = image;
  }

  public void paint (Graphics2D graphics)
  {
    loadImage();
    graphics.drawImage(image, null, null);
  }

  public Rectangle2D getBounds2D()
  {
    loadImage();
    int w = image.getWidth();
    int h = image.getHeight();
    return new Rectangle2D.Float(0, 0, w, h);
  }

  public Paintable transform (AffineTransform tr)
  {
    return new WithTransform(this, tr);
  }

  public Image getImage ()
  {
    loadImage();
    return image;
  }

  public
  /* #ifdef use:java.net.URI */
  java.net.URI
  /* #else */
  // String
  /* #endif */
  getSrc () { return src; }

  public void setSrc (Object src)
  /* #ifdef use:java.net.URI */
    throws java.net.URISyntaxException
  /* #endif */
  {
    this.src = URI_utils.toURI(src);
  }
}
