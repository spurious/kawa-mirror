package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;

public class DrawImage implements Paintable
{
  BufferedImage image;

  public DrawImage (BufferedImage image)
  {
    this.image = image;
  }

  public void paint (Graphics2D graphics)
  {
    graphics.drawImage(image, null, null);
  }

  public Rectangle2D getBounds2D()
  {
    int w = image.getWidth();
    int h = image.getHeight();
    return new Rectangle2D.Float(0, 0, w, h);
  }

  public Paintable transform (AffineTransform tr)
  {
    return new WithTransform(this, tr);
  }
}
