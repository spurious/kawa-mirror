package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class WithPaint implements Picture
{
  Picture picture;
  Paint paint;

  public WithPaint(Picture picture, Paint paint)
  {
    this.picture = picture;
    this.paint = paint;
  }

  public void paint (Graphics2D graphics)
  {
    Paint saved = graphics.getPaint();
    try
      {
	graphics.setPaint(paint);
	picture.paint(graphics);
      }
    finally
      {
	graphics.setPaint(saved);
      }
  }

  public Rectangle2D getBounds2D()
  {
    return picture.getBounds2D();
  }

  public Picture transform (AffineTransform tr)
  {
    return new WithPaint(picture.transform(tr), paint);
  }

    public void visit(PictureVisitor visitor) {
        visitor.visitWithPaint(this);
    }
}
