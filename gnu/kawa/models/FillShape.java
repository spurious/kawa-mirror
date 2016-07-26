package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class FillShape implements Picture
{
  Shape shape;

  public FillShape (Shape shape)
  {
    this.shape = shape;
  }

  public Shape getShape() { return shape; }

  public void paint (Graphics2D graphics)
  {
    graphics.fill(shape);
  }

  public Rectangle2D getBounds2D()
  {
    return shape.getBounds2D();
  }

  public Picture transform (AffineTransform tr)
  {
    return new FillShape(tr.createTransformedShape(shape));
  }

    public void visit(PictureVisitor visitor) {
        visitor.visitFillShape(this);
    }
}
