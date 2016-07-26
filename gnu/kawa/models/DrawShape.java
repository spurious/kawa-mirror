package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public class DrawShape implements Picture
{
  Shape shape;

  public DrawShape (Shape shape)
  {
    this.shape = shape;
  }
  public Shape getShape() { return shape; }

  public void paint (Graphics2D graphics)
  {
    graphics.draw(shape);
  }

  public Rectangle2D getBounds2D()
  {
    return shape.getBounds2D();
  }

  public Picture transform (AffineTransform tr)
  {
    return new DrawShape(tr.createTransformedShape(shape));
  }

    public void visit(PictureVisitor visitor) {
        visitor.visitDrawShape(this);
    }
}
