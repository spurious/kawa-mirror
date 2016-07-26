package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

// FIXME rename to Picture
public interface Paintable // extends Viewable
{
  public void paint(Graphics2D graphics);
  public Rectangle2D getBounds2D();
  public Paintable transform (AffineTransform tr);
    public void visit(PictureVisitor visitor);
}
