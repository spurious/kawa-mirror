package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;

public interface Picture // extends Viewable
{
    public void paint(Graphics2D graphics);
    public Rectangle2D getBounds2D();
    public Picture transform (AffineTransform tr);
    public void visit(PictureVisitor visitor);
}
