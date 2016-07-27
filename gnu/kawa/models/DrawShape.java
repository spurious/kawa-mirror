package gnu.kawa.models;

import gnu.mapping.SimpleSymbol;
import java.awt.*;
import java.awt.geom.*;
import java.util.ArrayList;
import java.util.List;

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

    private static Stroke mergeCap(int value, Stroke old) {
        return WithPaint.merge(new BasicStroke(1, value, 0),
                               WithPaint.STROKE_LINECAP_SET,
                               (BasicStroke) old);
    }

    private static Stroke mergeJoin(int value, Stroke old) {
        return WithPaint.merge(new BasicStroke(1, 0, value),
                               WithPaint.STROKE_LINEJOIN_SET,
                               (BasicStroke) old);
    }

    public static Picture makeDraw(List<Object> args) {
        Paint paint = null;
        Stroke stroke = null;
        int propertiesSet = 0;
        int nargs = args.size();
        ArrayList<DrawShape> ops = new ArrayList<DrawShape>();
        for (Object arg : args) {
            if (arg instanceof Shape) {
                ops.add(new DrawShape((Shape) arg));
                continue;
            }
            if (ops.size() > 0)
                throw new IllegalArgumentException("draw: property argument after shape argument");
            if (arg instanceof CharSequence || arg instanceof SimpleSymbol) {
                String str = arg.toString();
                if (str.equals("butt-cap")) {
                    propertiesSet |= WithPaint.STROKE_LINECAP_SET;
                    stroke = mergeCap(BasicStroke.CAP_BUTT, stroke);
                } else if (str.equals("square-cap")) {
                    propertiesSet |= WithPaint.STROKE_LINECAP_SET;
                    stroke = mergeCap(BasicStroke.CAP_SQUARE, stroke);
                } else if (str.equals("round-cap")) {
                    propertiesSet |= WithPaint.STROKE_LINECAP_SET;
                    stroke = mergeCap(BasicStroke.CAP_ROUND, stroke);
                } else if (str.equals("miter-join")) {
                    propertiesSet |= WithPaint.STROKE_LINEJOIN_SET;
                    stroke = mergeJoin(BasicStroke.JOIN_MITER, stroke);
                } else if (str.equals("round-join")) {
                    propertiesSet |= WithPaint.STROKE_LINEJOIN_SET;
                    stroke = mergeJoin(BasicStroke.JOIN_ROUND, stroke);
                } else if (str.equals("bevel-join")) {
                    propertiesSet |= WithPaint.STROKE_LINEJOIN_SET;
                    stroke = mergeJoin(BasicStroke.JOIN_BEVEL, stroke);
                } else {
                    Color color = StandardColor.valueOf(str);
                    if (color == null)
                        throw new IllegalArgumentException("draw: unknown keyword or color "+str);
                    paint = color;
                }
            } else if (arg instanceof Paint)
                paint = (Paint) arg;
            else if (arg instanceof Stroke) {
                propertiesSet |= WithPaint.STROKE_ALL_SET;
                stroke = (Stroke) arg;
            } else if (arg instanceof Number) {
                float width = ((Number) arg).floatValue();
                propertiesSet |= WithPaint.STROKE_WIDTH_SET;
                stroke = WithPaint.merge(new BasicStroke(width),
                                         WithPaint.STROKE_WIDTH_SET,
                                         (BasicStroke) stroke);
            } else {
                throw new IllegalArgumentException("draw: invalid argument type");
            }
        }
        Picture pic = PBox.combine(ops);
        if (paint != null || stroke != null)
            pic = new WithPaint(pic, paint, stroke, propertiesSet);
        return pic;
    }
}
