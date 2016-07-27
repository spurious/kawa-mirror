package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
import java.util.List;

public class WithPaint implements Picture
{
    Picture picture;
    Paint paint;
    Stroke stroke;
    int propertiesSet;
    public static final int STROKE_WIDTH_SET = 1;
    public static final int STROKE_LINECAP_SET = 2;
    public static final int STROKE_LINEJOIN_SET = 4;
    public static final int STROKE_MITERLIMIT_SET = 8;
    public static final int STROKE_DASHARRAY_SET = 16;
    public static final int STROKE_DASHOFFSET_SET = 32;
    public static final int STROKE_ALL_SET = 63;

    public WithPaint(Picture picture, Paint paint) {
        this.picture = picture;
        this.paint = paint;
    }

    public WithPaint(Picture picture, Paint paint,
                     Stroke stroke, int propertiesSet) {
        this.picture = picture;
        this.paint = paint;
        this.stroke = stroke;
        this.propertiesSet = propertiesSet;
    }

    public static BasicStroke merge(BasicStroke newStroke, int select,
                                    BasicStroke oldStroke) {
        if (oldStroke == null)
            return newStroke;
        float width = (select & STROKE_WIDTH_SET) != 0
            ? newStroke.getLineWidth() : oldStroke.getLineWidth();
        int cap = (select & STROKE_LINECAP_SET) != 0
            ? newStroke.getEndCap() : oldStroke.getEndCap();
        int join = (select & STROKE_LINEJOIN_SET) != 0
            ? newStroke.getLineJoin() : oldStroke.getLineJoin();
        float miterlimit = (select & STROKE_MITERLIMIT_SET) != 0
            ? newStroke.getMiterLimit() : oldStroke.getMiterLimit();
        float[] dash = (select & STROKE_DASHARRAY_SET) != 0
            ? newStroke.getDashArray() : oldStroke.getDashArray();
        float dash_phase = (select & STROKE_DASHOFFSET_SET) != 0
            ? newStroke.getDashPhase() : oldStroke.getDashPhase();
        return new BasicStroke(width, cap, join, miterlimit, dash, dash_phase);
    }

    public void paint(Graphics2D graphics) {
        Paint savedPaint = graphics.getPaint();
        Stroke savedStroke = graphics.getStroke();
        try {
            if (paint != null)
                graphics.setPaint(paint);
            if (stroke != null) {
                Stroke nstroke = stroke;
                if (savedStroke instanceof BasicStroke
                    && stroke instanceof BasicStroke
                    && (propertiesSet & STROKE_ALL_SET) != STROKE_ALL_SET)
                    nstroke = merge((BasicStroke) stroke, propertiesSet,
                                    (BasicStroke) savedStroke);
                graphics.setStroke(nstroke);
            }
            picture.paint(graphics);
        } finally {
            if (paint != null)
                graphics.setPaint(savedPaint);
            if (stroke != null)
                graphics.setStroke(savedStroke);
        }
    }

    public Rectangle2D getBounds2D() {
        return picture.getBounds2D();
    }

    public Picture transform(AffineTransform tr) {
        return new WithPaint(picture.transform(tr), paint,
                             stroke, propertiesSet);
    }

    public void visit(PictureVisitor visitor) {
        visitor.visitWithPaint(this);
    }
}
