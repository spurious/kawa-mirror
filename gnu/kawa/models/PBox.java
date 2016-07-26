package gnu.kawa.models;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.*;
import java.awt.image.BufferedImage;

/** Used to compose Paintables "next to" each other.
 * They be put in a row (X-axis), in a column (Y-axis),
 * or on top of each other with same origin (Z-axis).
 */

public class PBox implements Paintable {
    char axis; // X, Y, or Z
    Paintable[] children;
    Rectangle2D bounds;

    double[] translations;

    private PBox(char axis, Paintable[] children) {
        this.axis = axis;
        this.children = children;
        init();
    }

    public Rectangle2D getBounds2D() {
        return bounds;
    }

    void init() {
        int n = children.length;
        if (n == 0)
            return;
        Rectangle2D prevBounds = children[0].getBounds2D();
        double minX = prevBounds.getMinX();
        double maxX = prevBounds.getMaxX();
        double minY = prevBounds.getMinY();
        double maxY = prevBounds.getMaxY();
        double deltaX = 0, deltaY = 0;
        translations = new double[n];
        for (int i = 1; i < n; i++) {
            Rectangle2D curBounds = children[i].getBounds2D();
            double delta = 0;
            if (axis == 'X') {
                delta = prevBounds.getMaxX() - curBounds.getMinX();
                deltaX += delta;
            } else if (axis == 'Y') {
                delta = prevBounds.getMaxY() - curBounds.getMinY();
                deltaY += delta;
            }
            translations[i] = delta + translations[i-1];
            double cminX = curBounds.getMinX() + deltaX;
            if (cminX < minX)
                minX = cminX;
            double cminY = curBounds.getMinY() + deltaY;
            if (cminY < minY)
                minY = cminY;
            double cmaxX = curBounds.getMaxX() + deltaX;
            if (cmaxX > maxX)
                maxX = cmaxX;
            double cmaxY = curBounds.getMaxY() + deltaY;
            if (cmaxY > maxY)
                maxY = cmaxY;
           prevBounds = curBounds;
        }
        bounds = new Rectangle2D.Double(minX, minY, maxX-minX, maxY-minY);
    }

    public void paint (Graphics2D graphics) {
        AffineTransform saved = graphics.getTransform();
        try {
            int n = children.length;
            double prevOffset = 0;
            for (int i = 0; i < n; i++) {
                double offset = translations[i];
                if (i > 0 && axis != 'Z') {
                    double delta = offset - prevOffset;
                    if (axis == 'X')
                        graphics.translate(delta, 0);
                    else
                        graphics.translate(0, delta);
                }
                prevOffset = offset;
                children[i].paint(graphics);
            }
        } finally {
            graphics.setTransform(saved);
        }
    }

    public Paintable transform(AffineTransform tr) {
        return new WithTransform(this, tr);
    }
    public static PBox makeHBox(Object... args) {
        return new PBox('X', asPaintableAll(args));
    }
    public static PBox makeVBox(Object... args) {
        return new PBox('Y', asPaintableAll(args));
    }
    public static PBox makeZBox(Object... args) {
        return new PBox('Z', asPaintableAll(args));
    }
    public static Paintable asPaintable(Object arg) {
        if (arg instanceof BufferedImage)
            return new DrawImage((BufferedImage) arg);
        if (arg instanceof Shape)
            return new DrawShape((Shape) arg);
        return (Paintable) arg;
    }
    public static Paintable[] asPaintableAll(Object[] args) {
        int np = args.length;
        Paintable[] p = new Paintable[np];
        for (int i = 0; i < np; i++)
            p[i] = asPaintable(args[i]);
        return p;
    }
    public void visit(PictureVisitor visitor) {
        visitor.visitPBox(this);
    }
}
