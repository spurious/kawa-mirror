package gnu.kawa.models;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.*;
import java.util.List;

/** Used to compose Pictures "next to" each other.
 * They be put in a row (X-axis), in a column (Y-axis),
 * or on top of each other with same origin (Z-axis).
 */

public class PBox implements Picture {
    char axis; // X, Y, or Z
    Picture[] children;
    Rectangle2D bounds;

    double[] translations;
    /** Extra spacing between sub-pictures. */
    double spacing;

    private PBox(char axis, Picture[] children) {
        this.axis = axis;
        this.children = children;
        init();
    }

    private PBox(char axis, double spacing, Picture[] children) {
        this.axis = axis;
        this.children = children;
        this.spacing = spacing;
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
            double delta;
            if (axis == 'X') {
                delta = spacing + prevBounds.getMaxX() - curBounds.getMinX();
                deltaX += delta;
            } else if (axis == 'Y') {
                delta = spacing + prevBounds.getMaxY() - curBounds.getMinY();
                deltaY += delta;
            } else
                delta = 0;
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

    public Picture transform(AffineTransform tr) {
        return new WithTransform(this, tr);
    }

    public void visit(PictureVisitor visitor) {
        visitor.visitPBox(this);
    }

    public static PBox makeBox(char axis, Object... args) {
        int nargs = args.length;
        int start = 0;
        double spacing = 0;
        if (nargs > 0 && args[0] instanceof Number) {
            start = 1;
            spacing = ((Number) args[0]).doubleValue();
        }
        Picture[] pictures = new Picture[nargs-start];
        for (int i = start; i < nargs; i++)
            pictures[i-start] = Pictures.asPicture(args[i]);
        return new PBox(axis, spacing, pictures);
    }

    public static Picture combine(List parts) {
        int nparts = parts.size();
        Picture[] pics = new Picture[nparts];
        parts.toArray(pics);
        return nparts == 1 ? pics[0] : new PBox('Z', pics);
    }
}
