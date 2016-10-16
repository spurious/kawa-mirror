package gnu.kawa.models;
import gnu.kawa.io.CharArrayOutPort;
import gnu.lists.PrintConsumer;
import gnu.lists.Consumer;
import gnu.kawa.xml.KElement;
import gnu.kawa.xml.KNode;
import gnu.mapping.Symbol;
import gnu.xml.NodeTree;
import gnu.xml.XMLFilter;
import gnu.xml.XMLPrinter;
import java.awt.*;
import java.awt.geom.*;

public class SVGUtils {
    public static boolean haveToSvg = true;

    public static String toSVG(Picture p) {
        CharArrayOutPort cout = new CharArrayOutPort();
        XMLPrinter xout = new XMLPrinter(cout);
        toSVG(p, xout);
        return cout.toString();
    }

    public static void toSVG(Picture p, PrintConsumer out, boolean headers) {
        if (headers) {
            out.println("<?xml version=\"1.0\" standalone=\"no\"?>");
            out.println("<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" "
                + "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">");
        }
        toSVG(p, new XMLPrinter(out));
    }
    
    public static void toSVG(Picture p, Consumer out) {
        Rectangle2D bounds = adjustBounds(p, p.getBounds2D(), null);
        PictureToSvg.writeSVGElementStart(bounds, out);
        PictureToSvg pout = new PictureToSvg(out);
        p.visit(pout);
        out.endElement();
    }

    public static KElement toSVGNode(Picture p) {
        XMLFilter filter = new XMLFilter(new NodeTree());
        toSVG(p, filter);
        return (KElement) KNode.make((NodeTree) filter.out);
    }

    public static Rectangle2D adjustBounds(Picture pic, Rectangle2D bounds, AffineTransform transform) {
        AdjustBounds visitor = new AdjustBounds(bounds, transform);
        pic.visit(visitor);
        return visitor.getBounds();
    }

    static class AdjustBounds extends PictureVisitor.TrackingState {
        double x0, y0, x1, y1;
        Rectangle2D bounds;
        boolean adjusted;

        public AdjustBounds(Rectangle2D bounds, AffineTransform transform) {
            super(transform);
            this.bounds = bounds;
            this.x0 = bounds.getX();
            this.y0 = bounds.getY();
            this.x1 = this.x0 + bounds.getWidth();
            this.y1 = this.y0 + bounds.getHeight();
        }
        public Rectangle2D getBounds() {
            Rectangle2D b = this.bounds;
            if (adjusted)
                b = new Rectangle2D.Double(x0, y0, x1 - x0, y1 - y0);
            return b;
        }
        
        @Override
        public void visitDrawShape(DrawShape pic) {
            Shape shape = pic.shape;
            if (stroke instanceof BasicStroke
                && (strokePropertiesSet & WithPaint.STROKE_WIDTH_SET) != 0) {
                double lineWidth = ((BasicStroke) stroke).getLineWidth();
                double halfWidth = 0.5 * lineWidth;
                if (transform != null)
                    shape = transform.createTransformedShape(shape);
                Rectangle2D transformedBounds = shape.getBounds2D();
                double sx0 = transformedBounds.getX() - halfWidth;
                double sx1 = sx0 + transformedBounds.getWidth() + lineWidth;
                double sy0 = transformedBounds.getY() - halfWidth;
                double sy1 = sy0 + transformedBounds.getHeight() + lineWidth;
                if (sx0 < this.x0) {
                    this.x0 = sx0;
                    this.adjusted = true;
                }
                if (sy0 < this.y0) {
                    this.y0 = sy0;
                    this.adjusted = true;
                }
                if (sx1 > this.x1) {
                    this.x1 = sx1;
                    this.adjusted = true;
                }
                if (sy1 > this.y1) {
                    this.y1 = sy1;
                    this.adjusted = true;
                }
            }
        }
    }
}
