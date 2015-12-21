package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
/* #ifdef with:JFreeSVG */
// import org.jfree.graphics2d.svg.SVGGraphics2D;
/* #endif */

public class SVGUtils {
    public static final String JFREESVG_UNAVAILABLE
        = new String("[JFreeSVG is unavailable]");
    public static String toSVG(Paintable p) {
        /* #ifdef with:JFreeSVG */
        // Rectangle2D bounds = p.getBounds2D();
        // SVGGraphics2D g2 = new SVGGraphics2D((int)Math.ceil(bounds.getWidth()),
        //                                      (int)Math.ceil(bounds.getHeight()));
        // g2.setPaint(Color.BLACK);
        // g2.translate(bounds.getMinX(), -bounds.getMinY());
        // p.paint(g2);
        // return g2.getSVGElement();
        /* #else */
        return JFREESVG_UNAVAILABLE;
        /* #endif */
    }
}
