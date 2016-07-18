package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
/* #ifdef with:VectorGraphics2D */
// import de.erichseifert.vectorgraphics2d.Document;
// import de.erichseifert.vectorgraphics2d.VectorGraphics2D;
// import de.erichseifert.vectorgraphics2d.svg.SVGProcessor;
// import de.erichseifert.vectorgraphics2d.util.PageSize;
// import java.io.ByteArrayOutputStream;
/* #endif */

public class SVGUtils {
    /* #ifdef with:VectorGraphics2D */
    // public static boolean haveToSvg = true;
    /* #else */
    public static boolean haveToSvg = false;
    /* #endif */
    public static final String UNSUPPORTED =
        "[converting to SVG not supported]";
    public static String toSVG(Paintable p) {
        /* #ifdef with:VectorGraphics2D */
        // SVGProcessor svgProcessor;
        // VectorGraphics2D g2;
        // try {
        //     svgProcessor = new SVGProcessor();
        //     g2 = new VectorGraphics2D();
        // } catch (Throwable ex) {
        //     return UNSUPPORTED;
        // }
        // try {
        //     Rectangle2D bounds = p.getBounds2D();
        //     /*(int)Math.ceil(bounds.getWidth()), (int)Math.ceil(bounds.getHeight()));*/
        //     g2.setPaint(Color.BLACK);
        //     p.paint(g2);
        //     Document document = svgProcessor.getDocument(g2.getCommands(), new PageSize(bounds));
        //     ByteArrayOutputStream byteOutput = new ByteArrayOutputStream();
        //     document.writeTo(byteOutput);
        //     String str = new String(byteOutput.toByteArray()); // FIXME
        //     str = str.trim();
        //     int doctype = str.indexOf("<!DOCTYPE svg");
        //     if (doctype >= 0) {
        //         int close = str.indexOf(">\n", doctype);
        //         if (close > 0)
        //             str = str.substring(close+2);
        //     }
        //     return str;
        // } catch (Throwable ex) {
        //     ex.printStackTrace();
        //     return "[Caught "+ex+" while generating SVG]";
        // }
        /* #else */
        return UNSUPPORTED;
        /* #endif */
    }
}
