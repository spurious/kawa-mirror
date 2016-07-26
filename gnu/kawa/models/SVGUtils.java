package gnu.kawa.models;
import gnu.kawa.io.CharArrayOutPort;
import gnu.lists.Consumer;
import gnu.mapping.Symbol;
import gnu.xml.XMLPrinter;
import java.awt.*;
import java.awt.geom.*;

public class SVGUtils {
    public static boolean haveToSvg = true;

    public static String toSVG(Paintable p) {
        CharArrayOutPort cout = new CharArrayOutPort();
        XMLPrinter xout = new XMLPrinter(cout);
        PictureToSvg.writeSVGElementStart(p.getBounds2D(), xout);
        PictureToSvg pout = new PictureToSvg(xout);
        p.visit(pout);
        xout.endElement();
        //System.err.println("{SVG:"+cout.toString()+"}");
        return cout.toString();
    }
}
