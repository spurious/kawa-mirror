package gnu.kawa.models;

import gnu.kawa.io.CharArrayOutPort;
import gnu.kawa.io.Path;
import gnu.lists.Consumer;
import gnu.mapping.Symbol;
import gnu.xml.NamespaceBinding;
import gnu.xml.XMLPrinter;
import gnu.xml.XName;
import java.awt.*;
import java.awt.geom.*;
import java.io.ByteArrayOutputStream;
     
public class PictureToSvg extends PictureVisitor {
    Consumer out;
    Paint paint = StandardColor.black;
    Stroke stroke;
    int strokePropertiesSet;

    public static final String SVG_NAMESPACE_URI =
        "http://www.w3.org/2000/svg";
    public static final String XLINK_NAMESPACE_URI =
        "http://www.w3.org/1999/xlink";

    public PictureToSvg(Consumer out) {
        this.out = out;
    }

    public static Symbol svgTag(String name) {
        return Symbol.make(SVG_NAMESPACE_URI, name, "");
    }
    public static Symbol xlinkTag(String name) {
        return Symbol.make(XLINK_NAMESPACE_URI, name, "xlink");
    }
    public static void writeAttribute(String name, String value, Consumer out) {
        out.startAttribute(Symbol.valueOf(name));
        out.write(value);
        out.endAttribute();
    }
    public static void writeAttribute(String name, double value, Consumer out) {
        writeAttribute(name, formatDouble(value), out);
    }
    public static String formatDouble(double value) {
        String str = Double.toString(value);
        int len = str.length();
        int dot = str.indexOf('.');
        if (dot >= 0) {
            int j = len;
            while (j > dot && (j == dot+1 || str.charAt(j-1) == '0'))
                j--;
            str = str.substring(0, j);
        }
        return str;
    }
    public static String genShapeToString(Shape shape) {
        StringBuilder out = new StringBuilder();
        PathIterator it = shape.getPathIterator(null);
        double[] data = new double[6];
        while (! it.isDone()) {
            int kind = it.currentSegment(data);
            if (out.length() > 0)
                out.append(' ');
            char code = 0;
            int ncoords = 0;
            switch (kind) {
            case PathIterator.SEG_MOVETO:  code = 'M'; ncoords = 2; break;
            case PathIterator.SEG_LINETO:  code = 'L'; ncoords = 2; break;
            case PathIterator.SEG_QUADTO:  code = 'Q'; ncoords = 4; break;
            case PathIterator.SEG_CUBICTO: code = 'C'; ncoords = 6; break;
            case PathIterator.SEG_CLOSE:   code = 'Z'; ncoords = 0; break;
            }
            out.append(code);
            for (int j = 0; j < ncoords; j++) {
                if (j > 0)
                    out.append(',');
                out.append(data[j]);
            }
            it.next();
        }
        return out.toString();
    }

    public static void writeShapeStart(Shape shape, Consumer out) {
        if (shape instanceof Line2D) {
            Line2D s = (Line2D) shape;
            out.startElement(svgTag("line"));
            writeAttribute("x1", s.getX1(), out);
            writeAttribute("y1", s.getY1(), out);
            writeAttribute("x2", s.getX2(), out);
            writeAttribute("y2", s.getY2(), out);
        } else if (shape instanceof Rectangle2D) {
            Rectangle2D s = (Rectangle2D) shape;
            out.startElement(svgTag("rect"));
            writeAttribute("x", s.getX(), out);
            writeAttribute("y", s.getY(), out);
            writeAttribute("width", s.getWidth(), out);
            writeAttribute("height", s.getHeight(), out);
        } else if (shape instanceof RoundRectangle2D) {
            RoundRectangle2D s = (RoundRectangle2D) shape;
            out.startElement(svgTag("rect")); 
            writeAttribute("x", s.getX(), out);
            writeAttribute("y", s.getY(), out);
            writeAttribute("width", s.getWidth(), out);
            writeAttribute("height", s.getHeight(), out);
            writeAttribute("rx",  s.getArcHeight()/2.0, out);
        } else if (shape instanceof Ellipse2D) {
            Ellipse2D s = (Ellipse2D) shape;
            double w = s.getWidth();
            double h = s.getHeight();
            boolean circle = w == h;
            out.startElement(svgTag(circle ? "circle" : "ellipse")); 
            writeAttribute("cx", s.getCenterX(), out);
            writeAttribute("cy", s.getCenterY(), out);
            if (circle)
                writeAttribute("r", w/2, out);
            else {
                writeAttribute("rx", w/2, out);
                writeAttribute("ry", h/2, out);
            }
        } else {
            out.startElement(svgTag("path")); 
            writeAttribute("d", genShapeToString(shape), out);
        }
    }
    public static void writeDrawSimple(Shape shape, Consumer out) {
        writeShapeStart(shape, out);
        writeAttribute("stroke", "black", out);
        writeAttribute("fill", "none", out);
        out.endElement();
    }
    public static void writeFillSimple(Shape shape, Consumer out) {
        writeShapeStart(shape, out);
        writeAttribute("stroke", "none", out);
        writeAttribute("fill", "black", out);
        out.endElement();
    }
    public static void writeSVGElementStart(Rectangle2D bounds, Consumer out) {
        NamespaceBinding namespaces =
            new NamespaceBinding(null, SVG_NAMESPACE_URI,
            new NamespaceBinding("xlink", XLINK_NAMESPACE_URI,
            NamespaceBinding.predefinedXML));
        out.startElement(new XName(svgTag("svg"), namespaces));
        writeAttribute("version", "1.2", out);
        double x = bounds.getX();
        double y = bounds.getY();
        double w = bounds.getWidth();
        double h = bounds.getHeight();
        writeAttribute("width", w+"px", out);
        writeAttribute("height", h+"px", out);
        writeAttribute("viewBox", x+" "+y+" "+w+" "+h, out);
    }

    private void writePaint(Paint p, boolean filling) {
        if (p instanceof Color) {
            String cname;
            if (p instanceof StandardColor)
                cname = ((StandardColor) p).getName().replace("-", "");
            else {
                Color color = (Color) p;
                int r = color.getRed();
                int g = color.getGreen();
                int b = color.getBlue();
                int alpha = color.getAlpha();
                //cname = "#%2x%2x%2x".format(r, g, b);
                cname = "#"
                    + Character.forDigit(r>>4,16) + Character.forDigit(r&15,16)
                    + Character.forDigit(g>>4,16) + Character.forDigit(g&15,16)
                    + Character.forDigit(b>>4,16) + Character.forDigit(b&15,16);
                if (alpha < 255) {
                    writeAttribute(filling ? "fill-opacity" : "stroke-opacity",
                                   alpha / 255.0, out);
                }
            }
            writeAttribute(filling ? "fill" : "stroke", cname, out);
        } else {
            // FIXME handle other types of Paint
            writeAttribute(filling ? "fill" : "stroke", "black", out);
        }
        writeAttribute(filling ? "stroke" : "fill", "none", out);
    }

    private void writeStroke(Stroke stroke, int propertiesSet) {
        if (stroke instanceof BasicStroke) {
            BasicStroke bstroke = (BasicStroke) stroke;
            if ((propertiesSet & WithPaint.STROKE_WIDTH_SET) != 0)
                writeAttribute("stroke-width", bstroke.getLineWidth(), out);
            if ((propertiesSet & WithPaint.STROKE_LINECAP_SET) != 0) {
                String str = "error-value";
                switch (bstroke.getEndCap()) {
                case BasicStroke.CAP_BUTT: str = "butt"; break;
                case BasicStroke.CAP_ROUND: str = "round"; break;
                case BasicStroke.CAP_SQUARE: str = "square"; break;
                }
                writeAttribute("stroke-linecap", str, out);
            }
            if ((propertiesSet & WithPaint.STROKE_LINEJOIN_SET) != 0) {
                String str = "error-value";
                switch (bstroke.getLineJoin()) {
                case BasicStroke.JOIN_MITER: str = "miter"; break;
                case BasicStroke.JOIN_ROUND: str = "round"; break;
                case BasicStroke.JOIN_BEVEL: str = "bevel"; break;
                }
                writeAttribute("stroke-linejoin", str, out);
            }
            if ((propertiesSet & WithPaint.STROKE_MITERLIMIT_SET) != 0)
                writeAttribute("stroke-miterlimit",  bstroke.getMiterLimit(), out);
        }
    }

    @Override
    public void visitFillShape(FillShape pic) {
        writeShapeStart(pic.shape, out);
        writePaint(paint, true);
        out.endElement();
    }
    @Override
    public void visitDrawShape(DrawShape pic) {
        writeShapeStart(pic.shape, out);
        writePaint(paint, false);
        writeStroke(stroke, strokePropertiesSet);
        out.endElement();
    }
    @Override
    public void visitWithPaint(WithPaint pic) {
        Paint savePaint = this.paint;
        Stroke saveStroke = this.stroke;
        int savePropertiesSet = strokePropertiesSet;
        if (pic.paint != null)
            this.paint = pic.paint;
        Stroke nstroke = pic.stroke;
        if (nstroke != null) {
            strokePropertiesSet |= pic.propertiesSet;
            if (saveStroke instanceof BasicStroke
                && nstroke instanceof BasicStroke
                && (pic.propertiesSet & WithPaint.STROKE_ALL_SET) != WithPaint.STROKE_ALL_SET)
                nstroke = WithPaint.merge((BasicStroke) nstroke, pic.propertiesSet,
                                          (BasicStroke) saveStroke);
            this.stroke = nstroke;
        }
        super.visitWithPaint(pic);
        this.paint = savePaint;
        this.stroke = saveStroke;
        this.strokePropertiesSet = savePropertiesSet;
    }
    @Override
    public void visitDrawImage(DrawImage image) {
        out.startElement(svgTag("image"));
        writeAttribute("width", image.getWidth(), out);
        writeAttribute("height", image.getHeight(), out);
        Path src = image.src;
        String srcstr = src == null ? null : src.toString();
        if (src != null && Path.uriSchemeSpecified(srcstr)) {
            out.startAttribute(xlinkTag("href"));
            out.write(srcstr);
            out.endAttribute();
        } else {
            try {
                ByteArrayOutputStream bout = new ByteArrayOutputStream();
                javax.imageio.ImageIO.write(image, "png", bout);
                byte[] imgbytes = bout.toByteArray();
                /* #ifdef JAVA8 */
                // String b64 = java.util.Base64.getEncoder()
                //     .withoutPadding().encodeToString(imgbytes);
                /* #else */
                String b64 = javax.xml.bind.DatatypeConverter
                    .printBase64Binary(imgbytes);
                /* #endif */
                out.startAttribute(xlinkTag("href"));
                out.write("data:image/png;base64,");
                out.write(b64);
                out.endAttribute();
            } catch (Throwable ex) {
                // ???
            }
        }
        out.endElement();
    }
    public void visitPBox(PBox pic) {
        int n = pic.children.length;
        AffineTransform tr = n <= 1 || pic.axis == 'Z' ? null
            : new AffineTransform();
        for (int i = 0; i < n; i++) {
            Picture child = pic.children[i];
            double offset = pic.translations[i];
            if (i > 0 && pic.axis != 'Z') {
                if (pic.axis == 'X')
                    tr.setToTranslation(offset, 0);
                else
                    tr.setToTranslation(0, offset);
                visitWithTransform(child, tr);
            }
            else
                child.visit(this);
        }
    }

    public static String transformAttribute(AffineTransform tr) {
        double m00 = tr.getScaleX();
        double m10 = tr.getShearY();
        double m01 = tr.getShearX();
        double m11 = tr.getScaleY();
        double m02 = tr.getTranslateX();
        double m12 = tr.getTranslateY();
        return String.format("matrix(%g %g %g %g %g %g)",
                             m00, m10, m01, m11, m02, m12);
    }
    @Override
    public void visitWithTransform(WithTransform pic) {
        visitWithTransform(pic.picture, pic.transform);
    }
    public void visitWithTransform(Picture pic, AffineTransform tr) {
        out.startElement(svgTag("g"));
        writeAttribute("transform", transformAttribute(tr), out);
        pic.visit(this);
        out.endElement();
    }
    void writeCompOp(Composite comp) {
        String op = null;
        if (comp == AlphaComposite.Clear)
            op = "clear";
        else if (comp == AlphaComposite.DstOver)
            op = "dst-over";
        else if (comp == AlphaComposite.Src)
            op = "src";
        else if (comp == AlphaComposite.SrcOver)
            op = "src-over";

        if (op != null)
            writeAttribute("comp-op", op, out);
    }

    @Override
    public void visitWithComposite(WithComposite pic) {
        out.startElement(svgTag("g"));
        Composite op = pic.singleOp();
        int n = pic.children.length;
        if (n == 0) {
        } else if (op != null) { // simple case
            writeCompOp(op);
            for (int i = 0;  i < n;  i++) {
                pic.children[i].visit(this);
            }
        } else {
            Composite prev = null;
            for (int i = 0;  i < n;  i++) {
                op = pic.composite[i];
                if (op != null && op != prev) {
                    if (prev != null)
                        out.endElement();
                    out.startElement(svgTag("g"));
                    writeCompOp(op);
                    prev = op;
                }
                pic.children[i]. visit(this);
            }
            if (prev != null)
                out.endElement();
        }
        out.endElement();
    }
}
