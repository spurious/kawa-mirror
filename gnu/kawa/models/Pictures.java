package gnu.kawa.models;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.geom.*;

public class Pictures {
    public static Picture asPicture(Object arg) {
        if (arg instanceof BufferedImage)
            return new DrawImage((BufferedImage) arg);
        if (arg instanceof Shape)
            return new DrawShape((Shape) arg);
        return (Picture) arg;
    }
    public static Picture[] asPictureAll(Object[] args) {
        int np = args.length;
        Picture[] p = new Picture[np];
        for (int i = 0; i < np; i++)
            p[i] = asPicture(args[i]);
        return p;
    }

    public static BufferedImage toImage(Picture picture) {
         Rectangle2D size = picture.getBounds2D();
         int w = (int) Math.ceil(size.getWidth());
         int h = (int) Math.ceil(size.getHeight());
         BufferedImage image =
             new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB);
         Graphics2D g2 = image.createGraphics();
         g2.setPaint(Color.BLACK);
         g2.translate(- size.getX(), -size.getY());
         picture.paint(g2);
         return image;
    }

    public static Shape borderShape(Rectangle2D bounds, boolean border,
                                    double top, double right,
                                    double bottom, double left) {
        double x0 = bounds.getX();
        double w = bounds.getWidth();
        double x1 = x0 + w;
        double y0 = bounds.getY();
        double h = bounds.getHeight();
        double y1 = y0 + h;
        if (border) {
            double x0b = x0 - left;
            double x1b = x1 + right;
            double y0b = y0 - top;
            double y1b = y1 + bottom;
            Path2D.Double r = new Path2D.Double();
            r.setWindingRule(Path2D.WIND_NON_ZERO);
            r.moveTo(x0, y0);
            r.lineTo(x1, y0);
            r.lineTo(x1, y1);
            r.lineTo(x0, y1);
            r.closePath();
            r.moveTo(x0b, y0b);
            r.lineTo(x0b, y1b);
            r.lineTo(x1b, y1b);
            r.lineTo(x1b, y0b);
            r.closePath();
            return r;
        } else
            return new Rectangle2D.Double(x0 - left, y0 - top,
                                          w + left + right, h + top + bottom);
    }
}
