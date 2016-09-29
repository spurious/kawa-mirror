package gnu.kawa.swingviews;
import java.awt.*;
import java.awt.geom.*;
import gnu.kawa.models.*;
import javax.swing.*;

/** Embeds a Picture object in a JPanel,. */

public class SwingPicture extends JPanel
{
    Picture picture;
    Dimension dim;
    Rectangle2D rect;

    public SwingPicture(Picture picture) {
        setPicture(picture);
    }

    public Picture getPicture() { return picture; }

    public void setPicture(Object picture) {
        setPicture(Pictures.asPicture(picture));
    }

    public void setPicture(Picture picture) {
        this.picture = picture;

        Rectangle2D rect = picture.getBounds2D();
        this.rect = rect;
        int h = (int) Math.ceil(rect.getHeight());
        int w = (int) Math.ceil(rect.getWidth());
        dim = new Dimension(w, h);
        if (! isPreferredSizeSet())
            setPreferredSize(dim);
        repaint(0, 0, 0, getWidth(), getHeight());
    }

    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        AffineTransform saveTransform = g2.getTransform();
        try {
            g2.translate((getWidth() - rect.getWidth()) * 0.5 - rect.getX(),
                         (getHeight() - rect.getHeight()) * 0.5 - rect.getY());
            picture.paint(g2);
        } finally {
            g2.setTransform(saveTransform);
        }
    }
}
