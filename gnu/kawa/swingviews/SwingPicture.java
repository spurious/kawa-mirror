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

  public SwingPicture (Picture picture)
  {
    this.picture = picture;

    Rectangle2D rect = picture.getBounds2D();
    int h = (int) Math.ceil(rect.getHeight());
    int w = (int) Math.ceil(rect.getWidth());
    dim = new Dimension(w, h);
  }

  public void paint(Graphics g)
  {
    // FIXME may need to transform position
    picture.paint((Graphics2D) g);
  }

  public java.awt.Dimension getPreferredSize ()
  {
    return dim;
  }
}
