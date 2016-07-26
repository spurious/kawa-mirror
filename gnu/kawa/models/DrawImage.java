package gnu.kawa.models;
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.util.Vector;
import gnu.kawa.io.Path;
import gnu.mapping.WrappedException;
import java.net.URL;

public class DrawImage extends Model
    implements Picture, java.io.Serializable, RenderedImage
{
  BufferedImage image;
  Path src;
  String description;

  public DrawImage ()
  {
  }

  public void makeView (Display display, Object where)
  {
    display.addImage(this, where);
  }

  RenderedImage loadImage ()
  {
    if (image == null)
      {
        try
          {
            image = javax.imageio.ImageIO.read(src.openInputStream());
          }
        catch (Exception ex)
          {
            throw WrappedException.wrapIfNeeded(ex);
          }
      }
    return image;
  }

  public DrawImage (BufferedImage image)
  {
    this.image = image;
  }

  public void paint (Graphics2D graphics)
  {
    loadImage();
    //graphics.drawRenderedImage(image, WithTransform.identityTransform);
    graphics.drawRenderedImage(this, WithTransform.identityTransform);
  }

  public Rectangle2D getBounds2D()
  {
    loadImage();
    int w = image.getWidth();
    int h = image.getHeight();
    return new Rectangle2D.Float(0, 0, w, h);
  }

  public Picture transform (AffineTransform tr)
  {
    return new WithTransform(this, tr);
  }

  public BufferedImage getImage ()
  {
    loadImage();
    return image;
  }

  public Path getSrc () { return src; }

  public void setSrc (Path src)
  {
    this.src = src;
  }

    @Override
    public WritableRaster copyData(WritableRaster raster) {
        return getImage().copyData(raster);
    }
    @Override
    public ColorModel getColorModel() {
        return getImage().getColorModel();
    }
    @Override
    public Raster getData() {
        return getImage().getData();
    }
    @Override
    public Raster getData(Rectangle rect) {
        return getImage().getData(rect);
    }
    @Override
    public int getHeight() {
        return getImage().getHeight();
    }
    @Override
    public int getMinTileX() {
        return getImage().getMinTileX();
    }
    @Override
    public int getMinTileY() {
        return getImage().getMinTileX();
    }
    @Override
    public int getMinX() {
        return getImage().getMinX();
    }
    @Override
    public int getMinY() {
        return getImage().getMinY();
    }
    @Override
    public int getNumXTiles() {
        return getImage().getNumXTiles();
    }
    @Override
    public int getNumYTiles() {
        return getImage().getNumYTiles();
    }
    @Override
    public Raster getTile(int tileX, int tileY) {
        return getImage().getTile(tileX, tileY);
    }
    @Override
    public int getTileGridXOffset() {
        return getImage().getTileGridXOffset();
    }
    @Override
    public int getTileGridYOffset() {
        return getImage().getTileGridYOffset();
    }
    @Override
    public int getTileHeight() {
        return getImage().getTileHeight();
    }
    @Override
    public int getTileWidth() {
        return getImage().getTileWidth();
    }
    @Override
    public SampleModel getSampleModel() {
        return getImage().getSampleModel();
    }
    @Override
    public int getWidth() {
        return getImage().getWidth();
    }
    @Override
    public Vector<RenderedImage> getSources() {
        return getImage().getSources();
    }
    @Override
    public Object getProperty(String name) {
        if (src != null) {
            if ("SRC_LINK".equals(name)) {
                System.err.println("request src_link for "+this+" src:"+src);
                return src.toString();
            }
            if ("SRC_PATH".equals(name))
                return src;
        }
        return getImage().getProperty(name);
    }
    @Override
    public String[] getPropertyNames() {
        String[] names = getImage().getPropertyNames();
        if (src != null) {
            int nlen = names == null ? 0 : names.length;
            String[] tmp = new String[nlen+2];
            tmp[0] = "SRC_LINK";
            tmp[1] = "SRC_PATH";
            if (nlen > 0)
                System.arraycopy(names, 0, src, 2, nlen);
            names = tmp;
        }
        return names;
    }
    public void visit(PictureVisitor visitor) {
        visitor.visitDrawImage(this);
    }
}
