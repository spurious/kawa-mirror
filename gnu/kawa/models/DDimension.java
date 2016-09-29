package gnu.kawa.models;

import java.awt.Dimension;
import java.awt.geom.Dimension2D;

/** A subclass of Dimension that uses doubles.
 * This is compatible with code that expects a Dimension (which uses int),
 * but can also be used where fractional lengths are needed.
 * Avoid setting the inherited public {@code width} and {@code height} fields
 * directly, as that can lead to inconsistencies.
 */

public class DDimension extends Dimension
{
    private double w;
    private double h;

    public DDimension() {
    }
    public DDimension(double width, double height) {
        setWidth(width);
        setHeight(height);
    }
    public DDimension(Dimension2D d) {
        this(d.getWidth(), d.getHeight());
    }

    @Override
    public double getWidth() { return w; }

    @Override
    public double getHeight() { return h; }

    @Override
    public DDimension getSize() {
        return new DDimension(w, h);
    }

    public void setWidth(double width) {
        this.w = width;
        super.width = (int) Math.ceil(width);
     }

    public void setHeight(double height) {
        this.h = height;
        super.height = (int) Math.ceil(height);
    }

    @Override
    public void setSize(int width, int height) {
        this.w = width;
        super.width = width;
        this.h = height;
        super.height = height;
    }
 
    @Override
    public void setSize(double width, double height) {
        setWidth(width);
        setHeight(height);
    }

    @Override
    public void setSize(Dimension d) {
        setSize(d.getWidth(), d.getHeight());
    }

    public void setSize(Dimension2D d) {
        setSize(d.getWidth(), d.getHeight());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof DDimension) {
            DDimension d = (DDimension)obj;
            return (w == d.w) && (h == d.h);

        }
        return false;
    }
 
    @Override
    public String toString() {
        return getClass().getName() + "[width=" + getWidth()
            + ",height=" + getHeight() + "]";
    }
}
