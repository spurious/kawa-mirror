package kawa;
import java.awt.Component;
import javax.swing.*;
import javax.swing.text.*;
import gnu.lists.Consumer;
import gnu.mapping.*;
import gnu.kawa.format.AbstractFormat;
import gnu.kawa.format.GenericFormat;
import gnu.kawa.format.GenericFormat.TryFormatResult;
import gnu.kawa.functions.DisplayFormat;
import gnu.kawa.io.OutPort;
import gnu.kawa.io.Path;
import gnu.kawa.models.DrawImage;
import gnu.kawa.models.Picture;
import gnu.kawa.models.Viewable;
import gnu.kawa.swingviews.SwingDisplay;

/** A Writer that appends its output to a ReplPane.
  * Based on code from Albert L. Ting" alt@artisan.com.
  */

public class ReplPaneOutPort extends OutPort
{
    static {
        Class thisCls = ReplPaneOutPort.class;
        GenericFormat format = DisplayFormat.standardFormat;
        format.invalidateCache(java.awt.image.BufferedImage.class);
        format.invalidateCache(java.awt.Shape.class);
        format.invalidateCache(gnu.kawa.models.Picture.class);
        format.invalidateCache(gnu.kawa.models.Viewable.class);
        format.invalidateCache(java.awt.Component.class);
        format.add(thisCls, "writePicture");
        format.add(thisCls, "writeComponent");
        format.add(thisCls, "writeViewable");
    }

  ReplDocument document;
  AttributeSet style;
  String str="";
  TextPaneWriter tout;

  public ReplPaneOutPort (ReplDocument document, String path, AttributeSet style)
  {
    this(new TextPaneWriter(document, style), document, path, style);
  }

  ReplPaneOutPort (TextPaneWriter tout, ReplDocument document, String path, AttributeSet style)
  {
    super(tout, true, true, Path.valueOf(path));
    this.tout = tout;
    this.document = document;
    this.style = style;
  }

  public void write (String str, MutableAttributeSet style)
  {
    flush();
    document.write(str, style);
    setColumnNumber(1); // So freshline will Do The Right Thing.
  }

  public synchronized void write (Component c)
  {
    MutableAttributeSet style = new SimpleAttributeSet();
    StyleConstants.setComponent(style, c);
    write(" ", style);
  }

    public static TryFormatResult
            writeComponent(Object value, AbstractFormat format, Consumer out)  {
        if (! (value instanceof Component))
            return TryFormatResult.INVALID_CLASS;
        if (format.getReadableOutput() || ! (out instanceof ReplPaneOutPort))
            return TryFormatResult.INVALID;
        ((ReplPaneOutPort) out).write((Component) value);
        return TryFormatResult.HANDLED;
    }

    public static TryFormatResult
            writeViewable(Object value, AbstractFormat format, Consumer out)  {
        if (! (value instanceof Viewable))
            return TryFormatResult.INVALID_CLASS;
        if (format.getReadableOutput() || ! (out instanceof ReplPaneOutPort))
            return TryFormatResult.INVALID;
        MutableAttributeSet style = new SimpleAttributeSet();
        style.addAttribute(AbstractDocument.ElementNameAttribute, ReplPane.ViewableElementName);
        style.addAttribute(ReplPane.ViewableAttribute, value);
        ((ReplPaneOutPort) out).write(" ", style);
        return TryFormatResult.HANDLED;
    }

    public static TryFormatResult
            writePicture(Object value, AbstractFormat format, Consumer out)  {
        Picture pic = DrawImage.toPictureOrNull(value);
        if (pic == null)
            return TryFormatResult.INVALID_CLASS;
        if (format.getReadableOutput() || ! (out instanceof ReplPaneOutPort))
            return TryFormatResult.INVALID;
        MutableAttributeSet style = new SimpleAttributeSet();
        style.addAttribute(AbstractDocument.ElementNameAttribute, ReplPane.PictureElementName);
        style.addAttribute(ReplPane.PictureAttribute, pic);
        ((ReplPaneOutPort) out).write(" ", style);
        return TryFormatResult.HANDLED;
    }
}

class TextPaneWriter extends java.io.Writer
{
  ReplDocument document;
  AttributeSet style;
  String str="";

  public TextPaneWriter (ReplDocument document, AttributeSet style)
  {
    this.document = document;
    this.style = style;
  }

  public synchronized void write (int x)
  {
    str = str + (char) x;
    if (x == '\n')
      flush();
  }

  public void write (String str)
  {
    document.write(str, style);
  }

  public synchronized void write (char[] data, int off, int len)
  {
    flush();
    if (len != 0)
      write(new String(data, off, len));
  }

  public synchronized void flush()
  {
    String s = str;
    if (! s.equals(""))
      {
        str = "";
	write(s);
      }
  }

  public void close ()
  {
    flush();
  }
}
