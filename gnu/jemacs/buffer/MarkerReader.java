package gnu.jemacs.buffer;
import javax.swing.text.*;
import gnu.mapping.*;
import java.io.*;

public class MarkerReader extends InPort
{
  Marker marker;
  BufferContent content;

  public MarkerReader(Buffer buffer)
  {
    this(buffer, buffer.pointMarker);
  }

  public MarkerReader(Marker marker)
  {
    this(marker.getBuffer(), marker);
  }

  MarkerReader(Buffer buffer, Marker marker)
  {
    super(gnu.text.NullReader.nullReader, buffer.getName());
    this.marker = marker;
    content = ((gnu.jemacs.swing.SwingBuffer) buffer).content;
    this.buffer = content.getArray();
  }

  public int read()
  {
    int offset = marker.getOffset();
    buffer = content.getArray();
    if (offset >= content.gapStart)
      offset += content.gapEnd - content.gapStart;
    int length = buffer.length;
    pos = offset;
    if (offset >= length)
      {
	limit = offset;
	return -1;
      }
    else
      {
	int ch = buffer[pos];
	pos++;
	limit = pos;
	return ch;
      }
  }

  public int getLineNumber ()
  {
    throw new Error("MarkerReader. getLineNumber - not implemented");
  }

  public int getColumnNumber ()
  {
    throw new Error("MarkerReader,getColumnNumber - not implemented");
  }

  public void reset ()  throws IOException
  {
    throw new Error("MarkerReader.reset - not implemented");
  }

}

