package gnu.text;
import java.text.FieldPosition;
import java.io.Writer;

/* A Format that does nothing except flush the destination stream. */

public class FlushFormat extends ReportFormat
{
  private static FlushFormat flush;

  public static FlushFormat getInstance()
  {
    if (flush == null)
      flush = new FlushFormat();
    return flush;
  }

  public int format(Object[] args, int start, Writer dst, FieldPosition fpos)
    throws java.io.IOException
  {
    dst.flush();
    return start;
  }
}
