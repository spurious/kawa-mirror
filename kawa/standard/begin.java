package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * Implement the re-writer for the "begin" primitive.
 * @author	Per Bothner
 */

public class begin extends Syntax implements Printable
{
  public Expression rewrite (Object obj, Translator tr)
  {
    return tr.rewrite_body (obj);
  }
}
