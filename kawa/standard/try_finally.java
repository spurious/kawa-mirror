package kawa.standard;
import kawa.lang.*;
import gnu.kawa.util.*;
import gnu.mapping.*;
import gnu.expr.*;

/**
 * The Syntax transformer that re-writes "try-finally".
 * (try-finally try-clause finally-clause)
 * @author	Per Bothner
 */

public class try_finally extends Syntax implements Printable
{
  static private Pattern pattern = new ListPat (2);

  public Expression rewrite (Object obj, Translator tr)
  {
    Object [] match = pattern.match (obj);
    if (match == null)
      return tr.syntaxError ("invalid syntax for try-finally");
    return new TryExp (tr.rewrite(match[0]), tr.rewrite(match[1]));
  }
}

