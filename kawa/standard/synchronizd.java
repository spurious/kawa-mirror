package kawa.standard;
import kawa.lang.*;
import gnu.lists.*;
import gnu.expr.*;

public class synchronizd extends Syntax
{
  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError("missing argument in synchronized");
    Pair pair = (Pair) obj;
    Expression object = tr.rewrite(pair.car);
    Expression body = tr.rewrite_body(pair.cdr);
    return new SynchronizedExp (object, body);
  }
}

