package gnu.kawa.lispexpr;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.lists.*;
import kawa.lang.*;

/** Expand {@code $bracket-apply$} macro.
 * This is produced by the reader syntax {@code exp[args]}.
 * Currently, it us only use for array types: {@code type[]}.
 * In the future it might have other uses - perhaps parameterized tyoes,
 * as in Scala.
 */

public class BracketApply extends Syntax
{
  public static final BracketApply instance = new BracketApply();

  public Expression rewrite (Object obj, Translator tr)
  {
    if (! (obj instanceof Pair))
      return tr.syntaxError ("internal error - $bracket-apply$ missing functor");
    Pair pair = (Pair) obj;
    if (pair.getCdr() != LList.Empty)
      return tr.syntaxError ("unrecognized syntax: type[args]");
    Expression arg = tr.rewrite(pair.getCar());
    Object val = arg.valueIfConstant();
    if (val instanceof Type)
      return new QuoteExp(ArrayType.make((Type) val));
    if (val instanceof Class)
      {
        Class cls = (Class) val;
        return new QuoteExp(new ArrayType(ClassType.make(cls)).getReflectClass());
      }
    Type typ = tr.getLanguage().getTypeFor(arg, false);
    if (typ != null)
      return new QuoteExp(ArrayType.make(typ));
    return tr.syntaxError ("[] syntax not implemented for non-constant: "+arg+"::"+arg.getClass().getName());
  }
}
