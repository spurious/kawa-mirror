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
    int length = LList.listLength(pair, false)-1;
    Expression arg0 = tr.rewrite_car(pair, false);
    Object val = arg0.valueIfConstant();
    Object pairCdr = pair.getCdr();
    if (length > 0 && val instanceof Class) {
	Class clas = (Class) val;
	java.lang.reflect.TypeVariable[] vars = clas.getTypeParameters();
	if (vars.length != length)
	    return tr.syntaxError("expected "+vars.length+" parameters for type "+val);
	Type[] params = new Type[length];
	for (int i = 0;  i < length; i++) {
	    Pair pp = (Pair) pairCdr;
	    Expression arg1 = tr.rewrite_car(pp, false);
	    Type type1 = tr.getLanguage().getTypeFor(arg1, false);
	    TypeVariable tvar = TypeVariable.make(vars[i]);
	    if (type1 == null) {
		Object savedPosition = tr.pushPositionOf(pp);
		tr.error('e', "unrecognized parameter type "+pp.getCar());
		tr.popPositionOf(savedPosition);
		type1 = Type.objectType;
	    }
	    else {
		int comp = tvar.compare(type1);
		Language language = tr.getLanguage();
		if (comp < 0)
		    tr.error('e', "type parameter "+tvar.getName()+" must extend "+language.formatType(tvar.getRawType())+" which is incompatible with "+language.formatType(type1));
	    }
	    params[i] = type1;
	    pairCdr = pp.getCdr();
	}
	return new QuoteExp(new ParameterizedType((ClassType) ClassType.make(clas), params));
    }
    if (pair.getCdr() != LList.Empty)
      return tr.syntaxError ("unrecognized syntax: type[args]");
    if (val instanceof Type)
      return new QuoteExp(ArrayType.make((Type) val));
    if (val instanceof Class)
      {
        Class cls = (Class) val;
        return new QuoteExp(new ArrayType(ClassType.make(cls)).getReflectClass());
      }
    Type typ = tr.getLanguage().getTypeFor(arg0, false);
    if (typ != null)
      return new QuoteExp(ArrayType.make(typ));
    return tr.syntaxError ("[] syntax not implemented for non-constant: "+arg0+"::"+arg0.getClass().getName());
  }
}
