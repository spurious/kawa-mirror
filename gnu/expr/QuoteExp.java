package gnu.expr;
import gnu.mapping.*;

/**
 * An Expression that evaluates to a constant value.
 * @author	Per Bothner
 */

public class QuoteExp extends Expression
{
  Object value;

  public final Object getValue() { return value; }

  public final gnu.bytecode.Type getType()
  {
    if (value == Values.empty)
      return gnu.bytecode.Type.void_type;
    if (value == null)
      return gnu.bytecode.Type.nullType;
    return gnu.bytecode.Type.make(value.getClass());
  }

  static public QuoteExp undefined_exp
  = new QuoteExp (Undefined.getInstance());
  static public QuoteExp voidExp = new QuoteExp (Values.empty);
  static public QuoteExp trueExp = new QuoteExp(Boolean.TRUE);
  static public QuoteExp falseExp = new QuoteExp(Boolean.FALSE);
  static public QuoteExp nullExp = new QuoteExp(null);

  public static QuoteExp getInstance (Object value)
  {
    if (value == null)
      return nullExp;
    if (value == Undefined.getInstance())
      return undefined_exp;
    if (value == Values.empty)
      return voidExp;
    if (value instanceof Boolean)
      return ((Boolean) value).booleanValue() ? trueExp : falseExp;
    return new QuoteExp(value);
  }

  public QuoteExp (Object val) { value = val; }
  
  public Object eval (Environment env)
  {
    return value;
  }

  public void compile (Compilation comp, Target target)
  {
    comp.compileConstant(value, target);
  }
 
  protected Expression walk (ExpWalker walker)
  {
    return walker.walkQuoteExp(this);
  }

  public void print (OutPort out)
  {
    out.startLogicalBlock("(Quote", ")", 2);
    out.writeSpaceLinear();
    if (value instanceof Expression)
      value = value.toString(); // To avoid cycles.
    gnu.lists.FormatToConsumer saveFormat = out.objectFormat;
    try
      {
	out.objectFormat = Language.getDefaultLanguage().getFormat(true);
	out.print(value);
      }
    finally
      {
	out.objectFormat = saveFormat;
      }
    out.endLogicalBlock(")");
  }
}
