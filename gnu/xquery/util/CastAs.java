package gnu.xquery.util;
import gnu.kawa.functions.Convert;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.xml.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.reflect.OccurrenceType;
import gnu.mapping.Values;

public class CastAs extends Convert
{
  public static final CastAs castAs = new CastAs();

  public Object apply2 (Object arg1, Object arg2)
  {
    Type type = (Type) arg1;
    if (type instanceof XDataType)
      return ((XDataType) type).cast(arg2);
    if (type instanceof OccurrenceType)
      {
        OccurrenceType occ = (OccurrenceType) type;
        Type base = occ.getBase();
        if (base instanceof XDataType)
          {
            int min = occ.minOccurs();
            int max = occ.maxOccurs();
            if (arg2 instanceof Values)
              {
                if (arg2 == Values.empty && min == 0)
                  return arg2;
                Values vals = (Values) arg2;
                int pos = vals.startPos();
                int n = 0;
                Values result = new Values();
                for (;;)
                  {
                    pos = vals.nextPos(pos);
                    if (pos == 0)
                      {
                        if (n >= min && (max < 0 || n <= max))
                          return result.canonicalize();
                        break;
                      }
                    Object value = vals.getPosPrevious(pos);
                    value = ((XDataType) base).cast(value);
                    result.writeObject(value);
                    n++;
                  }
              }
            else
              {
                if (min <= 1 && max != 0)
                  return ((XDataType) base).cast(arg2);
              }
            throw new ClassCastException("cannot cast "+arg2
                                         +" to "+arg1);
          }
      }
    return super.apply2(arg1, arg2);
  }

  static final ClassType typeXDataType =
    ClassType.make("gnu.kawa.xml.XDataType");
  static final Method castMethod = typeXDataType.getDeclaredMethod("cast", 1);

  public Expression inline (ApplyExp exp, InlineCalls walker)
  {
    exp.walkArgs(walker);
    exp = Invoke.inlineClassName(exp, 0, walker);
    Expression[] args = exp.getArgs();
    if (args.length != 2 || ! (args[0] instanceof QuoteExp))
      return exp;
    Object type = ((QuoteExp) args[0]).getValue();
    if (type instanceof XDataType)
      {
        XDataType xtype = (XDataType) type;
        return new ApplyExp(castMethod, args);
      }
    return exp;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    ApplyExp.compile(exp, comp, target);
  }
}
