package gnu.kawa.functions;
import gnu.expr.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangObjType;

public class NumberPredicate extends Procedure1
{
  public static final int ODD = 1;
  public static final int EVEN = 2;
  final int op;

  Language language;

  protected final Language getLanguage ()
  {
    return language;
  }

    public static boolean isOdd(Object obj) {
        return isOddEven(true, obj);
    }

    public static boolean isEven(Object obj) {
        return isOddEven(false, obj);
    }

    public static boolean isOddEven(boolean isOdd, Object obj) {
        obj = Promise.force(obj);
        IntNum iarg = IntNum.asIntNumOrNull(obj);
        if (iarg == null && obj instanceof Number
            && (obj instanceof RealNum || ! (obj instanceof Numeric))) {
            double r = Math.IEEEremainder(((Number) obj).doubleValue(), 2.0);
            if (r == 0.0)
                return !isOdd;
            else if (r == -1.0 || r == 1.0)
                return isOdd;
        }
        if (iarg == null)
            throw new WrongType(WrongType.ARG_CAST, obj,
                                LangObjType.integerType);
        boolean result;
        return iarg.isOdd() == isOdd;
    }

    public Object apply1(Object arg1) {
        boolean result;
        switch (op) {
        case ODD: result = isOddEven(true,arg1); break;
        case EVEN: result = isOddEven(false, arg1); break;
        default: throw new Error();
        }
        return getLanguage().booleanObject(result);
    }

  public NumberPredicate (Language language, String name, int op)
  {
    super(name);
    this.language = language;
    this.op = op;
    setProperty(Procedure.validateApplyKey,
                "gnu.kawa.functions.CompileMisc:validateApplySimpleBoolean");
    setProperty(Procedure.compilerXKey,
                "gnu.kawa.functions.CompileMisc:compileNumPredicate");
  }

  public int numArgs()
  {
    return 0x1001;
  }

}
