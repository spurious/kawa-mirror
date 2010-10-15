package gnu.kawa.functions;
import gnu.expr.*;
import gnu.math.*;
import gnu.mapping.*;
import gnu.bytecode.*;
import gnu.kawa.lispexpr.LangObjType;

public class NumberPredicate extends Procedure1 implements Inlineable
{
  public static final int ODD = 1;
  public static final int EVEN = 2;
  final int op;

  Language language;

  protected final Language getLanguage ()
  {
    return language;
  }

  public Object apply1 (Object arg1)
  {
    IntNum iarg1 = LangObjType.coerceIntNum(arg1);
    boolean result;
    switch (op)
      {
      case ODD: result = iarg1.isOdd(); break;
      case EVEN: result = ! iarg1.isOdd(); break;
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
                "gnu.kawa.functions.CompileArith:validateApplyNumberPredicate");
  }

  public int numArgs()
  {
    return 0x1001;
  }

  public void compile (ApplyExp exp, Compilation comp, Target target)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1 && (op == ODD || op == EVEN))
      {
	Expression arg0 = args[0];
        int kind = Arithmetic.classifyType(arg0.getType());
        if (kind <= Arithmetic.INTNUM_CODE)
          {
            PrimType wtype = Type.intType;
            Target wtarget = StackTarget.getInstance(wtype);
            CodeAttr code = comp.getCode();
            if (op == EVEN)
              code.emitPushInt(1);
            arg0.compile(comp, wtarget);
            code.emitPushInt(1);
            code.emitAnd();
            if (op == EVEN)
              code.emitSub(Type.intType);
            target.compileFromStack(comp, Type.booleanType);
            return;
          }
      }
   ApplyExp.compile(exp, comp, target);
  }
}
