package gnu.kawa.functions;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.mapping.Procedure;

/** Implements Kawa extension function "setter", as in SRFI-17. */

public class Setter extends Procedure1 // implements CanInline
{
  public static final Setter setter = new Setter();
  static { setter.setName("setter"); }

  public static Object setter (Procedure arg)
  {
    return arg.getSetter();
  }

  public Object apply1 (Object arg)
  {
    return ((Procedure)arg).getSetter();
  }

  /*
  public Expression inline (ApplyExp exp)
  {
    Expression[] args = exp.getArgs();
    if (args.length == 1)
      {
        Expression arg = args[0];
        if (arg instanceof ReferenceExp)
          {
            Declaration decl = ((ReferenceExp) arg).getBinding();
            if (decl != null)
              {
                Expression dvalue = decl.getValue();
                if (dvalue instanceof QuoteExp)
                  {
                    Object value = ((QuoteExp) dvalue).getValue();
                    if (value instanceof Procedure)
                      {
                        Object setter = ((Procedure) value).getSetter();
                        if (setter instanceof Procedure)
                          {
                            decl
                              = Declaration.getDeclaration((Procedure) setter);
                            if (decl != null)
                              return new ReferenceExp(decl);
                          }
                      }
                  }
              }
          }
      }
    return exp;
  }
  */
}
