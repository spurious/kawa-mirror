package kawa.lang;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.bytecode.*;

public class DefMacro extends Macro
{
  Expression lexp;
  Procedure expander;

  public static ClassType thisType;
  static public Method makeMethod;

  public static Method getMakeMethod()
  {
    if (thisType == null)
      thisType = ClassType.make("kawa.lang.DefMacro");
    if (makeMethod == null)
      {
        Type[] args = { Compilation.javaStringType, Compilation.typeProcedure };
        makeMethod = thisType.addMethod("make", args,
                                        thisType, Access.STATIC|Access.PUBLIC);
      }
    return makeMethod;
  }

  public DefMacro (Procedure expander)
  {
    this.expander = expander;
  }

  /*
  public DefMacro (String name, Procedure expander)
  {
    String sym = Symbol.make(name);
    this.setName(sym);
    expander.setName(sym);
    this.expander = expander;
    Environment.define_global(sym, this);
  }
  */

  public static DefMacro make (String name, Procedure expander)
  {
    DefMacro mac = new DefMacro(expander);
    mac.setName(name);
    return mac;
  }

  public DefMacro (Expression lexp)
  {
    this.lexp = lexp;
  }

  public String toString()
  {
    return "#<macro "+getName()+'>';
  }

  public Object expand (Pair form, Translator tr)
  {
    if (expander == null)
      expander = (Procedure) lexp.eval(Environment.current());

    /* DEBUGGING:
    OutPort err = OutPort.errDefault();
    err.print("{Before defmacro expansion: ");
    SFormat.print (form, err);
    err.println ('}');
    */
    try
      {
        SyntaxForm sform = new SyntaxForm();
        sform.form = form;
        sform.tr = tr;
	Object expansion = expander.apply1(sform) ;
	/* DEBUGGING:
	err.print("{Expanded macro: ");
	SFormat.print (expansion, err);
	err.println ('}');
	*/
	return expansion;
      }
    catch (Exception ex)
      {
	return tr.syntaxError("caught exception ("+ex+") while expanding macro "+ name());
      }
  }
}

