package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.lists.*;

public class define_syntax extends Syntax
{
  static ClassType typeMacro = ClassType.make("kawa.lang.Macro");
  static Method makeMethod = typeMacro.getDeclaredMethod("make", 2);
  static Method setExpanderMethod
    = typeMacro.getDeclaredMethod("setExpander", 1);

  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair pair;
    Declaration decl;
    try
      {
        pair = (Pair) form.cdr;
        decl = (Declaration) pair.car;
      }
    catch (Exception ex)
      {
        return tr.syntaxError(getName()+" not in a statement list");
      }
    String name = decl.getName();
    Macro macro = (Macro) decl.getConstantValue();
    if (! (pair.cdr instanceof Pair))
      return tr.syntaxError("Missing transformation for "+form.car);
    pair = (Pair) pair.cdr;
    Macro savedMacro = tr.currentMacroDefinition;
    tr.currentMacroDefinition = macro;
    Expression rule = tr.rewrite(pair.car);
    tr.currentMacroDefinition = savedMacro;
    macro.expander = rule;
    if (! (decl.context instanceof ModuleExp))
      {
	return QuoteExp.voidExp;
      }
    else
      {
	if (! (rule instanceof QuoteExp)
	    || ! (((QuoteExp) rule).getValue() instanceof java.io.Externalizable))
	  {
	    Expression args[] = new Expression[2];
	    args[0] = new QuoteExp(name);
	    args[1] = rule;
	    rule = new ApplyExp(new PrimProcedure(makeMethod), args);
	  }
	else
	  rule = new QuoteExp(macro);
        SetExp result = new SetExp (decl, rule);
        result.setDefining (true);
        return result;
      }
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p;
    if (! (st.cdr instanceof Pair)
        || ! ((p = (Pair) st.cdr).car instanceof String))
      {
        forms.addElement(tr.syntaxError("Missing macro name for "+st.car));
        return false;
      }
    String name = (String) p.car;
    if (! (p.cdr instanceof Pair) || (p = (Pair) p.cdr).cdr != LList.Empty)
      {
        forms.addElement(tr.syntaxError("invalid syntax for define-syntax"));
        return false;
      }

    Declaration decl = defs.getDefine(name, 'w', tr);
    Macro macro = Macro.make(decl);

    p = tr.makePair(st, this, new Pair(decl, p));
    forms.addElement (p);

    tr.pushBinding(name, decl);
    return true;
  }
}
