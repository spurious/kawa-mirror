package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.mapping.*;
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
    Object name = decl.getSymbol();
    if (! (pair.cdr instanceof Pair))
      return tr.syntaxError("Missing transformation for "+form.car);
    pair = (Pair) pair.cdr;
    Macro savedMacro = tr.currentMacroDefinition;
    Macro macro = Macro.make(decl);
    tr.currentMacroDefinition = macro;
    Expression rule = tr.rewrite_car(pair, false);
    tr.currentMacroDefinition = savedMacro;
    macro.expander = rule;

    Object expander;
    if (rule instanceof QuoteExp
	&& (expander = ((QuoteExp) rule).getValue()) instanceof Procedure
	&& expander instanceof java.io.Externalizable)
      {
	macro.setExpander((Procedure) expander);
	rule = new QuoteExp(macro);
      }
    else
      {
	if (rule instanceof LambdaExp)
	  ((LambdaExp) rule).setFlag(LambdaExp.NO_FIELD);
	Expression args[] = new Expression[2];
	args[0] = new QuoteExp(name);
	args[1] = rule;
	rule = new ApplyExp(new PrimProcedure(makeMethod), args);
      }
    decl.noteValue(rule);

    if (decl.context instanceof ModuleExp)
      {
        SetExp result = new SetExp (decl, rule);
        result.setDefining (true);
        return result;
      }
    else
      return QuoteExp.voidExp;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Pair p;
    Object name;
    if (! (st.cdr instanceof Pair)
        || ! ((name = (p = (Pair) st.cdr).car) instanceof String
	      || name instanceof Symbol))
      {
        forms.addElement(tr.syntaxError("Missing macro name for "+st.car));
        return false;
      }
    if (! (p.cdr instanceof Pair) || (p = (Pair) p.cdr).cdr != LList.Empty)
      {
        forms.addElement(tr.syntaxError("invalid syntax for define-syntax"));
        return false;
      }

    Declaration decl = defs.getDefine(name, 'w', tr);
    decl.setType(typeMacro);
    p = tr.makePair(st, this, new Pair(decl, p));
    forms.addElement (p);

    tr.push(decl);
    return true;
  }
}
