package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.Method;

public class define_syntax extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Pair pair;
    if (! (form.cdr instanceof Pair)
        || ! ((pair = (Pair) form.cdr).car instanceof String
              || pair.car instanceof Declaration))
      return tr.syntaxError("Missing macro name for "+form.car);
    String name;
    Macro macro;
    if (pair.car instanceof String)
      {
        name = (String) pair.car;
        macro = null;
      }
    else
      {
        macro = (Macro) pair.car;
        name = macro.getName();
      }
    if (! (pair.cdr instanceof Pair))
      return tr.syntaxError("Missing transformation for "+form.car);
    pair = (Pair) pair.cdr;
    Expression rule = tr.rewrite(pair.car);
    if (macro != null)
      {
        macro.noteValue(rule);
        return QuoteExp.voidExp;
      }
    else
      {
        // Add rule to translation environment.
        macro = new Macro(name, rule);
        //macro.noteValue(rule);
        tr.addGlobal(name, macro);

        // Add rule to execution environment.
        Expression args[] = new Expression[2];
        args[0] = new QuoteExp(name);
        args[1] = rule;
        Method makeMacro = Macro.getMakeMethod();
        SetExp result
          = new SetExp (name,
                        new ApplyExp(new QuoteExp(new PrimProcedure(makeMacro)),
                                     args));
        result.setDefining (true);
        return result;
      }
  }
}
