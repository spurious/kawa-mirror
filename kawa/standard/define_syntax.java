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
        || ! ((pair = (Pair) form.cdr).car instanceof String))
      return tr.syntaxError("Missing macro name for "+form.car);
    String name = (String) pair.car;
    if (! (pair.cdr instanceof Pair))
      return tr.syntaxError("Missing transformation for "+form.car);
    pair = (Pair) pair.cdr;
    Expression rule = tr.rewrite(pair.car);

    // Add rule to translation environment.
    tr.addGlobal(name, new DefMacro(rule));
    // Add rule to execution environment.
    Expression args[] = new Expression[2];
    args[0] = new QuoteExp(name);
    args[1] = rule;
    Method makeDefMacro = DefMacro.getMakeMethod();
    SetExp result
      = new SetExp (name,
                    new ApplyExp(new QuoteExp(new PrimProcedure(makeDefMacro)),
                                 args));
    result.setDefining (true);
    return result;
  }
}
