package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;

public class module_extends extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Type base = tr.exp2Type((Pair) form.cdr);
    ModuleExp module = tr.getModule();
    module.setSuperType((ClassType) base);
    module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    return QuoteExp.voidExp;
  }
}
