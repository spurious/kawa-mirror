package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import gnu.bytecode.*;

public class module_extends extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Type base = prim_method.exp2Type(((Pair) form.cdr).car, tr);
    ModuleExp module = tr.getModule();
    module.setSuperType((ClassType) base);
    module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    return QuoteExp.voidExp;
  }
}
