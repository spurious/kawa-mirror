package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.expr.*;
import gnu.kawa.util.*;
import gnu.bytecode.*;

public class module_implements extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object args = form.cdr;
    int len = LList.list_length(args);
    if (len < 0)
      return tr.syntaxError("improper argument list for " + getName());
    ClassType[] interfaces = new ClassType[len];
    for (int i = 0;  i < len;  i++)
      {
	Pair pair = (Pair) args;
	interfaces[i] = (ClassType) prim_method.exp2Type(pair.car, tr);
	args = pair.cdr;
      }
    ModuleExp module = tr.getModule();
    module.setInterfaces(interfaces);
    return QuoteExp.voidExp;
  }
}
