package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;

public class module_implements extends Syntax
{
  public Expression rewriteForm (Pair form, Translator tr)
  {
    Object args = form.cdr;
    int len = LList.listLength(args, false);
    if (len < 0)
      return tr.syntaxError("improper argument list for " + getName());
    ClassType[] interfaces = new ClassType[len];
    for (int i = 0;  i < len;  i++)
      {
	Pair pair = (Pair) args;
	interfaces[i] = (ClassType) tr.exp2Type(pair);
	args = pair.cdr;
      }
    ModuleExp module = tr.getModule();
    module.setInterfaces(interfaces);
    module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
    return QuoteExp.voidExp;
  }
}
