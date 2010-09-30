package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.lists.*;
import gnu.bytecode.*;

public class module_implements extends Syntax
{
  public static final module_implements module_implements
    = new module_implements();
  static { module_implements.setName("module-implements"); }

  public void scanForm (Pair form, ScopeExp defs, Translator tr)
  {
    Object args = form.getCdr();
    int len = LList.listLength(args, false);
    if (len < 0)
      {
        tr.syntaxError("improper argument list for " + getName());
        return;
      }
    ClassType[] interfaces = new ClassType[len];
    for (int i = 0;  i < len;  i++)
      {
	Pair pair = (Pair) args;
	interfaces[i] = (ClassType) tr.exp2Type(pair);
	args = pair.getCdr();
      }
    ModuleExp module = tr.getModule();
    module.setInterfaces(interfaces);
    module.setFlag(ModuleExp.SUPERTYPE_SPECIFIED);
  }
}
