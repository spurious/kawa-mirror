// Copyright (c) 2001  Per M.A. Bothner and Brainfood Inc.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.xml;
import gnu.mapping.*;
import gnu.expr.Interpreter;

/** Helper Environment used by Scheme to resolve "NAMESPACE:NAME". */ 

public class NamespaceEnv extends Environment
{
  Environment mainEnv;

  public NamespaceEnv (Environment mainEnv)
  {
    this.mainEnv = mainEnv;
  }

  public Object get (String name, Object defaultValue)
  {
    int i = name.indexOf(':');
    if (i >= 0)
      {
	if (i == 0)
	  {
	    name = name.substring(1).intern();
	    return ElementConstructor.make(name, null, name);
	  }
	String prefix = name.substring(0, i);
	Object nsValue
	  = mainEnv.get((Interpreter.NAMESPACE_PREFIX+prefix).intern(), null);
	if (nsValue != null)
	  {
	    String localName = name.substring(i+1);
	    return ElementConstructor.make(name, nsValue.toString().intern(),
					   localName);
	  }
      }
    return defaultValue;
  }
}
