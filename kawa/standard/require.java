package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;

public class require extends Syntax
{
  public static Object allocate(ClassType type, Environment env)
  {
    String mangledName = "required:"+type.getName();
    Object value = env.get(mangledName);
    if (value == null)
      {
        try
          {
            value = type.getReflectClass().newInstance();
          }
        catch (Exception ex)
          {
            throw new WrappedException(ex);
          }
        if (value instanceof Runnable)
          ((Runnable) value).run();
        env.define(mangledName, value);
      }
    return value;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object name = ((Pair) st.cdr).car;
    // Type type = Scheme.expType(tr.rewrite(name));
    Type type = prim_method.exp2Type(name, tr);
    System.err.println("require "+type);
    Object instance = null;
    ClassType t = (ClassType) type;
    for (;;)
      {
        for (Field fld = t.getFields();  fld != null;  fld = fld.getNext())
          {
            int flags = fld.getFlags();
            if ((flags & Access.PUBLIC) == 0)
              continue;
            if ((flags & Access.STATIC) == 0 && instance == null)
              {
                instance = allocate((ClassType) type, tr.environ);
              }
            String fname = fld.getName(); // FIXME:unmangle name.
            System.err.println("field "+fld.getName());
            ClassMemberConstraint.define(fname, instance, fld, tr.environ);
          }
        t = t.getSuperclass();
        if (t == null)
          break;
      }
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
