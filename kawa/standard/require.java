package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.kawa.util.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.reflect.Invoke;

public class require extends Syntax
{
  public static Object find(String typeName)
  {
    try
      {
	return find(Class.forName(typeName), Environment.getCurrent()); 
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	throw new WrappedException("canot find module "+typeName, ex);
      }
  }

  public static Object find(ClassType type, Environment env)
  {
    return find(type.getReflectClass(), env);
  }

  public static Object find(Class ctype, Environment env)
  {
    String mangledName = ctype.getName() + "#instance";
    Binding binding = env.getBinding(mangledName);
    Object value;
    synchronized (binding)
      {
	if (binding.isBound())
	  return binding.get();
	try
	  {
	    value = ctype.newInstance();
	    binding.set(value);
	  }
        catch (Exception ex)
          {
            throw new WrappedException(ex);
          }
      }
    if (value instanceof Runnable)
      ((Runnable) value).run();
    else if (value instanceof ModuleBody)
      ((ModuleBody) value).run();
    return value;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    boolean immediate = tr.immediate && defs instanceof ModuleExp;
    Object name = ((Pair) st.cdr).car;
    // Type type = Scheme.expType(tr.rewrite(name));
    Type type;
    Pair p;
    if (name instanceof Pair && (p = (Pair) name).car == "quote")
      {
	name = p.cdr;
	if (! (name instanceof Pair)
	    || (p = (Pair) name).cdr != LList.Empty
	    || ! (p.car instanceof String))
	  {
	    tr.error('e', "invalid quoted symbol for `require'");
	    return false;
	  }
	name = gnu.kawa.slib.SLib.mapFeature((String) p.car);
	if (name == null)
	  {
	    tr.error('e', "unknown feature name `"+p.car+"' for `require'");
	    return false;
	  }
	type = ClassType.make((String) name);
      }
    else
      type = prim_method.exp2Type(name, tr);
    String tname = type.getName();
    Object instance = null;
    ClassType t = (ClassType) type;
    Declaration decl = null;
    for (;;)
      {
	Class rclass = t.getReflectClass();
        for (Field fld = t.getFields();  fld != null;  fld = fld.getNext())
          {
            int flags = fld.getFlags();
            if ((flags & Access.PUBLIC) == 0)
              continue;
	    boolean isStatic = (flags & Access.STATIC) != 0;
            if (! isStatic && instance == null)
              {
                instance = find((ClassType) type, tr.environ);
		if (! immediate)
		  {
		    String fname = tname.replace('.', '$') + "$instance";
		    decl = new Declaration(fname, type);
		    decl.setPrivate(true);
		    defs.addDeclaration(decl);
		  }
              }
            String fname = fld.getName();
	    Object fvalue;
	    java.lang.reflect.Field rfield;
	    try
	      {
		rfield = rclass.getField(fname);
		fvalue = rfield.get(instance);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException(ex);
	      }
	    String fdname
	      = (fvalue instanceof Named ? ((Named) fvalue).getName()
		 : fname.intern());
	    if (immediate)
	      {
		Binding fbind = Environment.getCurrentBinding(fdname);
		if (fbind != fvalue)
		  {
		    if (fvalue instanceof gnu.mapping.Location)
		      fvalue = ((gnu.mapping.Location) fvalue).get();
		    fbind.set(fvalue);
		  }
	      }
	    else
	      {
		Declaration fdecl = new Declaration(fdname, fld.getType());
		if (! isStatic)
		  fdecl.base = decl;
		fdecl.field = fld;
		fdecl.noteValue(new QuoteExp(fvalue));
		fdecl.setPrivate(true);
		//fdecl.setSimple(false);
		defs.addDeclaration(fdecl);
	      }
          }
        t = t.getSuperclass();
        if (t == null)
          break;
      }
    if (instance != null && ! immediate)
      {
	ClassType thisType = ClassType.make("kawa.standard.require");
	Expression[] args = { new QuoteExp(tname) };
	ApplyExp dofind = Invoke.makeInvokeStatic(thisType, "find", args);
	decl.noteValue(dofind);
	SetExp sexp = new SetExp(decl, dofind);
	sexp.setDefining(true);
	forms.addElement(sexp);
      }
    else
      forms.addElement(QuoteExp.voidExp);
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
