package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.reflect.Invoke;
import java.util.*;

public class require extends Syntax
{
  static java.util.Hashtable featureMap = new java.util.Hashtable();

  static void map(String featureName, String className)
  {
    featureMap.put(featureName, className);
  }

  private static final String SLIB_PREFIX = "gnu.kawa.slib.";

  static
  {
    map("generic-write", SLIB_PREFIX + "genwrite");
    map("pretty-print", SLIB_PREFIX + "pp");
    map("pprint-file", SLIB_PREFIX + "ppfile");
    map("printf", SLIB_PREFIX + "printf");
    map("xml", SLIB_PREFIX + "XML");
    map("readtable", SLIB_PREFIX + "readtable");
    map("http", SLIB_PREFIX + "HTTP");
    map("srfi-1", SLIB_PREFIX + "srfi1");
    map("list-lib", SLIB_PREFIX + "srfi1");
  }

  public static String mapFeature(String featureName)
  {
    return (String) featureMap.get(featureName);
  }

  public static Object find(String typeName)
  {
    try
      {
	Object value =
	  find(Class.forName(typeName), Environment.getCurrent()); 
	if (value instanceof Runnable)
	  ((Runnable) value).run();
	return value;
      }
    catch (java.lang.ClassNotFoundException ex)
      {
	throw new WrappedException("cannot find module " + typeName, ex);
      }
  }

  public static Object find(ClassType type, Environment env)
  {
    return find(type.getReflectClass(), env);
  }

  public static Object find(Class ctype, Environment env)
  {
    String mangledName = (ctype.getName() + "$instance").intern();
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
    return value;
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    boolean immediate = tr.immediate && defs instanceof ModuleExp;
    Interpreter interp = tr.getInterpreter();
    Object name = ((Pair) st.cdr).car;
    // Type type = Scheme.expType(tr.rewrite(name));
    Type type = null;
    Pair p;
    Hashtable ftable = null;
    if (name instanceof Pair && (p = (Pair) name).car == "quote")
      {
	name = p.cdr;
	if (! (name instanceof Pair)
	    || (p = (Pair) name).cdr != LList.Empty
	    || ! (p.car instanceof String))
	  {
	    tr.error('e', "invalid quoted symbol for 'require'");
	    return false;
	  }
	name = mapFeature((String) p.car);
	if (name == null)
	  {
	    tr.error('e', "unknown feature name '"+p.car+"' for 'require'");
	    return false;
	  }
	type = ClassType.make((String) name);
      }
    else
      {
	if (name instanceof String)
	  {
	    String str = (String) name;
	    int len = str.length();
	    if (len > 2
		&& str.charAt(0) == '<'
		&& str.charAt(len-1) == '>')
	      {
		str = str.substring(1, len-1);
		if (str.indexOf('.') < 0
		    && kawa.repl.compilationPrefix != null)
		  str = kawa.repl.compilationPrefix + str;
		type = Scheme.string2Type(str);
	      }
	  }
      }
    if (type == null)
      {
	tr.error('e', "invalid specifier for 'require'");
	return false;
      }
    String tname = type.getName();
    Object instance = null;
    ClassType t = (ClassType) type;
    boolean isRunnable = t.isSubtype(Compilation.typeRunnable);
    Declaration decl = null;
    Vector macros = null;
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
                instance = find((ClassType) type, Environment.getCurrent());
		if (! immediate)
		  {
		    String fname = tname.replace('.', '$') + "$instance";
		    decl = new Declaration(fname, type);
		    decl.setPrivate(true);
		    defs.addDeclaration(decl);
		    decl.setCanRead(true);
		  }
              }
            String fname = fld.getName();
	    java.lang.reflect.Field rfield;
	    try
	      {
		rfield = rclass.getField(fname);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException(ex);
	      }
	    if (immediate)
	      {
		ClassMemberConstraint.define(fname, instance, rfield, Environment.getCurrent());
	      }
	    else
	      {
		try
		  {
		    Object fvalue = rfield.get(instance);
		    String fdname
		      = (fvalue instanceof Named ? ((Named) fvalue).getName()
			 : Compilation.demangleName(fname, true).intern());
                    Type ftype = fld.getType();
		    Type dtype = interp.getTypeFor(ftype.getReflectClass());
		    Declaration fdecl = defs.getDefine(fdname, 'w', tr);
		    fdecl.setType(dtype);
                    if (ftype.isSubtype(Compilation.typeBinding))
                      fdecl.setIndirectBinding(true);
		    if (! isStatic || fvalue instanceof Macro)
		      fdecl.base = decl;
		    fdecl.field = fld;
		    if (ftable == null)
		      ftable = new Hashtable(40);
		    ftable.put(fname, fdecl);
		    if (fvalue instanceof Macro)
		      {
			// Copy the Macro, as we will be modifying it later.
			Macro mac = new Macro((Macro) fvalue);
			fvalue = mac;
			mac.bind(fdecl);
			if (macros == null)
			  macros = new Vector();
			macros.addElement(fvalue);
		      }
		    else
		      fdecl.noteValue(new QuoteExp(fvalue));
		    fdecl.setPrivate(true);
		    fdecl.setSimple(false);
		    tr.pushBinding(fdname, fdecl);  // Add to translation env.
		  }
		catch (Exception ex)
		  {
		    throw new WrappedException(ex);
		  }
	      }
          }
        t = t.getSuperclass();
        if (t == null)
          break;
      }

    if (macros != null)
      {
	for (int i = macros.size();  --i >= 0; )
	  {
	    ((Macro) macros.elementAt(i)).captureDecls(ftable);
	  }
      }

    ClassType thisType = ClassType.make("kawa.standard.require");
    Expression[] args = { new QuoteExp(tname) };
    ApplyExp dofind = Invoke.makeInvokeStatic(thisType, "find", args);
    Expression action;
    if (instance != null && ! immediate)
      {
	decl.noteValue(dofind);
	SetExp sexp = new SetExp(decl, dofind);
	sexp.setDefining(true);
	action = sexp;
      }
    else if (isRunnable) // Need to make sure 'run' is invoked.
      action = gnu.kawa.functions.Convert.makeCoercion(dofind, Type.void_type);
    else
      action = QuoteExp.voidExp;
    forms.addElement(action);
    tr.mustCompileHere();
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
