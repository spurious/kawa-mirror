package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.reflect.Invoke;
import gnu.kawa.functions.Convert;
import java.util.*;

public class require extends Syntax
{
  /* NOTE on handling mutually recursive modules:

     How can Kawa compile two or more modules that mutually require
     each other?  Kawa separates the "scan" stage (top-level
     scanning of a module, looking for definitions), and "rewrite"
     (expand macros and resolve names) makes this possible.

     If module A sees a (require <B>), it needs to suspend scanning A,
     and import the definitions exported by B.  If B has not been
     compiled yet, it must parse and scan B.  If while scanning B, it
     sees a (require <A>), it must wait to import the definitions of A
     until we've done scanning B, returned to A, and finished scanning
     A.  At that point we can add to B the definitions exported from
     A.  Thus the (require <A>) in B.has to *lazily* imports A's
     definitions, using some kind of placeholder.

     One complication is knowing whether a (require <B>) refers to a
     source file to be compiled.  It is not enough to check if a class
     B exists, since if we're compiljng B we want to use the current
     source B.scm, not an older B.class.  This is complicated by the
     (module-name B) declaration: We don't know whether source file
     B.scm provides the B class until we've parsed B.scm.  A solution
     to this problem is that we first parse all the source files (as
     listed on the command line),
     yielding their S-expression form.  We then check for module-name
     forms.  However, the possibility of macros does complicate this:
     There could be a macro that re-defines module-name, and there
     could be a macro that expands to module-name.  Also, we could
     have commands that change the reader or read-table.  Arguably worrying
     about these possibilities may be overkill.  However, it can be
     handled thus: Parse each source file to S-expressions.  Scan each
     source file's S-expression until the first require (if any).
     Then go back to each source file, process the require, and scan
     the rest of the file.  If we see a require for one of the source
     files later in the compilation list, skip it until the end.  At
     the end process any deferred require's.  Finally, do the
     "rewrite" step and the rest of compilation.
   */
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
    ClassType thisType = ClassType.make("kawa.standard.require");
    Expression[] args = { new QuoteExp(tname) };
    ApplyExp dofind = Invoke.makeInvokeStatic(thisType, "find", args);
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
		    decl.noteValue(dofind);
		    SetExp sexp = new SetExp(decl, dofind);
		    sexp.setDefining(true);
		    forms.addElement(sexp);
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
                    Type ftype = fld.getType();
		    boolean isAlias = ftype == Compilation.typeLocation;
		    String fdname
		      = (fvalue instanceof Named && ! isAlias
			 ? ((Named) fvalue).getName()
			 : Compilation.demangleName(fname, true).intern());
		    Type dtype = interp.getTypeFor(ftype.getReflectClass());
		    // We create an alias in the current context that points
		    // a dummy declaration in the exported module.  Normally,
		    // followAliases will skip the alias, so we use the latter.
		    // But if the binding is re-exported (or EXTERNAL_ACCESS
		    // gets set), then we need a separate declaration.
		    // (If EXTERNAL_ACCESS, the field gets PRIVATE_PREFIX.)
		    Declaration adecl = defs.getDefine(fdname, 'w', tr);
		    Declaration fdecl = new Declaration(fdname, dtype);
		    ReferenceExp fref = new ReferenceExp(fdecl);
		    SetExp sexp = new SetExp(adecl, fref);
		    sexp.setDefining(true);
                    if (isAlias || ftype.isSubtype(Compilation.typeBinding))
                      fdecl.setIndirectBinding(true);
		    if (isAlias)
		      fdecl.setAlias(true);
		    if (! isStatic || fvalue instanceof Macro)
		      fdecl.base = decl;
		    fdecl.field = fld;
		    if (ftable == null)
		      ftable = new Hashtable(40);
		    ftable.put(fname, adecl);
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
		    // Need to be aliase - so we can follow them!
		    adecl.setAlias(true);
		    adecl.setIndirectBinding(true);
		    adecl.noteValue(fref);
		    // Kludge, needed by FindCapturedVars.capture:
		    fdecl.context = defs;
		    if ((rfield.getModifiers() & Access.FINAL) != 0)
		      {
			adecl.setType(dtype);
			adecl.setFlag(Declaration.IS_CONSTANT);
		      }
		    fdecl.setPrivate(true);
		    adecl.setPrivate(true);
		    fdecl.setSimple(false);
		    adecl.setFlag(Declaration.IS_IMPORTED);
		    adecl.setSimple(false);
		    tr.pushBinding(fdname, adecl);  // Add to translation env.
		    forms.addElement(sexp);
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

    if ((instance == null || immediate)
	&& isRunnable) // Need to make sure 'run' is invoked.
      forms.addElement(Convert.makeCoercion(dofind, Type.void_type));
    tr.mustCompileHere();
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }
}
