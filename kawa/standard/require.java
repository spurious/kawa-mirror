package kawa.standard;
import kawa.lang.*;
import gnu.mapping.*;
import gnu.mapping.Location; // As opposed to gnu.bytecode.Location
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.expr.*;
import gnu.kawa.reflect.*;
import gnu.kawa.functions.Convert;
import java.util.*;

public class require extends Syntax
{
  public static final require require = new require();
  static { require.setName("require"); }

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
    map("srfi-37", SLIB_PREFIX + "srfi37");
    map("args-fold", SLIB_PREFIX + "srfi37");
    map("srfi-64", SLIB_PREFIX + "testing");
    map("testing", SLIB_PREFIX + "testing");
    map("gui", SLIB_PREFIX + "gui");
  }

  public static String mapFeature(String featureName)
  {
    return (String) featureMap.get(featureName);
  }

  public static Object find(String typeName)
  {
    return ModuleInfo.find(typeName).getInstance();
  }

  public boolean scanForDefinitions (Pair st, Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    Object name = ((Pair) st.cdr).car;
    // Type type = Scheme.expType(tr.rewrite(name));
    Type type = null;
    Pair p;
    if (name instanceof Pair
        && tr.matches((p = (Pair) name).car, Scheme.quote_sym))
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
    return importDefinitions(type, null, forms, defs, tr);
  }

  public static boolean importDefinitions (Type type, String uri, Vector forms,
					   ScopeExp defs, Compilation tr)
  {
    Language language = tr.getLanguage();
    boolean immediate = tr.immediate && defs instanceof ModuleExp;
    String tname = type.getName();
    Object instance = null;
    ClassType t = (ClassType) type;
    boolean isRunnable = t.isSubtype(Compilation.typeRunnable);
    Declaration decl = null;
    ClassType thisType = ClassType.make("kawa.standard.require");
    ModuleInfo info = ModuleInfo.find(tname);
    Expression[] args = { new QuoteExp(tname) };
    Expression dofind = Invoke.makeInvokeStatic(thisType, "find", args);
    Field instanceField = null;
    dofind.setLine(tr);
    int formsStart = forms.size();
    Vector declPairs = new Vector();
    // Should be info.getModuleExp() ?? FIXME
    ModuleExp mod = new ModuleExp();
    // Should skip if immediate.  FIXME
    ClassType typeFieldLocation
      = ClassType.make("gnu.kawa.reflect.FieldLocation");
    for (;;)
      {
	Class rclass = t.getReflectClass();
        for (Field fld = t.getFields();  fld != null;  fld = fld.getNext())
          {
            int flags = fld.getFlags();
            if ((flags & Access.PUBLIC) == 0)
              continue;
            String fname = fld.getName();
	    boolean isStatic = (flags & Access.STATIC) != 0;
            if (! isStatic && instance == null)
              {
                instance = info.getInstance();
		String iname = tname.replace('.', '$') + "$instance";
		decl = new Declaration(iname, type);
		decl.setPrivate(true);
                decl.setFlag(Declaration.IS_CONSTANT
                             |Declaration.MODULE_REFERENCE);
		defs.addDeclaration(decl);
		if (immediate)
		  {
		    decl.noteValue(new QuoteExp(instance));
		  }
		else
		  {
		    decl.noteValue(dofind);
		    SetExp sexp = new SetExp(decl, dofind);
		    sexp.setLine(tr);
		    sexp.setDefining(true);
		    forms.addElement(sexp);
                    formsStart = forms.size();
                    decl.setFlag(Declaration.EARLY_INIT);
                    // If Runnable, we need to set decl value in initializer,
                    // and later 'run' it, so it needs to be stored in a field.
                    if (isRunnable)
                      decl.setSimple(false);
		  }
                decl.setFlag(Declaration.TYPE_SPECIFIED);
              }
	    java.lang.reflect.Field rfield;
	    Object fvalue;
	    boolean isFinal;
	    try
	      {
		rfield = rclass.getField(fname);
		isFinal = (rfield.getModifiers() & Access.FINAL) != 0;
		if (isFinal)
		  fvalue = rfield.get(instance);
		else
		  fvalue = Undefined.getInstance();
	      }
	    catch (Exception ex)
	      {
		throw WrappedException.wrapIfNeeded(ex);
	      }
	    Type ftype = fld.getType();
	    boolean isAlias = ftype.isSubtype(Compilation.typeLocation);
	    Declaration fdecl = makeDeclInModule1(mod, fvalue, fld, language);
	    Object fdname = fdecl.getSymbol();
            if (fname.startsWith(Declaration.PRIVATE_PREFIX))
              {
                declPairs.add(null);
                declPairs.add(fdecl);
                continue;
              }
            if (fname.equals("$instance"))
              {
                instanceField = fld;
                continue;
              }

	    // We create an alias in the current context that points
	    // a dummy declaration in the exported module.  Normally,
	    // followAliases will skip the alias, so we use the latter.
	    // But if the binding is re-exported (or EXTERNAL_ACCESS
	    // gets set), then we need a separate declaration.
	    // (If EXTERNAL_ACCESS, the field gets PRIVATE_PREFIX.)
	    Object aname;

            if (fdname instanceof Symbol)
              aname = fdname;
            else
              {
                String sname = fdname.toString();
                if (uri == null)
                  aname = sname.intern();
                else
                  aname = Symbol.make(uri, sname);
              }
            boolean isImportedInstance = fname.endsWith("$instance");
	    try
	      {
		Declaration adecl;
                if (isImportedInstance)
                  {
                    if (defs.lookup(aname) != null)
                      continue;
                    adecl = defs.addDeclaration(aname);
                    adecl.setFlag(Declaration.IS_CONSTANT
                                  |Declaration.MODULE_REFERENCE);
                    adecl.setType(fdecl.getType());
                    adecl.setFlag(Declaration.TYPE_SPECIFIED);
                 }
                else
                  {
                    adecl = defs.getDefine(aname, 'w', tr);
                    adecl.setAlias(true);
                    adecl.setIndirectBinding(true);
                  }
		Expression fexp;
                ReferenceExp fref;
		if (immediate)
		  {
                    if (isFinal
                        && (isStatic || isImportedInstance))
                      {
                        // An optimization.
                        adecl.setType(fdecl.getType());
                        adecl.setAlias(false);
                        adecl.setIndirectBinding(isAlias);
                      }
                    else
                      fvalue = new FieldLocation(instance, t, fname);
                    fexp = new QuoteExp(fvalue);
                    adecl.setCanRead(true);
                    fref = null;
		  }
		else
		  {
		    fref = new ReferenceExp(fdecl);
		    fref.setContextDecl(decl);
                    if (! isImportedInstance)
                      {
                        fref.setDontDereference(true);
                        fref.setFlag(ReferenceExp.CREATE_FIELD_REFERENCE);
                        adecl.setPrivate(true);
                      }
		    fexp = fref;
		  }
		if ((rfield.getModifiers() & Access.FINAL) != 0 && ! isAlias)
		  {
		    adecl.setFlag(Declaration.IS_CONSTANT);
		  }
		if (fdecl.isProcedureDecl())
		  adecl.setProcedureDecl(true);
		if (isStatic)
		  adecl.setFlag(Declaration.STATIC_SPECIFIED);
		/*
		if (fvalue instanceof gnu.mapping.Location)
		  {
		    Declaration fdecl = new Declaration(aname, ftype);
		    fdecl.base = decl;
		    fdecl.field = fld;
		    ReferenceExp ref = new ReferenceExp(fdecl);
		    ref.setDontDereference(true);
		    fexp = ref;
		  }
		else
		  {
		    int j = 0;
		    Method m;
		    if (isStatic)
		      {
			m = makeClassMemberLocation2;
			args = new Expression[2];
		      }
		    else
		      {
			m = makeClassMemberLocation3;
			args = new Expression[3];
			args[j++] = new ReferenceExp(decl);
		      }
		    args[j++] = new QuoteExp(t.getName());
		    args[j++] = new QuoteExp(fname);
		    fexp = new ApplyExp(m, args);
		  }
		*/
		if (! immediate)
		  {
		    SetExp sexp = new SetExp(adecl, fexp);
                    adecl.setFlag(Declaration.EARLY_INIT);
		    sexp.setDefining(true);
                    if (isImportedInstance)
                      {
                        // Make sure the "MODULE$instance" declarations are
                        // initialized first, since we may need then for
                        // imported declarations that are re-exported.  (The
                        // instance may be needed for FieldLocation values.)
                        forms.insertElementAt(sexp, formsStart);
                        formsStart++;
                      }
                    else
                      forms.addElement(sexp);
		  }
		adecl.noteValue(fexp);
		adecl.setFlag(Declaration.IS_IMPORTED);
		adecl.setSimple(false);
		tr.push(adecl);  // Add to translation env.
                declPairs.add(adecl);
                declPairs.add(fdecl);
	      }
	    catch (Exception ex)
	      {
		throw new WrappedException(ex);
	      }
          }
        t = t.getSuperclass();
        if (t == null)
          break;
      }

    int ndecls = declPairs.size();
    for (int i = 0;  i < ndecls;  i += 2)
      {
        Declaration adecl = (Declaration) declPairs.elementAt(i);
        Declaration fdecl = (Declaration) declPairs.elementAt(i+1);

        makeDeclInModule2(mod, fdecl);

        Expression fval = fdecl.getValue();
        if (adecl != null && fdecl.isIndirectBinding() && ! immediate
            && fval instanceof ReferenceExp)
          {
            ReferenceExp fref = (ReferenceExp) fval;
            ReferenceExp aref = (ReferenceExp) adecl.getValue();
            aref.setBinding(fref.getBinding());
            Declaration fcontext = fref.contextDecl();
            if (fcontext != null && fcontext.needsContext())
              {
                Declaration cdecl = defs.lookup(fcontext.getSymbol());
                cdecl.setFlag(Declaration.EXPORT_SPECIFIED);
                fcontext = cdecl;
              }
            aref.setContextDecl(fcontext);
          }
      }

    if (isRunnable)
      {
	if (immediate)
	  {
            dofind = new ApplyExp(ClassType.make("gnu.expr.ModuleInfo")
                                  .getDeclaredMethod("getRunInstance", 0),
                                  new Expression[] { new QuoteExp(info)});
	    forms.addElement(gnu.kawa.functions.Convert
                             .makeCoercion(dofind,
                                           new QuoteExp(Type.void_type)));
	  }
	else
          {
            Method run = Compilation.typeRunnable.getDeclaredMethod("run", 0);
            if (decl != null) // Need to make sure 'run' is invoked.
              dofind = new ReferenceExp(decl);
            else
              {
                if (instanceField != null)
                  { //Optimization
                    args = new Expression[]
                      { new QuoteExp(type), new QuoteExp("$instance") };
                    dofind = new ApplyExp(SlotGet.staticField, args);
                  }
                //else
                //dofind = Convert.makeCoercion(dofind, Type.void_type);
              }
            dofind = new ApplyExp(run, new Expression[] { dofind });
            dofind.setLine(tr);
            forms.addElement(dofind);
	  }
      }
    tr.mustCompileHere();
    return true;
  }

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return null;
  }

  public static void makeModule (ModuleExp mod, ClassType type,
				 Object instance)
  {
    Language language = Language.getDefaultLanguage();
    Class rclass = type.getReflectClass();
    for (Field fld = type.getFields();  fld != null;  fld = fld.getNext())
      {
	int flags = fld.getFlags();
	if ((flags & Access.PUBLIC) == 0)
	  continue;
	try
	  {
            Object fvalue = rclass.getField(fld.getName()).get(instance);
            Declaration fdecl = makeDeclInModule1(mod, fvalue, fld, language);
	  }
	catch (Exception ex)
	  {
	    throw new WrappedException(ex);
	  }
      }
    mod.setFlag(false, ModuleExp.LAZY_DECLARATIONS);
    for (Declaration fdecl = mod.firstDecl();
         fdecl != null;  fdecl = fdecl.nextDecl())
      {
        makeDeclInModule2(mod, fdecl);
      }
  }

  private static Declaration makeDeclInModule1 (ModuleExp mod, Object fvalue,
					       Field fld, Language language)
  {
    String fname = fld.getName();
    Type ftype = fld.getType();
    boolean isAlias = ftype.isSubtype(Compilation.typeLocation);
    Object fdname;
    // FIXME if fvalue is FieldLocation, and field is final,
    // get name from value of field.
    boolean isImportedInstance;
    if ((isImportedInstance = fname.endsWith("$instance")))
      fdname = fname;
    else if (fvalue instanceof Named) // && ! isAlias
      fdname = ((Named) fvalue).getSymbol();
    else
      {
	// FIXME move this to demangleName
	if (fname.startsWith(Declaration.PRIVATE_PREFIX))
	  fname = fname.substring(Declaration.PRIVATE_PREFIX.length());
	fdname = Compilation.demangleName(fname, true).intern();
      }
    Type dtype = isAlias ? Type.pointer_type
      : language.getTypeFor(ftype.getReflectClass());
    Declaration fdecl = new Declaration(fdname, dtype);
    boolean isStatic = (fld.getModifiers() & Access.STATIC) != 0;
    boolean isFinal = (fld.getModifiers() & Access.FINAL) != 0;
    if (isAlias)
      fdecl.setIndirectBinding(true);
    else if (isFinal && ftype.isSubtype(Compilation.typeProcedure))
      fdecl.setProcedureDecl(true);
    if (isStatic)
      fdecl.setFlag(Declaration.STATIC_SPECIFIED);
    fdecl.field = fld; 
    if (isFinal && ! isAlias) // FIXME? ok for location?
      fdecl.setFlag(Declaration.IS_CONSTANT);
    if (isImportedInstance)
      fdecl.setFlag(Declaration.MODULE_REFERENCE);
    mod.addDeclaration(fdecl);
    fdecl.setSimple(false);
    if (isFinal && fvalue instanceof Syntax) // FIXME - should check type? not value?
      {
	fdecl.setSyntax();
        if (fvalue instanceof Macro)
          {
            Macro mac = (Macro) fvalue;
            mac.setCapturedScope(mod);
          }
      }
    if (isFinal
        && (! (fvalue instanceof gnu.mapping.Location)
            || fvalue instanceof FieldLocation))
      fdecl.noteValue(new QuoteExp(fvalue));
    else
      fdecl.noteValue(null);
    return fdecl;
  }

  private static void makeDeclInModule2 (ModuleExp mod, Declaration fdecl)
  {
    Object fvalue = fdecl.getConstantValue();
    if (fvalue instanceof FieldLocation)
      {
	FieldLocation floc = (FieldLocation) fvalue;
        Declaration vdecl = floc.getDeclaration();
        ReferenceExp fref = new ReferenceExp(vdecl);
        if (floc.isIndirectLocation())
          fref.setDontDereference(true);
        fdecl.setValue(fref);
        if (vdecl.isProcedureDecl())
          fdecl.setProcedureDecl(true);
        if (vdecl.getFlag(Declaration.IS_SYNTAX))
          {
            fdecl.setSyntax();
            fvalue = floc.get(null);
            if (fvalue instanceof Macro)
              {
                Macro mac = (Macro) fvalue;
                mac.setCapturedScope(mod);
              }
          }
        if (! fdecl.getFlag(Declaration.STATIC_SPECIFIED))
          {
            ClassType vtype = floc.getDeclaringClass();
            String vname = vtype.getName();
            for (Declaration xdecl = mod.firstDecl();
                 xdecl != null;  xdecl = xdecl.nextDecl())
              {
                if (vname.equals(xdecl.getType().getName())
                    && xdecl.getFlag(Declaration.MODULE_REFERENCE))
                  {
                    fref.setContextDecl(xdecl);
                    break;
                  }
              }
          }
      }
  }
}
