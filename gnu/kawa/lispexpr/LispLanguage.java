// Copyright (c) 2001, 2004, 2005, 2012  Per M.A. Bothner
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.kawa.lispexpr;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.text.*;
import gnu.lists.*;
import gnu.bytecode.*;
import gnu.mapping.EnvironmentKey;
import gnu.kawa.reflect.StaticFieldLocation;
import java.util.HashMap;
import kawa.lang.Translator; // FIXME
import kawa.lang.Syntax; // FIXME

/** Language sub-class for Lisp-like languages (including Scheme). */

public abstract class LispLanguage extends Language
{
  static public final String quote_sym = "quote";
  static public final String unquote_sym = "unquote";
  static public final String unquotesplicing_sym = "unquote-splicing";
  static public final String quasiquote_sym = "quasiquote";
  /** Used for Kawa infix ':' operator. */
  static public final Symbol lookup_sym = Namespace.EmptyNamespace.getSymbol("$lookup$");
  // FUTURE: Used for: [ e1 e2 ... ]
  // for future sequence/list constructors.
  static public final Symbol bracket_list_sym = Namespace.EmptyNamespace.getSymbol("$bracket-list$");
  // FUTURE: Used for: name[ e1 e2 ... ]
  // Needed for array types - e.g. Object[]
  // and (possible future) parameterized types - e.g. java.util.List[integer]
  static public final Symbol bracket_apply_sym = Namespace.EmptyNamespace.getSymbol("$bracket-apply$");

  public static StaticFieldLocation getNamedPartLocation =
    new StaticFieldLocation("gnu.kawa.functions.GetNamedPart", "getNamedPart");
  static { getNamedPartLocation.setProcedure(); }
  
  /**
   * The unit namespace contains the bindings for symbols such as `cm',
   * `s', etc.
   */
  public static final Namespace unitNamespace =
      Namespace.valueOf("http://kawa.gnu.org/unit", "unit");

  /** The default <code>ReadTable</code> for this language. */
  protected ReadTable defaultReadTable;

  /** Create a fresh <code>ReadTable</code> appropriate for this language. */
  public abstract ReadTable createReadTable ();

  public LispReader getLexer(InPort inp, SourceMessages messages)
  {
    return new LispReader(inp, messages);
  }

  public String getCompilationClass () { return "kawa.lang.Translator"; }

  public boolean parse (Compilation comp, int options)
    throws java.io.IOException, gnu.text.SyntaxException
  {
    kawa.lang.Translator tr = (kawa.lang.Translator) comp;
    Lexer lexer = tr.lexer;
    ModuleExp mexp = tr.mainLambda;
    Values forms = new Values();
    LispReader reader = (LispReader) lexer;
    Compilation saveComp = Compilation.setSaveCurrent(tr);
    try
      {
        if (tr.pendingForm != null)
          {
            tr.scanForm(tr.pendingForm, mexp);
            tr.pendingForm = null;
          }
        for (;;)
          {
            Object sexp = reader.readCommand();
            if (sexp == Sequence.eofValue)
              {
                if ((options & PARSE_ONE_LINE) != 0)
                  return false;  // FIXME
                break;
              }
            if (lexer.peek() == ')')
              {
                lexer.skip();
                lexer.fatal("An unexpected close paren was read.");
              }
            tr.scanForm(sexp, mexp);
            if ((options & PARSE_ONE_LINE) != 0)
              {
                if (tr.getMessages().seenErrors())
                  {
                    // Skip to end of line.
                    for (;;)
                      {
                        int ch = reader.peek();
                        if (ch < 0 || ch == '\r' || ch == '\n')
                          break;
                        reader.skip();
                      }
                  }
                break;
              }
            if ((options & PARSE_PROLOG) != 0
                && tr.getState() >= Compilation.PROLOG_PARSED)
              {
                return true;
              }
          }

        // Must be done before any other module imports this module.
        tr.finishModule(mexp);

        if ((options & PARSE_PROLOG) == 0)
          {
            tr.firstForm = 0;
          }
        tr.setState(Compilation.BODY_PARSED);
      }
    finally
      {
        Compilation.restoreCurrent(saveComp);
      }
    return true;
  }

  /** Resolve names and other post-parsing processing. */
  public void resolve (Compilation comp)
  {
    Translator tr = (Translator) comp;
    tr.resolveModule(tr.getModule());
  }

  public Declaration declFromField (ModuleExp mod, Object fvalue, Field fld)
  {
    Declaration fdecl = super.declFromField(mod, fvalue, fld);
    boolean isFinal = (fld.getModifiers() & Access.FINAL) != 0;
    if (isFinal && fvalue instanceof Syntax) // FIXME - should check type? not value?
      fdecl.setSyntax();
    return fdecl;
  }

  /** Declare in the current Environment a Syntax bound to a static field.
   * @param name the procedure's source-level name.
   * @param cname the name of the class containing the field.
   * @param fname the name of the field, which should be a static
   *   final field whose type extends kawa.lang.Syntax.
   */
  protected void defSntxStFld(String name, String cname, String fname)
  {
    Object property
      = hasSeparateFunctionNamespace() ? EnvironmentKey.FUNCTION : null;
    StaticFieldLocation loc = 
      StaticFieldLocation.define(environ, environ.getSymbol(name), property,
				 cname, fname);
    loc.setSyntax();
  }

  protected void defSntxStFld(String name, String cname)
  {
    defSntxStFld(name, cname, mangleNameIfNeeded(name));
  }

  public boolean selfEvaluatingSymbol (Object obj)
  {
    return obj instanceof Keyword;
  }

  /** Convert the Language's idea of a symbol to a gnu.mapping.Symbol. */
  public static Symbol langSymbolToSymbol (Object sym)
  {
    return ((LispLanguage) Language.getDefaultLanguage()).fromLangSymbol(sym);
  }

  protected Symbol fromLangSymbol (Object sym)
  {
    if (sym instanceof String)
      return getSymbol((String) sym);
    return (Symbol) sym;
  }

  /** The types common to Lisp-like languages. */
  private HashMap<String,Type> types;
  /** The string representations of Lisp-like types. */
  private HashMap<Type,String> typeToStringMap;

    protected synchronized HashMap<String, Type> getTypeMap () {
        if (types == null) {
            types = new HashMap<String, Type>(64); // Plently of space.
            types.put("void", LangPrimType.voidType);
            types.put("int", LangPrimType.intType);
            types.put("char", LangPrimType.charType);

            types.put("byte", LangPrimType.byteType);
            types.put("short", LangPrimType.shortType);
            types.put("long", LangPrimType.longType);
            types.put("float", LangPrimType.floatType);
            types.put("double", LangPrimType.doubleType);
            types.put("never-returns", Type.neverReturnsType);

            types.put("Object", Type.objectType);
            types.put("String", Type.toStringType);

            types.put("object", Type.objectType);
            types.put("number", LangObjType.numericType);
            types.put("quantity", ClassType.make("gnu.math.Quantity"));
            types.put("complex", ClassType.make("gnu.math.Complex"));
            types.put("real", LangObjType.realType);
            types.put("rational", LangObjType.rationalType);
            types.put("integer", LangObjType.integerType);
            types.put("symbol", ClassType.make("gnu.mapping.Symbol"));
            types.put("namespace", ClassType.make("gnu.mapping.Namespace"));
            types.put("keyword", ClassType.make("gnu.expr.Keyword"));
            types.put("pair", ClassType.make("gnu.lists.Pair"));
            types.put("pair-with-position",
                      ClassType.make("gnu.lists.PairWithPosition"));
            types.put("constant-string", ClassType.make("java.lang.String"));
            types.put("abstract-string", ClassType.make("gnu.lists.CharSeq"));
            types.put("character", ClassType.make("gnu.text.Char"));
            types.put("vector", LangObjType.vectorType);
            types.put("string", LangObjType.stringType);
            types.put("empty-list", ClassType.make("gnu.lists.EmptyList"));
            types.put("list", LangObjType.listType);
            types.put("function", ClassType.make("gnu.mapping.Procedure"));
            types.put("procedure", LangObjType.procedureType);
            types.put("input-port", ClassType.make("gnu.mapping.InPort"));
            types.put("output-port", ClassType.make("gnu.mapping.OutPort"));
            types.put("string-output-port",
                      ClassType.make("gnu.mapping.CharArrayOutPort"));
            types.put("string-input-port",
                      ClassType.make("gnu.mapping.CharArrayInPort"));
            types.put("record", ClassType.make("kawa.lang.Record"));
            types.put("type", LangObjType.typeType);
            types.put("class-type", LangObjType.typeClassType);
            types.put("class", LangObjType.typeClass);
            types.put("promise", LangObjType.promiseType);
            types.put("document", ClassType.make("gnu.kawa.xml.KDocument"));
            types.put("readtable",
                      ClassType.make("gnu.kawa.lispexpr.ReadTable"));
        }
        return types;
    }

    /**
     * Try to get a type of the form lang:type.
     *
     * E.g. elisp:buffer.
     *
     * @param name The package-style type name as a string.
     * @return null if no such type could be found, or the corresponding
     * {@code Type}.
     */
    public Type getPackageStyleType(String name) {
        int colon = name.indexOf(':');

        if (colon > 0) {
            String lang = name.substring(0, colon);
            Language interp = Language.getInstance(lang);
            if (interp == null)
                throw new RuntimeException("unknown type '" + name
                    + "' - unknown language '" + lang + '\'');

            Type type = interp.getNamedType(name.substring(colon + 1));

            if (type != null)
                types.put(name, type);
            return type;
        }
        return null;
    }

    @Override
    // FIXME: getNamedType is over-specialised....
    public Type getNamedType (String name) {
        // Initialise the type map if necessary.
        Type type = getTypeMap().get(name);
        return (type != null) ? type : getPackageStyleType(name);
    }

    // FIXME: Would be better and little fuss to use a perfect hash
    // function for this.
    public Type getTypeFor(Class clas) {
        String name = clas.getName();
        if (clas.isPrimitive())
            return getNamedType(name);
        if ("java.lang.String".equals(name))
            return Type.toStringType;
        if ("gnu.math.IntNum".equals(name))
            return LangObjType.integerType;
        if ("gnu.math.DFloNum".equals(name))
            return LangObjType.dflonumType;
        if ("gnu.math.RatNum".equals(name))
            return LangObjType.rationalType;
        if ("gnu.math.RealNum".equals(name))
            return LangObjType.realType;
        if ("gnu.math.Numeric".equals(name))
            return LangObjType.numericType;
        if ("gnu.lists.FVector".equals(name))
            return LangObjType.vectorType;
        if ("gnu.lists.LList".equals(name))
            return LangObjType.listType;
        if ("gnu.text.Path".equals(name))
            return LangObjType.pathType;
        if ("gnu.text.URIPath".equals(name))
            return LangObjType.URIType;
        if ("gnu.text.FilePath".equals(name))
            return LangObjType.filepathType;
        if ("java.lang.Class".equals(name))
            return LangObjType.typeClass;
        if ("gnu.bytecode.Type".equals(name))
            return LangObjType.typeType;
        if ("gnu.bytecode.ClassType".equals(name))
            return LangObjType.typeClassType;
        return super.getTypeFor(clas);
    }

}
