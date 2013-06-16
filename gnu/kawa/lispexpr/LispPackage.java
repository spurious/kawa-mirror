package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.lists.*;

/** Implement a Common Lisp "package" value.
 * 
 * @author Per Bothner
 * @author Charles Turner
 */

public class LispPackage extends Namespace
{
  /** The set of exported symbols.
   * This is one of the packages in importing.
   */
  Namespace exported = new Namespace();
  
  public void setExportedNamespace (Namespace exp)
  {
    this.exported = exp;
  }
  
  /** The nicknames for this package. */
  LList nicknames = LList.Empty;
  
  private static final Object masterLock = new Object();

  LList shadowingSymbols = LList.Empty;
  
  public LList getShadowingSymbols()
  {
    return shadowingSymbols;
  }
  
  public static final LispPackage CLNamespace = 
      (LispPackage) valueOf("COMMON-LISP");
  public static final LispPackage KeywordNamespace =
      (LispPackage) valueOf("KEYWORD");
  public static final LispPackage KawaNamespace =
      (LispPackage) valueOf("KAWA");
  /* The class namespace is used to resolve Java class names in the context of
   types. The user would specific class::|java.lang.String| if she wanted to
   use java.lang.String as a type name. This avoids various ambiguities. */
  public static final LispPackage ClassNamespace = (LispPackage) valueOf("CLASS");

  /** Common Lisp {@code *package*} special. */
  public static ThreadLocation<LispPackage> currentPackage
      = new ThreadLocation<LispPackage>("*package*");
  
  static
  {
    nsTable.put("CL", CLNamespace);
    CLNamespace.nicknames = Pair.make("CL", CLNamespace.nicknames);
    /* Package CL inherits from this package to facilitate cross language
     * lookups */
    KawaNamespace.setExportedNamespace(EmptyNamespace);
    LispPackage.use(CLNamespace, KawaNamespace); // FIXME: Should be used from CL-USER
    LispPackage.use(CLNamespace, ClassNamespace);
    currentPackage.setGlobal(CLNamespace);
  }

  /** Namespaces that this Namespace imports or uses.
   * These are the <code>imported</code> fields of the
   * <code>NamespaceUse</code>, chained using <code>nextImported</code> fields.
   * The CommonLisp "uses" list. */
  NamespaceUse imported;
  /** Namespaces that import/use this namespace.
   * The CommonLisp "used-by" list. */
  NamespaceUse importing;
  
  /** Used for the CL PACKAGE-USE-LIST function. */
  public static LList pkgUsesList(LispPackage lp)
  {
    LList uses = LList.Empty;
    NamespaceUse it = lp.imported;
    while (it != null) {
      uses = Pair.make(it.imported, uses);
      it = it.nextImported;
    }
    return uses;
  }
  
  /** Used for the CL PACKAGE-USED-BY-LIST function */
  public static LList pkgUsedByList(LispPackage lp)
  {
    LList usedby = LList.Empty;
    NamespaceUse it = lp.importing;
    while (it != null) {
      usedby = Pair.make(it.importing, usedby);
      it = it.nextImporting;
    }
    return usedby;
  }
  
  public static void addNickNames(LispPackage name, LList nicks)
  {
    synchronized (nsTable)
    {
      for (Object nick : nicks) {
        name.nicknames = Pair.make((String) nick, name.nicknames);
        nsTable.put((String) nick, name);
      }
    }
  }
  
  public static void usePackages (LList importees, LispPackage importer)
  {
    for (Object usePkg : importees)
    {
      LispPackage lp;

      if (usePkg instanceof Symbol)
        lp = (LispPackage) LispPackage.valueOfNoCreate(((Symbol) usePkg).getName());//uc
      else if (usePkg instanceof LispPackage)
        lp = (LispPackage) usePkg;
      else
        lp = (LispPackage) LispPackage.valueOfNoCreate((String) usePkg);

      if (lp != null)
      {
        use(importer, lp);
      }
      else
      {
        throw new RuntimeException("The name " + usePkg + " does not designate any package");
     }
   }
 }
  
  public static LispPackage makeLispPackage (Object name, LList nicks,
                                             LList used)
  {
    LispPackage newpack = (LispPackage) LispPackage.valueOf((String) name);
    addNickNames(newpack, nicks);
    usePackages(used, newpack);
    return newpack;    
  }

  public static void use (LispPackage importing, LispPackage imported)
  {
    synchronized (masterLock)
      {
	// FIXME check conflicts.
	NamespaceUse use = new NamespaceUse();
	use.nextImporting = imported.importing;
        use.importing = importing;
	imported.importing = use;
	use.nextImported = importing.imported;
        use.imported = imported;
	importing.imported = use;
      }
  }

  public Symbol lookup(String name, int hash, boolean create)
  {
    Symbol sym = exported.lookup(name, hash, false);
    if (sym != null)
      return sym;
    sym = lookupInternal(name, hash);
    if (sym != null)
      return sym;

    // Do we need to synchronize on masterLock as well?  FIXME
    for (NamespaceUse used = imported;  used != null;
	 used = used.nextImported)
      {
	sym = lookup(name, hash, false);
	if (sym != null)
	  return sym;
      }

    if (create)
      return add(Symbol.makeUninterned(name, this), hash); // Optimization
    else
      return null;
  }

  public Symbol lookupPresent (String name, int hash, boolean intern)
  {
    Symbol sym = exported.lookup(name, hash, false);
    if (sym == null)
      sym = super.lookup(name, hash, intern);
    return sym;
  }

  public boolean isPresent (String name)
  {
    return lookupPresent(name, name.hashCode(), false) != null;
  }

  public boolean unintern (Symbol symbol)
  {
    String name = symbol.getName();
    int hash = name.hashCode();
    if (exported.lookup(name, hash, false) == symbol)
      exported.remove(symbol);
    else if (super.lookup(name, hash, false) == symbol)
      super.remove(symbol);
    else
      return false;
    symbol.setNamespace(null);
    if (removeFromShadowingSymbols(symbol))
      {
	// FIXME check use list:  If thee are two or more different symbols
	// named 'name' in used packages, then signal a conflict.
      }
    return true;
  }

  private void addToShadowingSymbols (Symbol sym)
  {
    for (Object s = shadowingSymbols;  s != LList.Empty; )
      {
	Pair p = (Pair) s;
	if (p.getCar() == sym)
	  return;
	s = p.getCdr();
      }
    shadowingSymbols = new Pair(sym, shadowingSymbols);
  }

  private boolean removeFromShadowingSymbols (Symbol sym)
  {
    Pair prev = null;
    for (Object s = shadowingSymbols;  s != LList.Empty; )
      {
	Pair p = (Pair) s;
	s = p.getCdr();
	if (p.getCar() == sym)
	  {
	    if (prev == null)
	      shadowingSymbols = (LList) s;
	    else
	      prev.setCdr(s);
	    return true;
	  }
	prev = p;
      }
    return false;
  }

  /** The core of the Common Lisp shadow function. */
  public void shadow (String name)
  {
    Symbol sym = lookupPresent(name, name.hashCode(), true);
    addToShadowingSymbols(sym);
  }

  public void shadowingImport (Symbol symbol)
  {
    String name = symbol.getName();
    int hash = name.hashCode();
    Symbol old = lookupPresent(name, name.hashCode(), false);
    if (old != null && old != symbol)
      unintern(old);
    addToShadowingSymbols(symbol);
  }

}


/** This is used to implement two linked lists.
 * For performance they're combined into one object. */
class NamespaceUse
{
  Namespace imported;
  NamespaceUse nextImported;

  Namespace importing;
  NamespaceUse nextImporting;
}
