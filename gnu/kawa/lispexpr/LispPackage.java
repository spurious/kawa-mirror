package gnu.kawa.lispexpr;
import gnu.mapping.*;
import gnu.lists.*;

/** Implementa A Common Lisp "package" value.
 * Far from complete. */

public class LispPackage extends Namespace
{
  /** The set of exported symbols.
   * This is on of the packages in importing.
   */
  Namespace exported;

  LList shadowingSymbols = LList.Empty;

  /** Namespaces that this Namespace imports or uses.
   * These are the <code>imported</code> fields of the
   * <code>NamespaceUse</code>, chained using <code>nextImported</code> fields.
   * The CommonLisp "uses" list. */
  NamespaceUse imported;
  /** Namespaces that import/use this namespace.
   * The CommonLisp "used-by" list. */
  NamespaceUse importing;

  /*
  public static void use (Namespace importing, Namespace imported)
  {
    synchronized (masterLock)
      {
	// FIXME check conflicts.
	NamespaceUse use = new NamespaceUse();
	use.nextImporting = imported.importing;
	imported.importing = use;
	use.nextImported = importing.imported;
	importing.imported = use;
      }
  }
  */

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
      return add(new Symbol(this, name), hash);
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
