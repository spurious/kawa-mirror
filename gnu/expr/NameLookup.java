// Copyright (c) 2003, 2006  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.util.*;
import gnu.kawa.util.GeneralHashTable;
import gnu.kawa.util.HashNode;

/** Manages the set of declarations "currently" in scope. */

public class NameLookup extends GeneralHashTable<Object,Declaration>
{
  Language language;

  public NameLookup (Language language)
  {
    this.language = language;
  }

  public void push (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return;
    if (++num_bindings >= table.length)
      rehash();
    int hash = hash(symbol);
    HashNode node = makeEntry(symbol, hash, decl);
    int index = hashToIndex(hash);
    node.next = table[index];
    table[index] = node;
  }

  public boolean pop (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return false;
    int hash = hash(symbol);
    HashNode prev = null;
    int index = hashToIndex(hash);
    HashNode node = table[index];
    while (node != null)
      {
	HashNode next = node.next;
        if (node.getValue() == decl)
	  {
	    if (prev == null)
	      table[index] = next;
	    else
	      prev.next = next;
	    num_bindings--;
	    return true;
	  }
	prev = node;
	node = next;
      }
    return false;
  }

  public void push (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      push(decl);
  }

  public void pop (ScopeExp exp)
  {
    for (Declaration decl = exp.firstDecl();
         decl != null;  decl = decl.nextDecl())
      pop(decl);
  }

  public Declaration lookup (Object symbol, int namespace)
  {
    int hash = hash(symbol);
    int index = hashToIndex(hash);
    for (HashNode node = table[index];
	 node != null;  node = node.next)
      {
        Declaration decl = (Declaration) node.getValue();
	if (symbol.equals(decl.getSymbol())
	    && language.hasNamespace(decl, namespace))
	  return decl;
      }
    return null;
  }

  public Declaration lookup (Object symbol, boolean function)
  {
    return lookup(symbol, (function ? Language.FUNCTION_NAMESPACE
			   : Language.VALUE_NAMESPACE));
  }
}
