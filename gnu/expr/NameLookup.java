// Copyright (c) 2003  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.expr;
import java.util.*;

/** Manages the set of declarations "currently" in scope. */

public class NameLookup
{
  Hashtable map = new Hashtable(100);
  Interpreter interp;

  public NameLookup (Interpreter interp)
  {
    this.interp = interp;
  }

  public void push (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return;
    Object old = map.get(symbol);
    if (old == null)
      map.put(symbol, decl);
    else
      {
	Vector v;
	if (old instanceof Vector)
	  v = (Vector) old;
	else
	  {
	    v = new Vector(10);
	    v.addElement(old);
	    map.put(symbol, v);
	  }
	v.addElement(decl);
      }
  }

  public boolean pop (Declaration decl)
  {
    Object symbol = decl.getSymbol();
    if (symbol == null)
      return false;
    Object entry = map.get(symbol);
    if (entry == decl)
      {
	map.remove(symbol);
	return true;
      }
    else if (entry instanceof Vector)
      {
	Vector v = (Vector) entry;
	int size = v.size();
	for (int i = size; --i >= 0; )
	  {
	    if (v.elementAt(i) == decl)
	      {
		while (++i < size)
		  v.setElementAt(v.elementAt(i), i - 1);
		v.setSize(size - 1);
		return true;
	      }
	    
	  }
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
    Object r = map.get(symbol);
    if (r == null)
      return null;
    if (r instanceof Declaration)
      {
	Declaration decl = (Declaration) r;
	if (decl.getSymbol() == symbol
	    && (interp.getNamespaceOf(decl) & namespace) != 0)
	  return decl;
	return null;
      }
    Vector v = (Vector) r;
    int size = v.size();
    for (int i = size; --i >= 0; )
      {
	Declaration decl = (Declaration) v.elementAt(i);
	if (decl.getSymbol() == symbol
	    && (interp.getNamespaceOf(decl) & namespace) != 0)
	  return decl;
      }
    return null;
  }

  public Declaration lookup (Object symbol, boolean function)
  {
    return lookup(symbol, (function ? Interpreter.FUNCTION_NAMESPACE
			   : Interpreter.VALUE_NAMESPACE));
  }
}
