// Copyright (c) 1997  Cygnus Solutions, Inc.
// This is free software;  for terms and warranty disclaimer see ./LICENSE.

package gnu.bytecode;
import java.io.*;

public class Scope
{
  /** The enclosing scope. */
  Scope parent;
  Scope nextSibling;
  Scope firstChild, lastChild;

  int start_pc;
  int end_pc;
  Variable vars;
  Variable last_var;
  //  Variable lookup (String name);
  public final Variable firstVar () { return vars; }

  public VarEnumerator allVars () { return new VarEnumerator (this); }

  /** Link this scope as the next child of its parent scope. */
  public void linkChild (Scope parent)
  {
    this.parent = parent;
    if (parent == null)
      return;
    if (parent.lastChild == null)
      parent.firstChild = this;
    else
      parent.lastChild.nextSibling = this;
    parent.lastChild = this;
  }

  Variable addVariable (Method method, Type type, String name)
  {
    Variable var = new Variable ();
    var.setType(type);
    var.setName(name);
    addVariable (method, var);
    return var;
   }

  public void addVariable (Variable var)
  {
    if (last_var == null)
      vars = var;
    else
      last_var.next = var;
    last_var = var;
  }

  public void addVariable (Method method, Variable var)
  {
    var.start_pc = method.PC;
    addVariable (var);
    if (var.isSimple ())
      method.allocate_local (var);
  }

  /**
   * Return a variable the scope, by numerical index.
   * @param index the number of the variable
   */
  Variable find_var (int index) {
    Variable var = vars;
    while (--index >= 0)
      var = var.next;
    return var;
  }

  static boolean equals (byte[] name1, byte[] name2) {
    if (name1.length != name2.length)
      return false;
    if (name1 == name2)
      return true;
    for (int i = name1.length; --i >= 0; )
      if (name1[i] != name2[i])
	return false;
    return true;
  }

  /**
   * Search by name for a Variable.
   * @param name name to search for
   * @return the Variable, or null if not found (in this scope).
   */
  Variable lookup (String name) {
    for (Variable var = vars;  var != null;  var = var.next) {
      if (name.equals(var.name))
	return var;
    }
    return null;
  }
};
