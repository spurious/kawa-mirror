package codegen;
import java.io.*;
import codegen.*;

public class Scope {
  Scope prev;
  int start_pc;
  int end_pc;
  Variable vars;
  Variable last_var;
  //  Variable lookup (String name);


  Variable new_var (Method method, Type type, byte[] name)
  {
    Variable var = new Variable ();
    var.type = type;
    var.name = name;
    add_var (method, var);
    return var;
   }

  public void add_var (Method method, Variable var)
  {
    var.start_pc = method.PC;
    if (last_var == null)
      vars = var;
    else
      last_var.next = var;
    last_var = var;
    var.offset = method.allocate_local (var);
//System.err.println ("add_var " + var.strName () + ", offset: " + var.offset);
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
  Variable lookup (byte[] name) {
    for (Variable var = vars;  var != null;  var = var.next) {
      if (equals (var.name, name))
	return var;
    }
    return null;
  }
};
