package kawa.lang;

/** The "setter" of procedure that can be used in the LHS of an assignment. */

public class Setter extends ProcedureN
{
  Procedure getter;

  public Setter(Procedure getter)
  {
    this.getter = getter;
    String name = getter.getName();
    if (name != null)
      setName("(setter "+name+")");
  }

  public int numArgs()
  {
    int get_args = getter.numArgs();
    if (get_args < 0) return get_args+1;
    else return get_args + 0x1001;
  }

  public Object applyN(Object[] args)
  { getter.setN(args);  return Interpreter.voidObject; }
}
