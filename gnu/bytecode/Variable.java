package gnu.bytecode;
import java.io.*;

public class Variable implements java.util.Enumeration {

  /* Variables in a Scope are linked together in a linked list. */
  Variable next;
  public final Variable nextVar () { return next; }
  public final boolean hasMoreElements () { return next != null; }
  public Object nextElement ()
  {
    if (next == null)
      throw new java.util.NoSuchElementException("Variable enumeration");
    return next;
  }

  int name_index; /* Index in constant table, or 0 if un-assigned */
  int signature_index; /* Index in constant table, or 0 if un-assigned */

  private int flags = SIMPLE_FLAG;
  /* The SIMPLE_FLAG records the isSimple (q.v.) state. */
  private static final int SIMPLE_FLAG = 0x1;
  /* The PARAMETER_FLAG bit is true for parameters. */
  private static final int PARAMETER_FLAG = 0x2;
  /* The ARTIFICIAL_FLAG bits marks internals variables.
     PARAMETER_FLAG|ARTIFICIAL_FLAG means an incoming parameter. */
  private static final int ARTIFICIAL_FLAG = 0x4;

  static final int UNASSIGNED = -1;
  /** The local variable slot number used by this variable.
   * Not used (by the codegen layer) if !isSimple(). */
  public int offset = UNASSIGNED;
  /** Returns true iff assigned to a local variable slot.
   * Only relevant if isSimple (). */
  public final boolean isAssigned () { return offset != UNASSIGNED; }

  int start_pc;
  int end_pc;

  private Type type;
  public String name;
  final boolean dead () { return end_pc > 0; }

  /** Returns true for a "simple" variable.
   * A "simple" Variable can be stack-allocated using standard local
   * variable slots.  It is allocated by the codegen package.
   * A non-simple variable may need heap allocation, or more more
   * complex access;  it is basically ignored by the codegen package,
   * and must be managed by higher layers.  */
  public final boolean isSimple ()
  {
    return (flags & SIMPLE_FLAG) != 0;
  }
  
  public final void setSimple (boolean simple)
  {
    if (simple)
      flags |= SIMPLE_FLAG;
    else
      flags &= ~SIMPLE_FLAG;
  }

  public final boolean isParameter ()
  {
    return (flags & PARAMETER_FLAG) != 0;
  }
  
  public final void setParameter (boolean parameter)
  {
    if (parameter)
      flags |= PARAMETER_FLAG;
    else
      flags &= ~PARAMETER_FLAG;
  }

  public final boolean isArtificial ()
  {
    return (flags & ARTIFICIAL_FLAG) != 0;
  }
  
  public final void setArtificial (boolean artificial)
  {
    if (artificial)
      flags |= ARTIFICIAL_FLAG;
    else
      flags &= ~ARTIFICIAL_FLAG;
  }

  public final String getName ()
  {
    return name;
  }

  public final void setName (String name)
  {
    this.name = name;
  }

  public final Type getType()
  {
    return type;
  }
  public final void setType(Type type)
  {
    this.type = type;
  }
}
