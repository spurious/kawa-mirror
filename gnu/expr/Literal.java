package gnu.expr;
import gnu.bytecode.*;

/** A Literal contains compile-time information about a constant. */

public class Literal
{
  Literal next;

  public Field field;

  Object value;

  /* If field is non-null, it is the number of the Field.
   * I.e. if the index is 10, the value of the Literal is the value of
   * the static Field named Lit10. */
  int index;

  public Type type;
  
  public int flags;

  /** Set at the beginning of the call to writeObject. */
  static final int WRITING = 1;

  /** Set at the end of the call to writeObject. */
  static final int WRITTEN = 2;

  static final int CYCLIC = 4;

  /** In pass 2, object has been at least allocated. */
  static final int EMITTED = 8;

  /** Values produced by calling writeObject on value. */
  Object[] argValues;
  /** Types produced by calling writeObject on value. */
  Type[] argTypes;

  public static final Literal nullLiteral
    = new Literal(null, Type.nullType);

  public final Object getValue() { return value; }

  void assign (Compilation comp)
  {
    assign((String) null, comp);
  }

  /** Assign a static Field to hold the value of this Literal.
   * This supports the same value being used multiple times or cyclically. */
  void assign (String name, Compilation comp)
  {
    int flags = comp.immediate ? Access.STATIC|Access.PUBLIC
      : Access.STATIC|Access.FINAL;
    if (name == null)
      {
	index = comp.literalsCount++;
	name = "Lit" + index;
      }
    else
      flags |= Access.PUBLIC;
    assign(comp.mainClass.addField (name, type, flags), comp);
  }

  void assign (Field field, Compilation comp)
  {
    next = comp.literalsChain;
    comp.literalsChain = this;
    this.field = field;
  }

  /** Create a new Literal, where comp must be in immediate mode. */
  public Literal (Object value, Compilation comp)
  {
    this (value, (String) null, comp);
  }

  public Literal (Object value, String name, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    this.type = Type.make(value.getClass());
    assign(name, comp);
  }

  /** Create a new Literal, for a value available from a static field.
  * The field must be static and already exist. */
  public Literal (Object value, Field field, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    this.field = field;
    this.type = field.getType();
    flags = WRITTEN|EMITTED;
  }

  public Literal (Object value, Type type, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    this.type = type;
  }

  Literal (Object value, Type type)
  {
    this.value = value;
    this.type = type;
  }

  public static void emit(Compilation comp)
  {
    if (! comp.immediate && comp.literalsChain != null
	&& comp.litTable == null)
      {
	comp.litTable = new LitTable(comp);
	try
	  {
	    comp.litTable.emit();
	  }
	catch (Throwable ex)
	  {
	    comp.error('e', "Literals: Internal error:" + ex);
	    ex.printStackTrace(System.err);
	  }
      }
  }
}

