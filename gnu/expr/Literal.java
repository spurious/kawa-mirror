package gnu.expr;
import gnu.bytecode.*;

/** A Literal contains compile-time information about a constant. */

public class Literal extends Initializer
{
  Object value;

  /* If field is non-null, it is the number of the Field.
   * I.e. if the index is 10, the value of the Literal is the value of
   * the static Field named Lit10. */
  int index;

  public Type type;
  
  public int flags;
  /** Flag used to indicate intent to allocate the value.
   * The ALLOCATING flag is true if someone has committed to allocate
   * the value, but has not necessarily finished doing so.
   * The flag is used to detect circularities. */
  public static final int ALLOCATING = 1;

  /** Bit in flags used to indicate the value has been allocated.
   * I.e. it has been allocated, but it is not necessarily initialized. */
  public static final int ALLOCATED = 2;

  /** Bit in flags indicates that the value has been fully initialized. */
  public static final int INITIALIZED = 4;

  /* The ASSIGNED flag is set iff code has been emitted to assign the
   * value of the literal to the Field. */
  public static final int ASSIGNED = 8;

  public static final Literal nullLiteral
    = new Literal(null, Type.pointer_type);

  public final Object getValue() { return value; }

  void assign (Compilation comp)
  {
    assign(null, comp);
  }

  /** Assign a static Field to hold the value of this Literal.
   * This supports the same value being used multiple times or cyclically. */
  void assign (String name, Compilation comp)
  {
    next = comp.clinitChain;
    comp.clinitChain = this;
    int flags = comp.immediate ? Access.STATIC|Access.PUBLIC
      : Access.STATIC|Access.FINAL;
    if (name == null)
      {
	index = comp.literalsCount++;
	name = "Lit" + index;
      }
    else
      flags |= Access.PUBLIC;
    field = comp.mainClass.addField (name, type, flags);
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
    flags = ALLOCATED|INITIALIZED|ASSIGNED;
  }

  public Literal (Object value, Type type, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    this.type = type;
  }

  private Literal (Object value, Type type)
  {
    this.value = value;
    this.type = type;
  }

  /** Emit code to re-create this Literal's value, an Object array. */
  void emitArray (Compilation comp, Type element_type)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    Object[] array = (Object[]) value;
    int len = array.length;
    code.emitPushInt(len);
    code.emitNewArray(element_type);
    flags |= Literal.ALLOCATED;
    for (int i = 0;  i < len;  i++)
      {
	Object value = array[i];
	if (value == null)
	  continue;
	code.emitDup(1);
	code.emitPushInt(i);
	comp.emitLiteral (value);
	// Stack contents:  ..., array, array, i, array[i]
	code.emitArrayStore(comp.typeObject);
	// Stack contents:  ..., array
      }
  }

  public void emit(Compilation comp)
  {
    if (! comp.immediate && (flags & Literal.INITIALIZED) == 0)
      emit (comp, true);
  }

  void emit (Compilation comp, boolean ignore)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
    if (value == null)
      {
	if (! ignore)
	  code.emitPushNull();
	return;
      }
    if (value instanceof String)
      {
	if (! ignore)
	  code.emitPushString(value.toString ());
	return;
      }
    if ((flags & ALLOCATED) != 0)
      {
	if ((flags & ASSIGNED) == 0 || field == null)
	  throw new Error ("internal error in Literal.emit");
	if (! ignore)
	  code.emitGetStatic(field);
	return;
      }
    if (value instanceof Compilable)
      ((Compilable) value).emit (this, comp);
    else if (value instanceof Integer)
      {
	code.emitNew(comp.javaIntegerType);
	code.emitDup(comp.javaIntegerType);
	code.emitPushInt(((Integer)value).intValue ());
	code.emitInvokeSpecial(comp.initIntegerMethod);
      }
    else if (value instanceof String[])
      emitArray (comp, comp.typeString);
    else if (value instanceof Object[])
      emitArray (comp, comp.typeObject);
    else
      {
	comp.error('e', "unimplemented support for compiling "
		   + value.getClass().getName() + " literals");
	code.emitGetStatic(Compilation.undefinedConstant);
      }
    flags |= ALLOCATED|INITIALIZED;
    if (field != null && (flags & ASSIGNED) == 0)
      {
	if (! ignore)
	  code.emitDup(Compilation.scmPairType);
	code.emitPutStatic(field);
	flags |= ASSIGNED;
      }
    else if (ignore)
      code.emitPop(1);
  }

  /** Utility function to check for circular literals dependencies.
   * Use this in a Compilable.emit method if circularities are not allowed
   * (perhaps because it it not worth the trouble to handle them). */
  public void check_cycle ()
  {
    if ((flags & ALLOCATING) != 0)
      throw new Error ("circularity in emit - not supported for class "
		       + value.getClass().getName());
    flags |= ALLOCATING;
  }
}

