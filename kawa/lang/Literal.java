package kawa.lang;
import gnu.bytecode.*;

/** A Literal contains compile-time information about a constant. */

public class Literal
{
  Object value;

  /* If Compilation is immediate, this is the index into the literalsField
   * array that contains the value of this Literal.
   * Otherwise, if field is non-null, it is the number of the Field.
   * I.e. if the index is 10, the value of the Literal is the value of
   * the static Field named Lit10. */
  int index;

  Literal next;
  public Type type;
  
  int flags;
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

  /** The static final field that contains the value of the literal. */
  Field field;

  /** Assign a static Field to hold the value of this Literal.
   * This supports the same value being used multiple times or cyclically. */
  void assign (Compilation comp)
  {
    index = comp.literalsCount++;
    next = comp.literalsChain;
    comp.literalsChain = this;
    if (! comp.immediate)
      field = comp.mainClass.addField ("Lit"+index, type,
					Access.STATIC|Access.FINAL);
    else
      // Not actually used, except some places test that field==null.
      field = comp.literalsField;
  }

  /** Create a new Literal, where comp must be in immediate mode. */
  public Literal (Object value, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    assign (comp);
    this.type = comp.scmObjectType;
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
	code.emitDup(1);
	code.emitPushInt(i);
	comp.emitLiteral (array[i]);
	// Stack contents:  ..., array, array, i, array[i]
	comp.method.compile_array_store (comp.scmObjectType);
	// Stack contents:  ..., array
      }
  }

  void emit (Compilation comp, boolean ignore)
  {
    gnu.bytecode.CodeAttr code = comp.getCode();
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
	comp.method.compile_invoke_special (comp.initIntegerMethod);
      }
    else if (value instanceof String[])
      emitArray (comp, comp.javaStringType);
    else if (value instanceof Object[])
      emitArray (comp, comp.scmObjectType);
    else
      {
	System.err.print ("Unimplemented compileConstant for ");
	System.err.println (value.getClass ());
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
  void check_cycle ()
  {
    if ((flags & ALLOCATING) != 0)
      throw new Error ("circularity in emit - not supported for class "
		       + value.getClass().getName());
    flags |= ALLOCATING;
  }
}

