package kawa.lang;
import codegen.*;

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
      field = comp.mainClass.new_field ("Lit"+index, type,
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
    this.type = field.type;
    flags = ALLOCATED|INITIALIZED|ASSIGNED;
  }

  public Literal (Object value, Type type, Compilation comp)
  {
    this.value = value;
    comp.literalTable.put (value, this);
    this.type = type;
  }

  void compile (Compilation comp)
  {
    comp.method.compile_getstatic (field);
    if (field == comp.literalsField)
      {
	comp.method.compile_push_int (index);
	comp.method.compile_array_load (comp.scmObjectType);
	comp.method.maybe_compile_checkcast (type);
      }
  }

  /** Emit code to re-create this Literal's value, an Object array. */
  void emitArray (Compilation comp, Type element_type)
  {
    Object[] array = (Object[]) value;
    int len = array.length;
    comp.method.compile_push_int (len);
    comp.method.compile_new_array (element_type);
    flags |= Literal.ALLOCATED;
    for (int i = 0;  i < len;  i++)
      {
	comp.method.compile_dup (1);
	comp.method.compile_push_int (i);
	comp.emitLiteral (array[i]);
	// Stack contents:  ..., array, array, i, array[i]
	comp.method.compile_array_store (comp.scmObjectType);
	// Stack contents:  ..., array
      }
  }

  void emit (Compilation comp, boolean ignore)
  {
    if ((flags & ALLOCATED) != 0 && ! (value instanceof String))
      {
	if ((flags & ASSIGNED) == 0 || field == null)
	  throw new Error ("internal error in Literal.emit");
	if (! ignore)
	  comp.method.compile_getstatic (field);
	return;
      }
    if (value instanceof Compilable)
      ((Compilable) value).emit (this, comp);
    else if (value instanceof Integer)
      {
	comp.method.compile_new (comp.javaIntegerType);
	comp.method.compile_dup (comp.javaIntegerType);
	comp.method.compile_push_int (((Integer)value).intValue ());
	comp.method.compile_invoke_nonvirtual (comp.initIntegerMethod);
      }
    else if (value instanceof StringBuffer)
      {
	comp.method.compile_new (comp.scmStringType);
	comp.method.compile_dup (comp.scmStringType);
	StringBuffer string = (StringBuffer) value;
	comp.method.compile_push_string (value.toString ());
	comp.method.compile_invoke_nonvirtual (comp.initStringBufferMethod);
      }
    else if (value instanceof String)
      {
	comp.method.compile_push_string (value.toString ());
      }
    else if (value instanceof Symbol[])
      emitArray (comp, comp.scmSymbolType);
    else if (value instanceof Object[])
      emitArray (comp, comp.scmObjectType);
    else
      {
	System.err.print ("Unimplemented compileConstant for ");
	System.err.println (value.getClass ());
	comp.method.compile_getstatic (Compilation.undefinedConstant);
      }
    flags |= ALLOCATED|INITIALIZED;
    if (field != null && (flags & ASSIGNED) == 0)
      {
	if (! ignore)
	  comp.method.compile_dup (Compilation.scmPairType);
	comp.method.compile_putstatic (field);
	flags |= ASSIGNED;
      }
    else if (ignore)
      comp.method.compile_pop (1);
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

