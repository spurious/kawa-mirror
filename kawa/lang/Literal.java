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
  static final int ALLOCATING = 1;

  /** Bit in flags used to indicate the value has been allocated.
   * I.e. it has been allocated, but it is not necessarily initialized. */
  static final int ALLOCATED = 2;

  /** Bit in flags indicates that the value has been fully initialized. */
  static final int INITIALIZED = 4;

  /* The ASSIGNED flag is set iff code has been emitted to assign the
   * value of the literal to the Field. */
  static final int ASSIGNED = 8;

  /** The static final field that contains the value of the literal. */
  Field field;

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
  }

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
      }
  }

  void emit (Compilation comp, boolean ignore)
  {
    if ((flags & ALLOCATED) != 0)
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
	flags |= ALLOCATED|INITIALIZED;
      }
    else if (value instanceof StringBuffer)
      {
	comp.method.compile_new (comp.scmStringType);
	comp.method.compile_dup (comp.scmStringType);
	StringBuffer string = (StringBuffer) value;
	comp.method.compile_push_string (value.toString ());
	comp.method.compile_invoke_static (comp.initStringBufferMethod);
	flags |= Literal.ALLOCATED|Literal.INITIALIZED;
      }
    else
      {
	System.err.print ("Unimplemented compileConstant for ");
	System.err.println (value.getClass ());
	comp.method.compile_getstatic (Compilation.undefinedConstant);
      }
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
}

