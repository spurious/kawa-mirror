// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/* Represents the contents of a standard "Code" attribute.
 * @author      Per Bothner
 */

public class CodeAttr extends Attribute implements AttrContainer
{
  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
  { this.attributes = attributes; }
  LineNumbersAttr lines;
  LocalVarsAttr locals;

  private Type[] stack_types;
  int SP;  // Current stack size (in "words")
  private int max_stack;
  private int max_locals;
  int PC;
  byte[] code;

  /* The exception handler table, as a vector of quadruples
     (start_pc, end_pc, handler_pc, catch_type).
     Only the first exception_table_length quadrules are defined. */
  short[] exception_table;

  /* The number of (defined) exception handlers (i.e. quadruples)
     in exception_table. */
  int exception_table_length;

  /* A chain of labels.  Unsorted, except that the Label with
     the lowest element in fixups must be the first one. */
  Label labels;

  /** The stack of currently active conditionals. */
  IfState if_stack;

  /** The stack of currently active try statements. */
  TryState try_stack;

  public final Method getMethod() { return (Method) getContainer(); }

  public final ConstantPool getConstants ()
  {
    return getMethod().classfile.constants;
  }

  /* True if we cannot fall through to bytes[PC] -
     the previous instruction was an uncondition control transfer.  */
  boolean unreachable_here;
  /** True if control could reach here. */
  public boolean reachableHere () { return !unreachable_here; }

  /** Get the maximum number of words on the operand stack in this method. */
  public int getMaxStack() { return max_stack; }
  /** Get the maximum number of local variable words in this method. */
  public int getMaxLocals() { return max_locals; }

  /** Set the maximum number of words on the operand stack in this method. */
  public void setMaxStack(int n) { max_stack = n; }
  /** Set the maximum number of local variable words in this method. */
  public void setMaxLocals(int n) { max_locals = n; }

  /** Get the code (instruction bytes) of this method.
    * Does not make a copy. */
  public byte[] getCode() { return code; }
  /** Set the code (instruction bytes) of this method.
    * @param code the code bytes (which are not copied).
    * Implicitly calls setCodeLength(code.length). */
  public void setCode(byte[] code) { this.code = code; this.PC = code.length; }
  /** Set the length the the code (instruction bytes) of this method.
    * That is the number of current used bytes in getCode().
    * (Any remaing bytes provide for future growth.) */
  public void setCodeLength(int len) { PC = len; }
  /** Set the current lengthof the code (instruction bytes) of this method. */
  public int getCodeLength() { return PC; }

  public CodeAttr (Method meth)
  {
    super ("Code");
    setContainer(meth);
    setNext(meth.getAttributes());
    meth.setAttributes(this);
  }

  public final void reserve (int bytes)
  {
    if (code == null)
      code = new byte[100+bytes];
    else if (PC + bytes > code.length)
      {
	byte[] new_code = new byte[2 * code.length + bytes];
	System.arraycopy (code, 0, new_code, 0, PC);
	code = new_code;
      }

    while (labels != null && labels.fixups != null) {
      int oldest_fixup = labels.fixups[0];
      int threshold = unreachable_here ? 30000 : 32000;
      if (PC + bytes - oldest_fixup > threshold)
	labels.emit_spring (this);
      else
	break;
    }
  }

  /**
   * Write an 8-bit byte to the current code-stream.
   * @param i the byte to write
   */
  public final void put1(int i)
  {
    code[PC++] = (byte) i;
    unreachable_here = false;
  }

  /**
   * Write a 16-bit short to the current code-stream
   * @param i the value to write
   */
  public final void put2(int i)
  {
    code[PC++] = (byte) (i >> 8);
    code[PC++] = (byte) (i);
    unreachable_here = false;
  }
  /**
   * Write a 32-bit int to the current code-stream
   * @param i the value to write
   */
  public final void put4(int i)
  {
    code[PC++] = (byte) (i >> 24);
    code[PC++] = (byte) (i >> 16);
    code[PC++] = (byte) (i >> 8);

    code[PC++] = (byte) (i);
    unreachable_here = false;
  }

  public final void putIndex2 (CpoolEntry cnst)
  {
    put2(cnst.index);
  }

  public final void putLineNumber (int linenumber)
  {
    if (lines == null)
      lines = new LineNumbersAttr(this);
    lines.put(linenumber, PC);
  }

  public final void pushType(Type type)
  {
    if (type == Type.void_type)
      throw new Error ("pushing void type onto stack");
    if (stack_types == null)
      stack_types = new Type[20];
    else if (SP + 1 >= stack_types.length) {
      Type[] new_array = new Type[2 * stack_types.length];
      System.arraycopy (stack_types, 0, new_array, 0, SP);
      stack_types = new_array;
    }
    if (type.size == 8)
      stack_types[SP++] = Type.void_type;
    stack_types[SP++] = type;
    if (SP > max_stack)
      max_stack = SP;
  }

  public final Type popType ()
  {
    if (SP <= 0)
      throw new Error("popType called with empty stack");
    Type type = stack_types[--SP];
    if (type.size == 8)
      if (popType () != Type.void_type)
	throw new Error("missing void type on stack");
    return type;
  }

  public final Type topType ()
  {
    return stack_types[SP - 1];
  }

  /** Compile code to pop values off the stack (and ignore them).
   * @param nvalues the number of values (not words) to pop
   */
  public void emitPop (int nvalues)
  {
    for ( ; nvalues > 0;  --nvalues)
      {
        reserve(1);
	Type type = popType();
	if (type.size > 4)
	  put1(88);  // pop2
	else if (nvalues > 1)
	  { // optimization:  can we pop 2 4-byte words using a pop2
	    Type type2 = popType();
	    if (type2.size > 4)
	      {
		put1(87);  // pop
		reserve(1);
	      }
	    put1(88);  // pop2
	    --nvalues;
	  }
	else
	  put1(87); // pop
      }
  }

  public void emitSwap ()
  {
    reserve(1);
    Type type1 = popType();
    Type type2 = popType();
    if (type1.size > 4 || type2.size > 4)
      throw new Error ("emitSwap:  not allowed for long or double");
    pushType(type1);
    put1(95);  // swap
    pushType(type2);
  }

  /** Compile code to duplicate with offset.
   * @param size the size of the stack item to duplicate (1 or 2)
   * @param offset where to insert the result (must be 0, 1, or 2)
   * The new words get inserted at stack[SP-size-offset]
   */
  public void emitDup (int size, int offset)
  {
    if (size == 0)
      return;
    reserve(1);
    // copied1 and (optionally copied2) are the types of the duplicated words
    Type copied1 = popType ();
    Type copied2 = null;
    if (size == 1)
      {
	if (copied1.size > 4)
	  throw new Error ("using dup for 2-word type");
      }
    else if (size != 2)
      throw new Error ("invalid size to emitDup");
    else if (copied1.size <= 4)
      {
	copied2 = popType();
	if (copied2.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }

    int kind;
    // These are the types of the words (in any) that are "skipped":
    Type skipped1 = null;
    Type skipped2 = null;
    if (offset == 0)
      {
	kind = size == 1 ? 89 : 92;  // dup or dup2
      }
    else if (offset == 1)
      {
	kind = size == 1 ? 90 : 93; // dup_x1 or dup2_x1
	skipped1 = popType ();
	if (skipped1.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }
    else if (offset == 2)
      {
	kind = size == 1 ? 91 : 94; // dup_x2 or dup2_x2
	skipped1 = popType();
	if (skipped1.size <= 4)
	  {
	    skipped2 = popType();
	    if (skipped2.size > 4)
	      throw new Error ("dup will cause invalid types on stack");
	  }
      }
    else
      throw new Error ("emitDup:  invalid offset");

    put1(kind);
    if (copied2 != null)
      pushType(copied2);
    pushType(copied1);
    if (skipped2 != null)
      pushType(skipped2);
    if (skipped1 != null)
      pushType(skipped1);
    if (copied2 != null)
      pushType(copied2);
    pushType(copied1);
  }

  /**
   * Compile code to duplicate the top 1 or 2 words.
   * @param size number of words to duplicate
   */
  public void emitDup (int size)
  {
    emitDup(size, 0);
  }

  public void emitDup (Type type)
  {
    emitDup(type.size > 4 ? 2 : 1, 0);
  }

  public void enterScope (Scope scope)
  {
    locals.enterScope(scope);
  }

  public Scope pushScope () {
    Scope scope = new Scope ();
    scope.start_pc = PC;
    if (locals == null)
      locals = new LocalVarsAttr(this);
    locals.enterScope(scope);
    if (locals.parameter_scope == null) 
      locals.parameter_scope= scope;
    return scope;
  }


  public Scope popScope () {
    Scope scope = locals.current_scope;
    locals.current_scope = scope.parent;
    scope.end_pc = PC;
    for (Variable var = scope.vars; var != null; var = var.next) {
      if (var.isSimple () && ! var.dead ())
	var.freeLocal(this);
    }
    return scope;
  }

  /** Get the index'th parameter. */
  public Variable getArg (int index)
  {
    return locals.parameter_scope.find_var (index);
  }

  /**
   * Search by name for a Variable
   * @param name name to search for
   * @return the Variable, or null if not found (in any scope of this Method).
   */
  Variable lookup (String name)
  {
    Scope scope = locals.current_scope;
    for (; scope != null;  scope = scope.parent)
      {
	Variable var = scope.lookup (name);
	if (var != null)
	  return var;
      }
    return null;
  }

  /** Add a new local variable (in the current scope).
   * @param type type of the new Variable.
   * @return the new Variable. */
  public Variable addLocal (Type type)
  {
    return locals.current_scope.addVariable(this, type, null);
  }

  /** Add a new local variable (in the current scope).
   * @param type type of the new Variable.
   * @param name name of the new Variable.
   * @return the new Variable. */
  Variable addLocal (Type type, String name)
  {
    return locals.current_scope.addVariable (this, type, name);
  }

  public final void emitPushInt(int i)
  {
    reserve(3);
    if (i >= -1 && i <= 5)
      put1(i + 3);  // iconst_m1 .. iconst_5
    else if (i >= -128 && i < 128)
      {
	put1(16); // bipush
	put1(i);
      }
    else if (i >= -32768 && i < 32768)
      {
	put1(17); // sipush
	put2(i);
      }
    else
      {
	int j = getConstants().addInt(i).index;
	if (j < 256)
	  {
	    put1(18); // ldc1
	    put1(j);
	  }
	else
	  {
	    put1(19); // ldc2
	    put2(j);
	  }
      }
    pushType(Type.int_type);
  }

  public void emitPushLong (long i)
  {
    if (i == 0 || i == 1)
      {
	reserve(1);
	put1 (9 + (int) i);  // lconst_0 .. lconst_1
      }
    else if ((long) (int) i == i)
      {
	emitPushInt ((int) i);
	reserve(1);
	popType();
	put1 (133); // i2l
      }
    else
      {
	reserve(3);
	int j = getConstants().addLong(i).index;
      	put1 (20); // ldc2w
	put2 (j);
      }
    pushType(Type.double_type);
  }

  public void emitPushDouble (double x)
  {
    if (x == 0.0)
      {
	reserve(1);
	put1 (14);  // dconst_0
      }
    else if (x == 1.0)
      {
	reserve(1);
	put1 (15);  // dconst_1
      }
    else if (x >= -128.0 && x < 128.0
	     && (double)(int)x == x)
      {
	// Saves space in the constant pool
	// Probably faster, at least on modern CPUs.
	emitPushInt ((int) x);
	reserve(1);
	popType();
	put1 (135); // i2d
      }
    else
      {
	reserve(3);
	int j = getConstants().addDouble(x).index;
      	put1(20); // ldc2w
	put2(j);
      }
    pushType(Type.long_type);
  }

  public final void emitPushString (String str)
  {
    reserve(3);
    int index = getConstants().addString(str).index;
    if (index < 256)
      {
	put1(18); // ldc1
	put1(index);
      }
    else
      {
	put1(19); // ldc2
	put2(index);
      }
    pushType(Type.string_type);
  }

  public void emitPushNull ()
  {
    reserve(1);
    put1(1);  // aconst_null
    pushType(Type.pointer_type);
  }

  void emitNewArray (int type_code)
  {
    reserve(2);
    put1(188);  // newarray
    put1(type_code);
  }

  public final void emitArrayLength ()
  {
    reserve(1);
    put1(190);  // arraylength
    pushType(Type.int_type);
  }

  /**
   * Invoke new on a class type.
   * Does not call the constructor!
   * @param type the desired new object type
   */
  public void emitNew (ClassType type)
  {
    reserve(3);
    put1(187); // new
    putIndex2(getConstants().addClass(type));
    pushType(type);
  }

  /** Compile code to allocate a new array.
   * The size shold have been already pushed on the stack.
   * @param type type of the array elements
   */
  public void emitNewArray (Type element_type)
  {
    popType();
    if (element_type == Type.byte_type)
      emitNewArray (8);
    else if (element_type == Type.short_type)
      emitNewArray (9);
    else if (element_type == Type.int_type)
      emitNewArray (10);
    else if (element_type == Type.long_type)
      emitNewArray (11);
    else if (element_type == Type.float_type)
      emitNewArray (6);
    else if (element_type == Type.double_type)
      emitNewArray (7);
    else if (element_type == Type.boolean_type)
      emitNewArray (4);
    else if (element_type == Type.char_type)
      emitNewArray (5);
    else if (element_type instanceof ClassType)
      {
	reserve(3);
	put1(189); // anewarray
	putIndex2(getConstants().addClass((ClassType) element_type));
      }
    else
      throw new Error ("unimplemented type in emitNewArray");
    pushType(Type.pointer_type);
  }

  /**
   * Comple code to push the contents of a local variable onto the statck.
   * @param var The variable whose contents we want to push.
   */
  public final void emitLoad (Variable var)
  {
    if (var.dead())
      throw new Error("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple())
      throw new Error ("attempting to load from unassigned variable "+var
		       +" simple:"+var.isSimple()+", offset: "+offset);
    Type type = var.getType().promote();
    int kind;
    reserve(4);
    if (type == Type.int_type)
      kind = 0; // iload??
    else if (type == Type.long_type)
      kind = 1; // lload??
    else if (type == Type.float_type)
      kind = 2; // float??
    else if (type == Type.double_type)
      kind = 3; // dload??
    else
      kind = 4; // aload??
    if (offset <= 3)
      put1(26 + 4 * kind + offset);  // [ilfda]load_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1(196); // wide
	    put1(offset >> 8);
	  }
	put1(21 + kind);  // [ilfda]load
	put1(offset);
      }
    pushType(var.getType());
  }

  public void emitStore (Variable var)
  {
   if (var.dead ())
      throw new Error ("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple ())
      throw new Error ("attempting to store in unassigned variable");
    Type type = var.getType().promote ();
    int kind;
    reserve(4);
    popType();
    if (type == Type.int_type)
      kind = 0; // istore??
    else if (type == Type.long_type)
      kind = 1; // lstore??
    else if (type == Type.float_type)
      kind = 2; // float??
    else if (type == Type.double_type)
      kind = 3; // dstore??
    else
      kind = 4; // astore??
    if (offset <= 3)
      put1(59 + 4 * kind + offset);  // [ilfda]store_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1(196); // wide
	    put1(offset >> 8);
	  }
	put1(54 + kind);  // [ilfda]store
	put1(offset);
      }
  }


  private final void emitFieldop (Field field, int opcode)
  {
    reserve(3);
    put1(opcode);
    putIndex2(getConstants().addFieldRef(field));
  }

  /** Compile code to get a static field value.
   * Stack:  ... => ..., value */

  public final void emitGetStatic(Field field)
  {
    pushType(field.type);
    emitFieldop (field, 178);  // getstatic
  }

  /** Compile code to get a non-static field value.
   * Stack:  ..., objectref => ..., value */

  public final void emitGetField(Field field)
  {
    popType();
    pushType(field.type);
    emitFieldop(field, 180);  // getfield
  }

  /** Compile code to put a static field value.
   * Stack:  ..., value => ... */

  public final void emitPutStatic (Field field)
  {
    popType();
    emitFieldop(field, 179);  // putstatic
  }

  /** Compile code to put a non-static field value.
   * Stack:  ..., objectref, value => ... */

  public final void emitPutField (Field field)
  {
    popType();
    popType();
    emitFieldop(field, 181);  // putfield
  }


  final void emitTransfer (Label label, int opcode)
  {
    put1(opcode);
    label.emit(this);
  }

  /** Compile an unconditional branch (goto) or a jsr.
   * @param label target of the branch (must be in this method).
   */
  public final void emitGoto (Label label, int opcode)
  {
    reserve(5);
    if (label.defined ())
      {
	int delta = label.position - PC;
	if (delta < -32768)
	  {
	    put1(opcode-167);  // goto_w or jsr_w
	    put4(delta);
	  }
	else
	  {
	    put1(opcode); // goto or jsr
	    put2(delta);
	  }
      }
    else
      emitTransfer (label, opcode); // goto label or jsr label
  }

  /** Compile an unconditional branch (goto).
   * @param label target of the branch (must be in this method).
   */
  public final void emitGoto (Label label)
  {
    emitGoto(label, 167);
    unreachable_here = true;
  }

  public void emitRet (Variable var)
  {
    int offset = var.offset;
    if (offset < 256)
      {
	put1(169);  // ret
	put1(offset);
      }
    else
      {
	put1(196);  // wide
	put1(169);  // ret
	put2(offset);
      }
  }

  /** Compile start of else clause. */
  public final void emitElse ()
  {
    Label else_label = if_stack.end_label;
    Label end_label = new Label (this);
    if_stack.end_label = end_label;
    if (reachableHere ())
      {
	int stack_growth = SP-if_stack.start_stack_size;
	if_stack.then_stacked_types = new Type[stack_growth];
	System.arraycopy (stack_types, if_stack.start_stack_size,
			  if_stack.then_stacked_types, 0, stack_growth);
	emitGoto (end_label);
      }
    while (SP != if_stack.start_stack_size)
      popType();
    else_label.define (this);
    if_stack.doing_else = true;    
  }

  /** Compile end of conditional. */
  public final void emitFi ()
  {
    boolean make_unreachable = false;
    if (! if_stack.doing_else)
      { // There was no 'else' clause.
	if (reachableHere ()
	    && SP != if_stack.start_stack_size)
	  throw new Error ("then clause grows stack with no else clause");
      }
    else if (if_stack.then_stacked_types != null)
      {
	int then_clause_stack_size
	  = if_stack.start_stack_size + if_stack.then_stacked_types.length;
	if (! reachableHere ())
	  {
	    System.arraycopy (if_stack.then_stacked_types, 0,
			      stack_types, if_stack.start_stack_size,
			      if_stack.then_stacked_types.length);
	    SP = then_clause_stack_size;
	  }
	else if (SP != then_clause_stack_size)
	  throw new Error ("SP at end of 'then' was " +
			   then_clause_stack_size
			   + " while SP at end of 'else' was " + SP);
      }
    else if (unreachable_here)
      make_unreachable = true;

    if_stack.end_label.define (this);
    if (make_unreachable)
      unreachable_here = true;
    // Pop the if_stack.
    if_stack = if_stack.previous;
  }

  public void emitCheckcast (Type type)
  {
    reserve(3);
    popType();
    put1(192);  // checkcast
    if (type instanceof ArrayType)
      {
	ArrayType atype = (ArrayType) type;
	CpoolUtf8 name = getConstants().addUtf8(atype.signature);
	putIndex2(getConstants().addClass(name));
      }
    else if (type instanceof ClassType)
      {
	putIndex2(getConstants().addClass((ClassType) type));
      }
    else
      throw new Error ("unimplemented type in compile_checkcast");
    pushType(type);
  }

  public final void emitThrow ()
  {
    popType();
    reserve(1);
    put1 (191);  // athrow
    unreachable_here = true;
  }

  /**
   * Compile a method return.
   */
  public final void emitReturn ()
  {
    reserve(1);
    if (getMethod().getReturnType() == Type.void_type)
      {
	put1(177); // return
	return;
      }
    Type type = popType();
    if (type == Type.int_type
	|| type == Type.short_type
	|| type == Type.byte_type
	|| type == Type.boolean_type
	|| type == Type.char_type)
      put1(172); // ireturn
    else if (type == Type.long_type)
      put1(173); // lreturn
    else if (type == Type.float_type)
      put1(174); // freturn
    else if (type == Type.double_type)
      put1(175); // dreturn
    else if (type == Type.void_type)
      throw new Error ("returning void type");
    else
      put1(176); // arreturn
  }

  /** Add an exception handler. */
  public void addHandler (int start_pc, int end_pc,
			  int handler_pc, int catch_type)
  {
    int index = 4 * exception_table_length;
    if (exception_table == null)
      {
	exception_table = new short[20];
      }
    else if (exception_table.length <= index)
      {
	short[] new_table = new short[2 * exception_table.length];
	System.arraycopy(exception_table, 0, new_table, 0, index);
	exception_table = new_table;
      }
    exception_table[index++] = (short) start_pc;
    exception_table[index++] = (short) end_pc;
    exception_table[index++] = (short) handler_pc;
    exception_table[index++] = (short) catch_type;
    exception_table_length++;
  }

  /** Add an exception handler. */
  public void addHandler (int start_pc, int end_pc, int handler_pc,
			  ClassType catch_type, ConstantPool constants)
  {
    int catch_type_index;
    if (catch_type == null)
      catch_type_index = 0;
    else
      catch_type_index = constants.addClass(catch_type).index;
    addHandler(start_pc, end_pc, handler_pc, catch_type_index);
  }


  public void emitTryStart(boolean has_finally)
  {
    TryState try_state = new TryState(this);
    if (has_finally)
      try_state.finally_subr = new Label(this);
  }

  public void emitTryEnd()
  {
    if (try_stack.end_label == null)
      {
	try_stack.end_label = new Label(this);
	if (try_stack.finally_subr != null)
	  emitGoto(try_stack.finally_subr, 168);  // jsr
	if (reachableHere())
	  emitGoto(try_stack.end_label);
	try_stack.end_pc = PC;
      }
  }

  public void emitCatchStart(Variable var)
  {
    emitTryEnd();
    if (try_stack.try_type != null)
      {
	emitCatchEnd();
      }
    ClassType type = var == null ? null : (ClassType) var.getType();
    try_stack.try_type = type;
    addHandler(try_stack.start_pc, try_stack.end_pc,
	       PC, type, getConstants());
    if (var != null)
      {
	pushType(type);
	emitStore(var);
      }
  }

  public void emitCatchEnd()
  {
    if (reachableHere())
      {
	if (try_stack.finally_subr != null)
	  emitGoto(try_stack.finally_subr, 168); // jsr
	emitGoto(try_stack.end_label);
      }
    popScope();
    try_stack.try_type = null;
  }

  public void emitFinallyStart()
  {
    emitTryEnd();
    if (try_stack.try_type != null)
      {
	emitCatchEnd();
      }
    try_stack.end_pc = PC;

    pushScope();
    Type except_type = Type.pointer_type;
    Variable except = addLocal(except_type);
    emitCatchStart(null);
    pushType(except_type);
    emitStore(except);
    emitGoto(try_stack.finally_subr, 168); // jsr
    emitLoad(except);
    emitThrow();
    
    //emitCatchEnd();
    
    //if (try_stack.finally_subr == null)
    // error();
    try_stack.finally_subr.define(this);
    Type ret_addr_type = Type.pointer_type;
    try_stack.finally_ret_addr = addLocal(ret_addr_type);
    pushType(ret_addr_type);
    emitStore(try_stack.finally_ret_addr);
  }

  public void emitFinallyEnd()
  {
    emitRet(try_stack.finally_ret_addr);
    unreachable_here = true;
    popScope();
    try_stack.finally_subr = null;
  }

  public void emitTryCatchEnd()
  {
    if (try_stack.finally_subr != null)
      emitFinallyEnd();
    try_stack.end_label.define(this);
    try_stack = try_stack.previous;
  }

  /* Make sure the label with oldest fixup is first in labels. */
  void reorder_fixups ()
  {
    Label prev = null;
    Label cur;
    Label oldest = null;
    Label oldest_prev = null;
    int oldest_fixup = PC+100;
    for (cur = labels;  cur != null;  cur = cur.next)
      {
	if (cur.fixups != null && cur.fixups[0] < oldest_fixup)
	  {
	    oldest = cur;
	    oldest_prev = prev;
	    oldest_fixup = cur.fixups[0];
	  }
	prev = cur;
      }
    if (oldest != labels)
      {
	oldest_prev.next = oldest.next;
	oldest.next = labels;
	labels = oldest;
      }
  }

  public void finalize_labels ()
  {
    while (labels != null && labels.fixups != null)
      labels.emit_spring (this);
    for (Label label = labels;  label != null;  label = label.next)
      {
	if (!label.defined ())
	  throw new Error ("undefined label");
	if (label.fixups != null)
	  throw new Error ("internal error: finalize_labels");
    }
  }

  public void assignConstants (ClassType cl)
  {
    super.assignConstants(cl);
    Attribute.assignConstants(this, cl);
  }

  public final int getLength()
  {
    return 12 + getCodeLength() + 8 * exception_table_length
      + Attribute.getLengthAll(this);
  }

  public void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeShort (max_stack);
    dstr.writeShort (max_locals);
    dstr.writeInt (PC);
    dstr.write (code, 0, PC);

    dstr.writeShort (exception_table_length);
    int count = exception_table_length;
    for (int i = 0;  --count >= 0;  i += 4)
      {
	dstr.writeShort(exception_table[i]);
	dstr.writeShort(exception_table[i+1]);
	dstr.writeShort(exception_table[i+2]);
	dstr.writeShort(exception_table[i+3]);
      }

    Attribute.writeAll(this, dstr);
  }

  public void print (ClassTypeWriter dst) 
  {
    dst.print("Attribute \"");
    dst.print(getName());
    dst.print("\", length:");
    dst.print(getLength());
    dst.print(", max_stack:");
    dst.print(max_stack);
    dst.print(", max_locals:");
    dst.print(max_locals);
    dst.print(", code_length:");
    int length = getCodeLength();
    dst.println(length);
    disAssemble(dst, 0, length);
    if (exception_table_length > 0)
      {
	dst.print("Exceptions (count: ");
	dst.print(exception_table_length);
	dst.println("):");
	int count = exception_table_length;
	for (int i = 0;  --count >= 0;  i += 4)
	  {
	    dst.print("  start: ");
	    dst.print(exception_table[i] & 0xffff);
	    dst.print(", end: ");
	    dst.print(exception_table[i+1] & 0xffff);
	    dst.print(", handler: ");
	    dst.print(exception_table[i+2] & 0xffff);
	    dst.print(", type: ");
	    int catch_type_index = exception_table[i+3] & 0xffff;
	    if (catch_type_index == 0)
	      dst.print("0 /* finally */");
	    else
	      {
		dst.printOptionalIndex(catch_type_index);
		dst.printConstantTersely(catch_type_index, ConstantPool.CLASS);
	      }
	    dst.println();
	  }
      }
    dst.printAttributes(this);
  }

  public void disAssemble (ClassTypeWriter dst, int offset, int length)
  {
    boolean wide = false;
    for (int i = offset;  i < length; )
      {
	int oldpc = i++;
	int op = code[oldpc] & 0xff;
	String str = Integer.toString(oldpc);
	int printConstant = 0;
	int j = str.length();
	while (++j <= 3) dst.print(' ');
	dst.print(str);
	dst.print(": ");
	if (op < 120)
	  {
	    if (op < 87)
	      {
		if (op < 3)  print("nop;aconst_null;iconst_m1;", op, dst);
		else if (op < 9) { dst.print("iconst_");  dst.print(op-3); }
		else if (op < 16) // op >= 9 [lconst_0] && op <= 15 [dconst_1]
		  {
		    char typ;
		    if (op < 11) { typ = 'l';  op -= 9; }
		    else if (op < 14) { typ = 'f';  op -= 11; }
		    else { typ = 'd';  op -= 14; }
		    dst.print(typ);  dst.print("const_");  dst.print(op);
		  }
		else if (op < 21)
		  {
		    if (op < 18)  // op >= 16 [bipush] && op <= 17 [sipush]
		      {
			print("bipush ;sipush ;", op-16, dst);
			int constant;
			if (op == 16)  constant = code[i++];
			else { constant = (short) readUnsignedShort(i);  i+=2;}
			dst.print(constant);
		      }
		    else // op >= 18 [ldc] && op <= 20 [ldc2_w]
		      {
			printConstant = op == 18 ? 1 : 2;
			print("ldc;ldc_w;ldc2_w;", op-18, dst);
		      }
		  }
		else // op >= 21 && op < 87
		  {
		    String load_or_store;
		    if (op < 54) { load_or_store = "load"; }
		    else { load_or_store = "store"; op -=(54-21); }
		    int index;  // -2 if array op;  -1 if index follows
		    if (op < 26) { index = -1; op -= 21; }
		    else if (op < 46) { op -= 26;  index = op % 4;  op >>= 2; }
		    else { index = -2; op -= 46; }
		    dst.print("ilfdabcs".charAt(op));
		    if (index == -2) dst.write('a');
		    dst.print(load_or_store);
		    if (index >= 0) { dst.write('_');  dst.print(index); }
		    else if (index == -1)
		      {
			if (wide) { index = readUnsignedShort(i); index += 2; }
			else { index = code[i] & 0xff; i++; }
			wide = false;
			dst.print(' ');
			dst.print(index);
		      }
		  }
	      }
	    else // op >= 87 && op < 120
	      {
		if (op < 96)
		  print("pop;pop2;dup;dup_x1;dup_x2;dup2;dup2_x1;dup2_x2;swap;"
			, op-87, dst);
		else // op >= 96 [iadd] && op <= 119 [dneg]
		  {
		    dst.print("ilfda".charAt((op-96) % 4));
		    print("add;sub;mul;div;rem;neg;", (op-96)>>2, dst);
		  }
	      }
	  }
	else // op >= 120
	  {
	    if (op < 170)
	      {
		if (op < 132) // op >= 120 [ishl] && op <= 131 [lxor]
		  {
		    dst.print((op & 1) == 0 ? 'i' : 'l');
		    print("shl;shr;ushr;and;or;xor;", (op-120)>>1, dst);
		  }
		else if (op == 132) // iinc
		  {
		    int var_index;
		    int constant;
		    dst.print("iinc");
		    if (! wide)
		      {
			var_index = 0xff & code[i++];
			constant = code[i++];
		      }
		    else
		      {
			var_index = readUnsignedShort(i);
			i += 2;
			constant = (short) readUnsignedShort(i);
			i += 2;
			wide = false;
		      }
		    dst.print(' ');  dst.print(var_index);
		    dst.print(' ');  dst.print(constant);
		  }
		else if (op < 148) // op >= 133 [i2l] && op <= 147 [i2s]
		  {
		    dst.print("ilfdi".charAt((op-133) / 3));
		    dst.print('2');
		    dst.print("lfdifdildilfbcs".charAt(op-133));
		  }
		else if (op < 153) // op >= 148 [lcmp] && op <= 152 [dcmpg]
		  print("lcmp;fcmpl;fcmpg;dcmpl;dcmpg;", op-148, dst);
		else
		  {
		    if (op < 159)
		      {
			dst.print("if");
			print("eq;ne;lt;ge;gt;le;", op-153, dst);
		      }
		    else if (op < 167)
		      {
			if (op < 165) { dst.print("if_icmp"); }
			else { dst.print("if_acmp"); op -= 165-159; }
			print("eq;ne;lt;ge;gt;le;", op-159, dst);
		      }
		    else
		      print("goto;jsr;ret;", op-167, dst);
		    int delta = (short) readUnsignedShort(i);
		    i += 2;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
	      }
	    else
	      {
		if (op < 172) //  [tableswitch] or [lookupswitch]
		  {
		    i = (i + 3) & ~3; // skip 0-3 byte padding.
		    int code_offset = readInt(i);  i += 4;
		    if (op == 170)
		      {
			dst.print("tableswitch");
			int low = readInt(i);  i += 4;
			int high = readInt(i+4);  i += 4;
			dst.print(" low: "); dst.print(low);
			dst.print(" high: "); dst.print(high);
			dst.print(" default: "); dst.print(code_offset);
			for (;  low <= high;  low++)
			  {
			    code_offset = readInt(i);  i += 4;
			    dst.println();
			    dst.print("  ");  dst.print(low);
			    dst.print(": ");  dst.print(oldpc + code_offset); 
			  }
		      }
		    else
		      {
			dst.print("lookupswitch");
			int npairs = readInt(i);  i += 4;
			dst.print(" npairs: "); dst.print(npairs);
			dst.print(" default: "); dst.print(code_offset);
			while (--npairs >= 0)
			  {
			    int match = readInt(i);  i += 4;
			    code_offset = readInt(i);  i += 4;
			    dst.println();
			    dst.print("  ");  dst.print(match);
			    dst.print(": ");  dst.print(oldpc + code_offset); 
			  }
		      }
		  }
		else if (op < 178) // op >= 172 [ireturn] && op <= 177 [return]
		  {
		    if (op < 177) dst.print("ilfda".charAt(op-172));
		    dst.print("return");
		  }
		else if (op < 182) // op >= 178 [getstatic] && op <= 181 [putfield]
		  {
		    print("getstatic;putstatic;getfield;putfield;", op-178, dst);
		    printConstant = 2;
		  }
		else if (op < 186) // op >= 182 && op <= 185 [invoke*]
		  {
		    dst.print("invoke");
		    print("virtual;special;static;interface;", op-182, dst);
		    printConstant = 2;
		    if (op == 185) // invokeinterface
		      i += 2;
		  }
		else if (op < 196)
		  {
		    print("186;new;newarray;anewarray;arraylength;athrow;checkcast;instanceof;moniorenter;monitorexit;", op-186, dst);
		    if (op == 187 || op == 189 || op == 192 || op == 193)
		      printConstant = 2;
		    else if (op == 188)  // newarray
		      {
			int type = code[i++];
			dst.print(' ');
			if (type >= 4 && type <= 11)
			  print("boolean;char;float;double;byte;short;int;long;",
				type-4, dst);
			else
			  dst.print(type);
		      }
			
		  }
		else if (op == 196) // wide
		  {
		    dst.print("wide");
		    wide = true;
		  }
		else if (op == 197)
		  {
		    dst.print("multianewarray");
		    int index = readUnsignedShort(i);
		    i += 2;
		    dst.printConstantOperand(index);
		    int dims = 0xff & code[i++];
		    dst.print(' ');
		    dst.print(dims);
		  }
		else if (op < 200)
		  {
		    print("ifnull;ifnonnull;", op-198, dst);
		    int delta = (short) readUnsignedShort(i);
		    i += 2;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
		else if (op < 202)
		  {
		    print("goto_w;jsr_w;", op-200, dst);
		    int delta = readInt(i);
		    i += 4;
		    dst.print(' ');  dst.print(oldpc+delta);
		  }
		else
		  dst.print(op);
	      }
	  }
	if (printConstant > 0)
	  {
	    int index;
	    if (printConstant == 1) index = 0xff & code[i++];
	    else { index = readUnsignedShort(i);  i += 2; }
	    dst.printConstantOperand(index);
	  }
	dst.println();
      }
  }

  private int readUnsignedShort(int offset)
  {
    return ((0xff & code[offset]) << 8) | (0xff & code[offset+1]);
  }

  private int readInt(int offset)
  {
    return (readUnsignedShort(offset) << 16) | readUnsignedShort(offset+2);
  }

  /* Print the i'th ';'-delimited substring of str on dst. */
  private void print (String str, int i, PrintWriter dst)
  {
    int last = 0;
    int pos = -1;
    for (; i >= 0; i--)
      {
	last = ++pos;
	pos = str.indexOf(';', last);
      }
    dst.write(str, last, pos-last);
  }
}
