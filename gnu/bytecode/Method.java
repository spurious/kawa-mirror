// Copyright (c) 1997  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

public class Method implements AttrContainer {
  private String name;
  Type[] arg_types;
  
  Type return_type;
  int access_flags;
  int name_index; /* Index in constant table, or 0 if un-assigned */
  int signature_index; /* Index in constant table, or 0 if un-assigned */
  Method next;
  ClassType classfile;

  Attribute attributes;
  public final Attribute getAttributes () { return attributes; }
  public final void setAttributes (Attribute attributes)
  { this.attributes = attributes; }
  CodeAttr code;
  LocalVarsAttr locals;

  Method (ClassType clfile, int flags) {
     if (clfile.last_method == null)
       clfile.methods = this;
    else
      clfile.last_method.next = this;
    clfile.last_method = this;
    clfile.methods_count++;
    access_flags = flags;
    classfile = clfile;
  }

  public final void setStaticFlag (boolean is_static) {
    if (is_static)
      access_flags |= Access.STATIC;
    else
      access_flags ^= ~Access.STATIC;
  }

  public final boolean getStaticFlag () {
    return (access_flags & Access.STATIC) != 0;
  }

  public final ConstantPool getConstants ()
  {
    return classfile.constants;
  }
    
  public void enterScope (Scope scope)
  {
    if (locals == null)
      locals = new LocalVarsAttr(code);
    locals.enterScope(scope);
  }

  public Scope pushScope () {
    prepareCode(0);
    Scope scope = new Scope ();
    scope.start_pc = code.PC;
    enterScope(scope);
    return scope;
  }

  /** True if control could reach here. */
  public boolean reachableHere () { return !code.unreachable_here; }

  public Scope popScope () {
    Scope scope = locals.current_scope;
    locals.current_scope = scope.parent;
    scope.end_pc = code.PC;
    for (Variable var = scope.vars; var != null; var = var.next) {
      if (var.isSimple () && ! var.dead ())
	kill_local (var);
    }
    return scope;
  }

  /**
   * Search by name for a Variable
   * @param name name to search for
   * @return the Variable, or null if not found (in any scope of this Method).
   */
  Variable lookup (String name) {
    for (Scope scope = locals.current_scope; scope != null;  scope = scope.parent) {
      Variable var = scope.lookup (name);
      if (var != null)
	return var;
    }
    return null;
  }

  /** Assign a local variable to a given slot.
   * @param local the Variable to assign
   * @param slot the local variable slot desired
   * @return true iff we succeeded (i.e. the slot was unused) */
  public boolean assign_local (Variable local, int slot)
  {
    int size = local.getType().size > 4 ? 2 : 1;
    if (locals.used == null)
      locals.used = new Variable[20+size];
    else if (code.getMaxLocals() + size >= locals.used.length) {
      Variable[] new_locals = new Variable [2 * locals.used.length + size];
      System.arraycopy (locals.used, 0, new_locals, 0, code.getMaxLocals());
      locals.used = new_locals;
    }
    for (int j = 0; j < size; j++)
      {
	if (locals.used[slot+j] != null)
	  return false;
      }
    for (int j = 0; j < size; j++)
      locals.used[slot + j] = local;
    if (slot + size > code.getMaxLocals())
      code.setMaxLocals(slot + size);
    local.offset = slot;
    return true;
  }

  /**
   * Allocate slots for a local variable (or parameter).
   * @param local the variable we need to allocate
   * @return the index of the (first) slot.
   */
  public void allocate_local (Variable local)
  {
    for (int i = 0; ; i++)
      {
	if (assign_local (local, i))
	  {
	    return;
	  }
      }
  }

  /** Allocate a Code attribute, and prepare to generate code. */

  public void initCode ()
  {
    if (classfile.constants == null)
      classfile.constants = new ConstantPool();
    prepareCode(0);
    if (locals == null)
      locals = new LocalVarsAttr(code);
    if (locals.parameter_scope == null)
      locals.parameter_scope = pushScope();
  }

  public void init_param_slots ()
  {
    initCode ();
    if ((access_flags & Access.STATIC) == 0)
      addLocal (classfile).setParameter (true);
    int arg_count = arg_types.length;
    for (int i = 0;  i < arg_count;  i++) {
      addLocal (arg_types[i]).setParameter (true);
    }
  }

  /* Get the index'th parameter. */
  public Variable find_arg (int index) {
    return locals.parameter_scope.find_var (index);
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
  void kill_local (Variable var) {
    var.end_pc = code.PC;
    int size = var.getType().size > 4 ? 2 : 1;
    while (--size >= 0)
      locals.used [var.offset + size] = null;
  }

  /** Method that must be called before we generate any instructions.
    * Set so there is room for at least max_size bytes of code.
    */
  void prepareCode(int max_size)
  {
    if (code == null)
      code = new CodeAttr(this);
    code.reserve(max_size);
  }

  // This method should be called before we generate code for
  // an instruction (or sequence).
  // An upper bound of the intruction length is max_size.
  // deprecated!
  void instruction_start_hook (int max_size)
  {
    prepareCode(max_size);
  }

  final Type pop_stack_type () { return code.popType(); }
  final void push_stack_type (Type type) { code.pushType(type); }

  public void push_int_const (int i) { compile_push_int (i); } //old
  public void compile_push_int (int i) {
    prepareCode(3);
    if (i >= -1 && i <= 5)
      code.put1(i + 3);  // iconst_m1 .. iconst_5
    else if (i >= -128 && i < 128) {
      code.put1(16); // bipush
      code.put1(i);
    } else if (i >= -32768 && i < 32768) {
      code.put1(17); // sipush
      code.put2(i);
    } else {
      int j = getConstants().addInt(i).index;
      if (j < 256) {
	code.put1(18); // ldc1
	code.put1(j);
      } else {
	code.put1(19); // ldc2
	code.put2(j);
      }
    }
    push_stack_type (Type.int_type);
  }

  public void compile_push_long (long i)
  {
    if (i == 0 || i == 1)
      {
	prepareCode(1);
	code.put1 (9 + (int) i);  // lconst_0 .. lconst_1
      }
    else if ((long) (int) i == i)
      {
	compile_push_int ((int) i);
	prepareCode(1);
	pop_stack_type ();
	code.put1 (133); // i2l
      }
    else
      {
	prepareCode(3);
	int j = getConstants().addLong(i).index;
      	code.put1 (20); // ldc2w
	code.put2 (j);
      }
    push_stack_type (Type.double_type);
  }

  public void compile_push_double (double x)
  {
    if (x == 0.0)
      {
	prepareCode(1);
	code.put1 (14);  // dconst_0
      }
    else if (x == 1.0)
      {
	prepareCode(1);
	code.put1 (15);  // dconst_1
      }
    else if (x >= -128.0 && x < 128.0
	     && (double)(int)x == x)
      {
	// Saves space in the constant pool
	// Probably faster, at least on modern CPUs.
	compile_push_int ((int) x);
	prepareCode(1);
	pop_stack_type ();
	code.put1 (135); // i2d
      }
    else
      {
	prepareCode(3);
	int j = getConstants().addDouble(x).index;
      	code.put1(20); // ldc2w
	code.put2(j);
      }
    push_stack_type (Type.long_type);
  }

  public void compile_push_string (String str)
  {
    prepareCode(3);
    int index = getConstants().addString(str).index;
    if (index < 256)
      {
	code.put1(18); // ldc1
	code.put1(index);
      }
    else
      {
	code.put1(19); // ldc2
	code.put2(index);
      }
    push_stack_type (Type.string_type);
  }

  public void compile_push_null ()
  {
    prepareCode(1);
    code.put1(1);  // aconst_null
    push_stack_type (Type.pointer_type);
  }

  void compile_new_array (int type_code)
  {
    prepareCode(2);
    code.put1(188);  // newarray
    code.put1(type_code);
  }

  public final void compile_arraylength ()
  {
    prepareCode(1);
    code.put1(190);  // arraylength
    push_stack_type (Type.int_type);
  }

  /**
   * Invoke new on a class type.
   * Does not call the constructor!
   * @param type the desired new object type
   */
  public void compile_new (ClassType type)
  {
    prepareCode(3);
    code.put1(187); // new
    code.putIndex2(getConstants().addClass(type));
    push_stack_type (type);
  }

  /** Compile code to allocate a new array.
   * The size shold have been already pushed on the stack.
   * @param type type of the array elements
   */
  public void compile_new_array (Type element_type)
  {
    pop_stack_type ();
    if (element_type == Type.byte_type)
      compile_new_array (8);
    else if (element_type == Type.short_type)
      compile_new_array (9);
    else if (element_type == Type.int_type)
      compile_new_array (10);
    else if (element_type == Type.long_type)
      compile_new_array (11);
    else if (element_type == Type.float_type)
      compile_new_array (6);
    else if (element_type == Type.double_type)
      compile_new_array (7);
    else if (element_type == Type.boolean_type)
      compile_new_array (4);
    else if (element_type == Type.char_type)
      compile_new_array (5);
    else if (element_type instanceof ClassType)
      {
	prepareCode(3);
	code.put1(189); // anewarray
	code.putIndex2(getConstants().addClass((ClassType) element_type));
      }
    else
      throw new Error ("unimplemented type in compile_new_array");
    push_stack_type (Type.pointer_type);
  }

  public void compile_checkcast (Type type)
  {
    prepareCode(3);
    pop_stack_type ();
    code.put1(192);  // checkcast
    if (type instanceof ArrayType)
      {
	ArrayType atype = (ArrayType) type;
	CpoolUtf8 name = getConstants().addUtf8(atype.signature);
	code.putIndex2(getConstants().addClass(name));
      }
    else if (type instanceof ClassType)
      {
	code.putIndex2(getConstants().addClass((ClassType) type));
      }
    else
      throw new Error ("unimplemented type in compile_checkcast");
    push_stack_type (type);
  }

  public void maybe_compile_checkcast (Type type)
  {
    Type stack_type = code.topType();
    if (type != stack_type)  // FIXME rather simple-minded, but safe.
      compile_checkcast (type);
  }

  /** Store into an element of an array.
   * Must already have pushed the array reference, the index,
   * and the new value (in that order).
   * Stack:  ..., array, index, value => ...
   */
  public void compile_array_store (Type element_type)
  {
    prepareCode(1);
    pop_stack_type ();  // Pop new values
    pop_stack_type ();  // Pop index
    pop_stack_type ();  // Pop array reference
    if      (element_type == Type.byte_type)    code.put1(84);  // bastore
    else if (element_type == Type.short_type)   code.put1(86);  // sastore
    else if (element_type == Type.int_type)     code.put1(79);  // iastore
    else if (element_type == Type.long_type)    code.put1(80);  // lastore
    else if (element_type == Type.float_type)   code.put1(81);  // fastore
    else if (element_type == Type.double_type)  code.put1(82);  // dastore
    else if (element_type == Type.boolean_type) code.put1(84);  // bastore
    else if (element_type == Type.char_type)    code.put1(85);  // castore
    else                                        code.put1(83);  // aastore
  }

  /** Load an element from an array.
   * Must already have pushed the array and the index (in that order):
   * Stack:  ..., array, index => ..., value */
  public void compile_array_load (Type element_type)
  {
    prepareCode(1);
    pop_stack_type ();  // Pop index
    pop_stack_type ();  // Pop array reference
    if      (element_type == Type.byte_type)    code.put1(51);  // baload
    else if (element_type == Type.short_type)   code.put1(53);  // saload
    else if (element_type == Type.int_type)     code.put1(46);  // iaload
    else if (element_type == Type.long_type)    code.put1(47);  // laload
    else if (element_type == Type.float_type)   code.put1(48);  // faload
    else if (element_type == Type.double_type)  code.put1(49);  // daload
    else if (element_type == Type.boolean_type) code.put1(51);  // baload
    else if (element_type == Type.char_type)    code.put1(52);  // caload
    else                                        code.put1(50);  // aaload
    push_stack_type (element_type);
  }

  /**
   * Compile code to pop values off the stack (and ignore them).
   * @param nvalues the number of values (not words) to pop
   */
  public void compile_pop (int nvalues)
  {
    for ( ; nvalues > 0;  --nvalues)
      {
        prepareCode(1);
	Type type = pop_stack_type ();
	if (type.size > 4)
	  code.put1(88);  // pop2
	else if (nvalues > 1)
	  { // optimization:  can we pop 2 4-byte words using a pop2
	    Type type2 = pop_stack_type ();
	    if (type2.size > 4)
	      {
		code.put1(87);  // pop
		prepareCode(1);
	      }
	    code.put1(88);  // pop2
	    --nvalues;
	  }
	else
	  code.put1(87); // pop
      }
  }

  public void compile_swap ()
  {
    prepareCode(1);
    Type type1 = pop_stack_type ();
    Type type2 = pop_stack_type ();
    if (type1.size > 4 || type2.size > 4)
      throw new Error ("compile_swap:  not allowed for long or double");
    push_stack_type (type1);
    code.put1(95);  // swap
    push_stack_type (type2);
  }

  /** Compile code to duplicate with offset.
   * @param size the size of the stack item to duplicate (1 or 2)
   * @param offset where to insert the result (must be 0, 1, or 2)
   * The new words get inserted at stack[SP-size-offset]
   */
  public void compile_dup (int size, int offset)
  {
    if (size == 0)
      return;
    prepareCode(1);
    // copied1 and (optionally copied2) are the types of the duplicated words
    Type copied1 = pop_stack_type ();
    Type copied2 = null;
    if (size == 1)
      {
	if (copied1.size > 4)
	  throw new Error ("using dup for 2-word type");
      }
    else if (size != 2)
      throw new Error ("invalid size to compile_dup");
    else if (copied1.size <= 4)
      {
	copied2 = pop_stack_type ();
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
	skipped1 = pop_stack_type ();
	if (skipped1.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }
    else if (offset == 2)
      {
	kind = size == 1 ? 91 : 94; // dup_x2 or dup2_x2
	skipped1 = pop_stack_type ();
	if (skipped1.size <= 4)
	  {
	    skipped2 = pop_stack_type ();
	    if (skipped2.size > 4)
	      throw new Error ("dup will cause invalid types on stack");
	  }
      }
    else
      throw new Error ("compile_dup:  invalid offset");

    code.put1(kind);
    if (copied2 != null)
      push_stack_type (copied2);
    push_stack_type (copied1);
    if (skipped2 != null)
      push_stack_type (skipped2);
    if (skipped1 != null)
      push_stack_type (skipped1);
    if (copied2 != null)
      push_stack_type (copied2);
    push_stack_type (copied1);
  }

  /**
   * Compile code to duplicate the top 1 or 2 words.
   * @param size number of words to duplicate
   */
  public void compile_dup (int size)
  {
    compile_dup (size, 0);
  }

  public void compile_dup (Type type)
  {
    compile_dup (type.size > 4 ? 2 : 1, 0);
  }

  /**
   * Comple code to push the contents of a local variable onto the statck.
   * @param var The variable whose contents we want to push.
   */
  public void push_var (Variable var) { compile_push_value (var); }
  public void compile_push_value (Variable var) {
    if (var.dead ())
      throw new Error ("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple ())
      throw new Error ("attempting to load from unassigned variable "+var
+" simple:"+var.isSimple ()+", offset: "+offset);
    Type type = var.getType().promote ();
    int kind;
    prepareCode(4);
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
      code.put1(26 + 4 * kind + offset);  // [ilfda]load_[0123]
    else
      {
	if (offset >= 256)
	  {
	    code.put1(196); // wide
	    code.put1(offset >> 8);
	  }
	code.put1(21 + kind);  // [ilfda]load
	code.put1(offset);
      }
    push_stack_type (var.getType());
  }

  public void compile_store_value (Variable var)
  {
   if (var.dead ())
      throw new Error ("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple ())
      throw new Error ("attempting to store in unassigned variable");
    Type type = var.getType().promote ();
    int kind;
    prepareCode(4);
    pop_stack_type ();
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
      code.put1(59 + 4 * kind + offset);  // [ilfda]store_[0123]
    else
      {
	if (offset >= 256)
	  {
	    code.put1(196); // wide
	    code.put1(offset >> 8);
	  }
	code.put1(54 + kind);  // [ilfda]store
	code.put1(offset);
      }
  }

  public void compile_push_this ()
  {
    prepareCode(1);
    code.put1(42);  // aload_0
    push_stack_type (classfile);
  }

  /**
   * Convert the element on top of the stack to requested type
   */
  public final void compile_convert (Type type) {
    Type from = pop_stack_type();
    push_stack_type(from);
    compile_convert (from, type);
  }

  public final void compile_convert (Type from, Type to)
  {
    String to_sig = to.getSignature();
    String from_sig = from.getSignature();
    int op = -1;
    if (to_sig.length() == 1 || from_sig.length() == 1)
      {
	char to_sig0 = to_sig.charAt(0);
	char from_sig0 = to_sig.charAt(0);
	if (from.size < 4)
	  from_sig0 = 'I';
	if (to.size < 4)
	  {
	    compile_convert (from, Type.int_type);
	    from_sig0 = 'I';
	  }
	if (from_sig0 == to_sig0)
	  return;
	switch (from_sig0)
	  {
	  case 'I':
	    switch (to_sig0)
	      {
	        case 'B':  op = 145;  break;  // i2b
	        case 'C':  op = 146;  break;  // i2c
	        case 'S':  op = 147;  break;  // i2s
		case 'J':  op = 133;  break;  // i2l
		case 'F':  op = 134;  break;  // i2f
		case 'D':  op = 135;  break;  // i2d
	      }
	    break;
	  case 'J':
	    switch (to_sig0)
	      {
		case 'I':  op = 136;  break;  // l2i
		case 'F':  op = 137;  break;  // l2f
		case 'D':  op = 138;  break;  // l2d
	      }
	    break;
	  case 'F':
	    switch (to_sig0)
	      {
		case 'I':  op = 139;  break;  // f2i
		case 'J':  op = 140;  break;  // f2l
		case 'D':  op = 141;  break;  // f2d
	      }
	    break;
	  case 'D':
	    switch (to_sig0)
	      {
		case 'I':  op = 142;  break;  // d2i
		case 'J':  op = 143;  break;  // d2l
		case 'F':  op = 144;  break;  // d2f
	      }
	    break;
	  }
      }
    if (op < 0)
      throw new Error ("unsupported Method.compile_convert");
    prepareCode(1);
    pop_stack_type();
    code.put1(op);
    push_stack_type(to);
  }

  public final void compile_goto_ifeq (Label label, boolean invert)
  {
    Type type2 = pop_stack_type ().promote ();
    Type type1 = pop_stack_type ().promote ();
    prepareCode(4);
    int opcode;
    if (type1 == Type.int_type && type2 == Type.int_type)
      opcode = 159;  // if_cmpeq (inverted: if_icmpne)
    else if (type1 == Type.long_type && type2 == Type.long_type)
      {
	code.put1(148);   // lcmp
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1 == Type.float_type && type2 == Type.float_type)
      {
	code.put1(149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1 == Type.double_type && type2 == Type.double_type)
      {
	code.put1(149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1.getSignature().length() == 1
	     || type2.getSignature().length() == 1)
      throw new Error ("non-matching types to compile_goto_ifeq");
    else
      opcode = 165;  // if_acmpeq (inverted: if_acmpne)
    if (invert)
      opcode++;
    code.compileTransfer (label, opcode);
  }

  /** Compile a conditional transfer if 2 top stack elements are equal. */
  public final void compile_goto_ifeq (Label label)
  {
    compile_goto_ifeq (label, false);
  }
  /** Compile conditional transfer if 2 top stack elements are not equal. */
  public final void compile_goto_ifne (Label label)
  {
    compile_goto_ifeq (label, true);
  }

  /** Compile start of a conditional:  if (!(x OPCODE 0)) ...
   * The value of x must already have been pushed. */
  public final void compile_goto_if (int opcode)
  {
    IfState new_if = new IfState (code);
    pop_stack_type ();
    prepareCode(3);
    code.compileTransfer (new_if.end_label, opcode);
    new_if.start_stack_size = code.SP;
  }

  /** Compile start of conditional:  if (x != 0) */
  public final void compile_if_neq_0 ()
  {
    compile_goto_if (153); // ifeq
  }

  /** Compile start of a conditional:  if (!(x OPCODE y)) ...
   * The value of x and y must already have been pushed. */
  public final void compile_if_icmp(int opcode)
  {
    IfState new_if = new IfState (code);
    pop_stack_type ();
    pop_stack_type ();
    prepareCode(3);
    code.compileTransfer(new_if.end_label, opcode);
    new_if.start_stack_size = code.SP;
  }

  /* Compile start of a conditional:  if (x < y) ... */
  public final void compile_ifi_lt()
  {
    compile_if_icmp(162);  // if_icmpge
  }

  /** Compile start of a conditional:  if (x != y) ...
   * The values of x and y must already have been pushed. */
  public final void compile_ifneq ()
  {
    IfState new_if = new IfState (code);
    compile_goto_ifeq (new_if.end_label);
    new_if.start_stack_size = code.SP;
  }

  /** Compile start of else clause. */
  public final void compile_else ()  { code.compileElse(); }

  /** Compile end of conditional. */
  public final void compile_fi () { code.compileFi(); }

  /** Compile an unconditional branch (goto).
   * @param label target of the branch (must be in this method).
   */
  public final void compile_goto (Label label) { code.compileGoto(label); }

  /**
   * Compile a function return.
   */
  public final void compile_return () {
    prepareCode(1);
    if (return_type == Type.void_type) {
      code.put1(177); // return
      return;
    }
    Type type = pop_stack_type ();
    if (type == Type.int_type
	|| type == Type.short_type
	|| type == Type.byte_type
	|| type == Type.boolean_type
	|| type == Type.char_type)
      code.put1(172); // ireturn
    else if (type == Type.long_type)
      code.put1(173); // lreturn
    else if (type == Type.float_type)
      code.put1(174); // freturn
    else if (type == Type.double_type)
      code.put1(175); // dreturn
    else if (type == Type.void_type)
      throw new Error ("returning void type");
    else
      code.put1(176); // arreturn
  }

  /*
  public Scope scope_start ();
  public scope_end ();
  public Variable new_temp (String name);
  public Variable free_temp (Variable temp_var);

  public void push_long_const (long i);

  */

  // public final void compile_int_add () { code.put1(96); pop_stack_type ();}
  // public final void compile_long_add () { code.put1(97); pop_stack_type ();}
  // public final void compile_float_add () { code.put1(98); pop_stack_type ();}
  // public final void compile_double_add () { code.put1(99); pop_stack_type ();}
  public final void compile_add () { compile_binop (96); }
  public final void compile_sub () { compile_binop (100); }
  public final void compile_mul () { compile_binop (104); }
  public final void compile_div () { compile_binop (108); }
  public final void compile_rem () { compile_binop (112); }

  private final void compile_binop (int base_code) {
    prepareCode(1);
    Type type2 = pop_stack_type ().promote ();
    Type type1_raw = pop_stack_type ();
    Type type1 = type1_raw.promote ();
    if (type1 != type2)
      throw new Error ("non-matching types in binary operation");
    if (type1 == Type.int_type)
      code.put1(base_code);
    else if (type1 == Type.long_type)
      code.put1(base_code+1);
    else if (type1 == Type.float_type)
      code.put1(base_code+2);
    else if (type1 == Type.double_type)
      code.put1(base_code+3);
    else
      throw new Error ("bad type in binary operation");
    push_stack_type (type1_raw);
  }

  public void compile_primop (int opcode, int arg_count, Type retType)
  {
    prepareCode(1);
    while (-- arg_count >= 0)
      pop_stack_type ();
    code.put1(opcode);
    push_stack_type (retType);
  }

  public void compile_invoke_method (Method method, int opcode)
  {
    prepareCode(opcode == 185 ? 5 : 3);
    int arg_count = method.arg_types.length;
    boolean is_invokestatic = opcode == 184;
    if (is_invokestatic != ((method.access_flags & Access.STATIC) != 0))
      throw new Error
	("compile_invoke_xxx static flag mis-match method.flags="+method.access_flags);
    if (!is_invokestatic)
      arg_count++;
    code.put1(opcode);  // invokevirtual, invokespecial, or invokestatic
    code.putIndex2(getConstants().addMethodRef(method));
    if (opcode == 185)  // invokeinterface
      {
	code.put1(arg_count);
	code.put1(0);
      }
    while (--arg_count >= 0)
      pop_stack_type ();
    if (method.return_type != Type.void_type)
      push_stack_type (method.return_type);
  }

  /** Compile a virtual method call.
   * The stack contains the 'this' object, followed by the arguments in order.
   * @param method the method to invoke virtually
   */
  public void compile_invoke_virtual (Method method)
  {
    compile_invoke_method (method, 182);  // invokevirtual
  }

  public void compile_invoke_special (Method method)
  {
    compile_invoke_method (method, 183);  // invokespecial
  }

  /** Compile a static method call.
   * The stack contains the the arguments in order.
   * @param method the static method to invoke
   */
  public void compile_invoke_static (Method method)
  {
    compile_invoke_method (method, 184);  // invokestatic
  }

  /** Compile a tail-call to position 0 of the current procewure.
   * If pop_args is true, copy argument registers (except this) from stack. */
  public void compile_tailcall (boolean pop_args)
  {
    if (pop_args)
      {
	int arg_slots = ((access_flags & Access.STATIC) != 0) ? 0 : 1;
	for (int i = arg_types.length;  --i >= 0; )
	  arg_slots += arg_types[i].size > 4 ? 2 : 1;
	for (int i = arg_types.length;  --i >= 0; )
	  {
	    arg_slots -= arg_types[i].size > 4 ? 2 : 1;
	    compile_store_value (locals.used [arg_slots]);
	  }
      }
    prepareCode(5);
    int delta = - code.PC;
    if (delta < -32768)
      {
	code.put1(200);  // goto_w
	code.put4(delta);
      }
    else
      {
	code.put1(167); // goto
	code.put2(delta);
      }
    code.unreachable_here = true;
  }

  private void compile_fieldop (Field field, int opcode)
  {
    prepareCode(3);
    code.put1(opcode);
    code.putIndex2(getConstants().addFieldRef(field));
  }

  /** Compile code to get a static field value.
   * Stack:  ... => ..., value */

  public void compile_getstatic (Field field)
  {
    push_stack_type (field.type);
    compile_fieldop (field, 178);  // getstatic
  }

  /** Compile code to get a non-static field value.
   * Stack:  ..., objectref => ..., value */

  public void compile_getfield (Field field)
  {
    pop_stack_type ();
    push_stack_type (field.type);
    compile_fieldop (field, 180);  // getfield
  }

  /** Compile code to put a static field value.
   * Stack:  ..., value => ... */

  public void compile_putstatic (Field field)
  {
    pop_stack_type ();
    compile_fieldop (field, 179);  // putstatic
  }

  /** Compile code to put a non-static field value.
   * Stack:  ..., objectref, value => ... */

  public void compile_putfield (Field field)
  {
    pop_stack_type ();
    pop_stack_type ();
    compile_fieldop (field, 181);  // putfield
  }


  public void compile_linenumber (int linenumber)
  {
    if (code == null)
      code = new CodeAttr(this);
    code.putLineNumber(linenumber);
  }

  void write (DataOutputStream dstr, ClassType classfile)
       throws java.io.IOException
  {
    Variable var;
    dstr.writeShort (access_flags);
    dstr.writeShort (name_index);
    dstr.writeShort (signature_index);

    Attribute.writeAll(this, dstr);
  }

  private String signature;

  public String getSignature ()
  {
    if (signature == null)
      {
	StringBuffer buf = new StringBuffer(100);
	int args_count = arg_types.length; 
	buf.append('(');
	for (int i = 0; i < args_count; i++)
	  buf.append (arg_types[i].getSignature());
	buf.append(')');
	buf.append(return_type.getSignature());
	signature = buf.toString();
      }
    return signature;
  }

  public void setSignature (String signature)
  {
    int len = signature.length();
    if (len < 3 || signature.charAt(0) != '(')
      throw new ClassFormatError("bad method signature");
    int pos = 1;
    java.util.Stack types = new java.util.Stack();
    for (;;)
      {
	int arg_sig_len = Type.signatureLength(signature, pos);
	if (arg_sig_len < 0)
	  {
	    if (pos < len && signature.charAt(pos) == ')')
	      break;
	    throw new ClassFormatError("bad method signature");
	  }
	Type arg_type = Type.signatureToType(signature, pos, arg_sig_len);
	types.push(arg_type);
	pos += arg_sig_len;
      }
    arg_types = new Type[types.size()];
    for (int i = types.size();  --i >= 0; )
      arg_types[i] = (Type) types.pop();
    return_type = Type.signatureToType(signature, pos+1, len-pos-1);
  }

  public void setSignature (int signature_index)
  {
    CpoolUtf8 sigConstant = (CpoolUtf8)
      getConstants().getForced(signature_index, ConstantPool.UTF8);
    this.signature_index = signature_index;
    setSignature(sigConstant.string);
  }

  void assign_constants ()
  {
    ConstantPool constants = getConstants();
    if (name_index == 0 && name != null)
      name_index = constants.addUtf8(name).index;
    if (signature_index == 0)
      signature_index = constants.addUtf8(getSignature()).index;
    Attribute.assignConstants(this, classfile);
  }

  public ClassType getDeclaringClass() { return classfile; }

  public Type getReturnType() { return return_type; }

  public Type[] getParameterTypes() { return arg_types; }

  public final String getName ()
  {
    return name;
  }
  public final void setName(String name)
  {
    this.name = name;
  }

  public final void setName(int name_index)
  {
    if (name_index <= 0)
      name = null;
    else
      {
	CpoolUtf8 nameConstant = (CpoolUtf8)
	  getConstants().getForced(name_index, ConstantPool.UTF8);
	name = nameConstant.string;
      }
    this.name_index = name_index;
  }
};
