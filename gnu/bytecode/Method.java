package codegen;
import java.io.*;

public class Method {
  byte[] name;  /* Utf8 */
  Type[] arg_types;
  
  Type return_type;
  int access_flags;
  int name_index; /* Index in constant table, or 0 if un-assigned */
  int signature_index; /* Index in constant table, or 0 if un-assigned */
  Method next;
  ClassType classfile;

  /* A chain of labels.  Unsorted, except that the Label with
     the lowest element in fixups must be the first one. */
  Label labels;

  /* True if we cannot fall through to bytes[PC] -
     the previous instruction was an uncondition control transfer.  */
  boolean unreachable_here;
  /** True if control could reach here. */
  public boolean reachableHere () { return !unreachable_here; }

  Method (ClassType clfile, int flags) {
     if (clfile.last_method == null)
       clfile.methods = this;
    else
      clfile.last_method.next = this;
    clfile.last_method = this;
    clfile.methods_count++;
    access_flags = flags;
    classfile = clfile;

    parameter_scope = push_scope ();
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
    

  private Type[] stack_types;
  private int SP;  // Current stack size (in "words")
  private int max_stack;
  private int max_locals;
  int PC;
  byte[] code;
  int exception_table_length;

  /* Map local slot index to the local variable that is there. */
  Variable[] used_locals;
  public Scope current_scope;
  Scope parameter_scope;

  public Scope push_scope () {
    Scope scope = new Scope ();
    enterScope (scope);
    return scope;
  }

  public void enterScope (Scope scope) {
    scope.start_pc = PC;
    scope.linkChild (current_scope);
    current_scope = scope;
    for (Variable var = scope.firstVar ();  var != null;  var = var.nextVar ())
      {
	if (var.isSimple ())
	  {
	    if (! var.isAssigned ())
	      allocate_local (var);
	    else if (used_locals[var.offset] == null)
	      used_locals[var.offset] = var;
	    else if (used_locals[var.offset] != var)
	      throw new Error ("inconsistent local variable assignments for "
+var.strName()+" at "+var.offset+" != "+used_locals[var.offset]);
	  }
      }
  }

  public Scope pop_scope () {
    Scope scope = current_scope;
    current_scope = scope.parent;
    scope.end_pc = PC;
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
  Variable lookup (byte[] name) {
    for (Scope scope = current_scope; scope != null;  scope = scope.parent) {
      Variable var = scope.lookup (name);
      if (var != null)
	return var;
    }
    return null;
  }

  Variable lookup (String name) {
    return lookup (ClassType.to_utf8 (name));
  }

  /** Assign a local variable to a given slot.
   * @param local the Variable to assign
   * @param slot the local variable slot desired
   * @return true iff we succeeded (i.e. the slot was unused) */
  public boolean assign_local (Variable local, int slot)
  {
    int size = local.type.size > 4 ? 2 : 1;
    if (used_locals == null)
      used_locals = new Variable[20+size];
    else if (max_locals + size >= used_locals.length) {
      Variable[] new_locals = new Variable [2 * used_locals.length + size];
      System.arraycopy (used_locals, 0, new_locals, 0, max_locals);
      used_locals = new_locals;
    }
    for (int j = 0; j < size; j++)
      {
	if (used_locals[slot+j] != null)
	  return false;
      }
    for (int j = 0; j < size; j++)
      used_locals[slot + j] = local;
    if (slot + size > max_locals)
      max_locals = slot + size;
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
//System.err.println ("allocate_local for "+local+"("+local.strName()+")=>"+i);
	    return;
	  }
      }
  }

  public void init_param_slots ()
  {
    if ((access_flags & Access.STATIC) == 0)
      new_local (classfile).setParameter (true);
    int arg_count = arg_types.length;
    for (int i = 0;  i < arg_count;  i++) {
      new_local (arg_types[i]).setParameter (true);
    }
  }

  /* Get the index'th parameter. */
  public Variable find_arg (int index) {
    return parameter_scope.find_var (index);
  }

  public Variable new_local (Type type)
  {
    return current_scope.new_var (this, type, null);
  }
  Variable new_local (Type type, String name) {
    return current_scope.new_var (this, type, ClassType.to_utf8 (name));
  }
  Variable new_local (Type type, byte[] name) {
    return current_scope.new_var (this, type, name);
  }

  void kill_local (Variable var) {
    var.end_pc = PC;
    int size = var.type.size > 4 ? 2 : 1;
    while (--size >= 0)
      used_locals [var.offset + size] = null;
  }

  // This method should be called before we generate code for
  // an instruction (or sequence).
  // An upper bound of the intruction length is max_size.
  void instruction_start_hook (int max_size)
  {
    while (labels != null && labels.fixups != null) {
      int oldest_fixup = labels.fixups[0];
      int threshold = unreachable_here ? 30000 : 32000;
      if (PC + max_size - oldest_fixup > threshold)
	labels.emit_spring (this);
      else
	break;
    }
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

  /**
   * Write an 8-bit byte to the current code-stream.
   * @param i the byte to write
   */
  public void put1(int i) {
    if (code == null)
      code = new byte[100];
    else if (PC >= code.length) {
      byte[] new_code = new byte[2 * code.length];
      System.arraycopy (code, 0, new_code, 0, PC);
      code = new_code;
    }
    code[PC++] = (byte) i;
    unreachable_here = false;
  }

  /**
   * Write a 16-bit short to the current code-stream
   * @param i the value to write
   */
  public final void put2(int i) { put1 (i >> 8); put1 (i & 0xFF); }
  /**
   * Write a 32-bit int to the current code-stream
   * @param i the value to write
   */
  public final void put4(int i) {
    put1 (i >> 24);  put1 (i >> 16);  put1 (i >> 8);  put1 (i); }

  final Type pop_stack_type () {
    if (SP <= 0)
      throw new Error("pop_stack_type called with empty stack");
    Type type = stack_types[--SP];
    if (type.size == 8)
      if (pop_stack_type () != Type.void_type)
	throw new Error("missing void type on stack");
    return type;
  }

  final void push_stack_type (Type type) {
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

  public void push_int_const (int i) { compile_push_int (i); } //old
  public void compile_push_int (int i) {
    instruction_start_hook (3);
    if (i >= -1 && i <= 5)
      put1 (i + 3);  // iconst_m1 .. iconst_5
    else if (i >= -128 && i < 128) {
      put1 (16); // bipush
      put1 (i);
    } else if (i >= -32768 && i < 32768) {
      put1 (17); // sipush
      put2 (i);
    } else {
      int j = classfile.get_int_const (i);
      if (j < 256) {
	put1 (18); // ldc1
	put1 (j);
      } else {
	put1 (19); // ldc2
	put2 (j);
      }
    }
    push_stack_type (Type.int_type);
  }

  public void compile_push_long (long i)
  {
    if (i == 0 || i == 1)
      {
	instruction_start_hook (1);
	put1 (9 + (int) i);  // lconst_0 .. lconst_1
      }
    else if ((long) (int) i == i)
      {
	compile_push_int ((int) i);
	instruction_start_hook (1);
	pop_stack_type ();
	put1 (133); // i2l
      }
    else
      {
	instruction_start_hook (3);
	int j = CpoolLong.get_const (classfile, i).index;
      	put1 (20); // ldc2w
	put2 (j);
      }
    push_stack_type (Type.double_type);
  }

  public void compile_push_double (double x)
  {
    if (x == 0.0)
      {
	instruction_start_hook (1);
	put1 (14);  // dconst_0
      }
    else if (x == 1.0)
      {
	instruction_start_hook (1);
	put1 (15);  // dconst_1
      }
    else if (x >= -128.0 && x < 128.0
	     && (double)(int)x == x)
      {
	// Saves space in the constant pool
	// Probably faster, at least on modern CPUs.
	compile_push_int ((int) x);
	instruction_start_hook (1);
	pop_stack_type ();
	put1 (135); // i2d
      }
    else
      {
	instruction_start_hook (3);
	int j = CpoolDouble.get_const (classfile, x).index;
      	put1 (20); // ldc2w
	put2 (j);
      }
    push_stack_type (Type.long_type);
  }

  public void compile_push_string (byte[] str)
  {
    instruction_start_hook (3);
    CpoolUtf8 bytes = CpoolUtf8.get_const (classfile, str);
    int index = CpoolString.get_const (classfile, bytes).index;
    if (index < 256)
      {
	put1 (18); // ldc1
	put1 (index);
      }
    else
      {
	put1 (19); // ldc2
	put2 (index);
      }
    push_stack_type (Type.string_type);
  }

  public void compile_push_string (String str)
  {
    compile_push_string (ClassType.to_utf8 (str));
  }

  public void compile_push_null ()
  {
    instruction_start_hook (1);
    put1 (1);  // aconst_null
    push_stack_type (Type.pointer_type);
  }

  void compile_new_array (int type_code)
  {
    instruction_start_hook (2);
    put1 (188);  // newarray
    put1 (type_code);
  }

  public final void compile_arraylength ()
  {
    instruction_start_hook (1);
    put1 (190);  // arraylength
    push_stack_type (Type.int_type);
  }

  /**
   * Invoke new on a class type.
   * Does not call the constructor!
   * @param type the desired new object type
   */
  public void compile_new (ClassType type)
  {
    instruction_start_hook (3);
    put1 (187); // new
    put2 (classfile.get_class_const (type));
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
	instruction_start_hook (3);
	put1 (189); // anewarray
	put2 (classfile.get_class_const ((ClassType) element_type));
      }
    else
      throw new Error ("unimplemented type in compile_new_array");
    push_stack_type (Type.pointer_type);
  }

  public void compile_checkcast (Type type)
  {
    instruction_start_hook (3);
    pop_stack_type ();
    put1 (192);  // checkcast
    if (type instanceof ArrayType)
      {
	ArrayType atype = (ArrayType) type;
	CpoolUtf8 name = CpoolUtf8.get_const (classfile, atype.signature);
	put2 (classfile.get_class_const (name));
      }
    else if (type instanceof ClassType)
      {
	put2 (classfile.get_class_const ((ClassType) type));
      }
    else
      throw new Error ("unimplemented type in compile_checkcast");
    push_stack_type (type);
  }

  public void maybe_compile_checkcast (Type type)
  {
    Type stack_type = stack_types[SP-1];
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
    instruction_start_hook (1);
    pop_stack_type ();  // Pop new values
    pop_stack_type ();  // Pop index
    pop_stack_type ();  // Pop array reference
    if      (element_type == Type.byte_type)    put1 (84);  // bastore
    else if (element_type == Type.short_type)   put1 (86);  // sastore
    else if (element_type == Type.int_type)     put1 (79);  // iastore
    else if (element_type == Type.long_type)    put1 (80);  // lastore
    else if (element_type == Type.float_type)   put1 (81);  // fastore
    else if (element_type == Type.double_type)  put1 (82);  // dastore
    else if (element_type == Type.boolean_type) put1 (84);  // bastore
    else if (element_type == Type.char_type)    put1 (85);  // castore
    else                                        put1 (83);  // aastore
  }

  /** Load an element from an array.
   * Must already have pushed the array and the index (in that order):
   * Stack:  ..., array, index => ..., value */
  public void compile_array_load (Type element_type)
  {
    instruction_start_hook (1);
    pop_stack_type ();  // Pop index
    pop_stack_type ();  // Pop array reference
    if      (element_type == Type.byte_type)    put1 (51);  // baload
    else if (element_type == Type.short_type)   put1 (53);  // saload
    else if (element_type == Type.int_type)     put1 (46);  // iaload
    else if (element_type == Type.long_type)    put1 (47);  // laload
    else if (element_type == Type.float_type)   put1 (48);  // faload
    else if (element_type == Type.double_type)  put1 (49);  // daload
    else if (element_type == Type.boolean_type) put1 (51);  // baload
    else if (element_type == Type.char_type)    put1 (52);  // caload
    else                                        put1 (50);  // aaload
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
        instruction_start_hook (1);
	Type type = pop_stack_type ();
	if (type.size > 4)
	  put1 (88);  // pop2
	else if (nvalues > 1)
	  { // optimization:  can we pop 2 4-byte words using a pop2
	    Type type2 = pop_stack_type ();
	    if (type2.size > 4)
	      {
		put1 (87);  // pop
		instruction_start_hook (1);
	      }
	    put1 (88);  // pop2
	    --nvalues;
	  }
	else
	  put1 (87); // pop
      }
  }

  public void compile_swap ()
  {
    instruction_start_hook (1);
    Type type1 = pop_stack_type ();
    Type type2 = pop_stack_type ();
    if (type1.size > 4 || type2.size > 4)
      throw new Error ("compile_swap:  not allowed for long or double");
    push_stack_type (type1);
    put1 (95);  // swap
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
    instruction_start_hook (1);
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

    int code;
    // These are the types of the words (in any) that are "skipped":
    Type skipped1 = null;
    Type skipped2 = null;
    if (offset == 0)
      {
	code = size == 1 ? 89 : 92;  // dup or dup2
      }
    else if (offset == 1)
      {
	code = size == 1 ? 90 : 93; // dup_x1 or dup2_x1
	skipped1 = pop_stack_type ();
	if (skipped1.size > 4)
	  throw new Error ("dup will cause invalid types on stack");
      }
    else if (offset == 2)
      {
	code = size == 1 ? 91 : 94; // dup_x2 or dup2_x2
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

    put1 (code);
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
    Type type = var.type.promote ();
    int code;
    instruction_start_hook (4);
    if (type == Type.int_type)
      code = 0; // iload??
    else if (type == Type.long_type)
      code = 1; // lload??
    else if (type == Type.float_type)
      code = 2; // float??
    else if (type == Type.double_type)
      code = 3; // dload??
    else
      code = 4; // aload??
    if (offset <= 3)
      put1 (26 + 4 * code + offset);  // [ilfda]load_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1 (196); // wide
	    put1 (offset >> 8);
	  }
	put1 (21 + code);  // [ilfda]load
	put1 (offset);
      }
    push_stack_type (var.type);
  }

  public void compile_store_value (Variable var)
  {
    if (var.dead ())
      throw new Error ("attempting to push dead variable");
    int offset = var.offset;
    if (offset < 0 || !var.isSimple ())
      throw new Error ("attempting to store in unassigned variable");
    Type type = var.type.promote ();
    int code;
    instruction_start_hook (4);
    pop_stack_type ();
    if (type == Type.int_type)
      code = 0; // istore??
    else if (type == Type.long_type)
      code = 1; // lstore??
    else if (type == Type.float_type)
      code = 2; // float??
    else if (type == Type.double_type)
      code = 3; // dstore??
    else
      code = 4; // astore??
    if (offset <= 3)
      put1 (59 + 4 * code + offset);  // [ilfda]store_[0123]
    else
      {
	if (offset >= 256)
	  {
	    put1 (196); // wide
	    put1 (offset >> 8);
	  }
	put1 (54 + code);  // [ilfda]store
	put1 (offset);
      }
  }

  public void compile_push_this ()
  {
    instruction_start_hook (1);
    put1 (42);  // aload_0
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
    byte[] to_sig = to.signature;
    byte[] from_sig = from.signature;
    int op = -1;
    if (to_sig.length == 1 || from_sig.length == 1)
      {
	byte to_sig0 = to_sig[0];
	byte from_sig0 = to_sig[0];
	if (from.size < 4)
	  from_sig0 = (byte) 'I';
	if (to.size < 4)
	  {
	    compile_convert (from, Type.int_type);
	    from_sig0 = (byte) 'I';
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
    instruction_start_hook (1);
    pop_stack_type();
    put1 (op);
    push_stack_type(to);
  }

  final void compile_transfer (Label label, int opcode)
  {
    put1 (opcode);
    label.emit (this);
  }

  public final void compile_goto_ifeq (Label label, boolean invert)
  {
    Type type2 = pop_stack_type ().promote ();
    Type type1 = pop_stack_type ().promote ();
    instruction_start_hook (4);
    int opcode;
    if (type1 == Type.int_type && type2 == Type.int_type)
      opcode = 159;  // if_cmpeq (inverted: if_icmpne)
    else if (type1 == Type.long_type && type2 == Type.long_type)
      {
	put1 (148);   // lcmp
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1 == Type.float_type && type2 == Type.float_type)
      {
	put1 (149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1 == Type.double_type && type2 == Type.double_type)
      {
	put1 (149);   // fcmpl
	opcode = 153;  // ifeq (inverted: ifeq)
      }
    else if (type1.signature.length == 1 || type2.signature.length == 1)
      throw new Error ("non-matching types to compile_goto_ifeq");
    else
      opcode = 165;  // if_acmpeq (inverted: if_acmpne)
    if (invert)
      opcode++;
    compile_transfer (label, opcode);
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

  /** The stack of currently active conditionals. */
  IfState if_stack;

  /** Compile start of a conditional:  if (!(x OPCODE 0)) ...
   * The value of x must already have been pushed. */
  public final void compile_goto_if (int opcode)
  {
    IfState new_if = new IfState (this);
    pop_stack_type ();
    instruction_start_hook (3);
    compile_transfer (new_if.end_label, opcode);
    new_if.start_stack_size = SP;
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
    IfState new_if = new IfState (this);
    pop_stack_type ();
    pop_stack_type ();
    instruction_start_hook (3);
    compile_transfer (new_if.end_label, opcode);
    new_if.start_stack_size = SP;
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
    IfState new_if = new IfState (this);
    compile_goto_ifeq (new_if.end_label);
    new_if.start_stack_size = SP;
  }

  /** Compile start of else clause. */
  public final void compile_else ()
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
	compile_goto (end_label);
      }
    while (SP != if_stack.start_stack_size)
      pop_stack_type ();
    else_label.define (this);
    if_stack.doing_else = true;    
  }

  /** Compile end of conditional. */
  public final void compile_fi ()
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

  /**
   * Compile an unconditional branch (goto).
   * @param label target of the branch (must be in this method).
   */
  public final void compile_goto (Label label)
  {
    instruction_start_hook (5);
    if (label.defined ())
      {
	int delta = label.position - PC;
	if (delta < -32768)
	  {
	    put1 (200);  // goto_w
	    put4 (delta);
	  }
	else
	  {
	    put1 (167); // goto
	    put2 (delta);
	  }
      }
    else
      compile_transfer (label, 167); // goto label
    unreachable_here = true;
  }

  /**
   * Compile a function return.
   */
  public final void compile_return () {
    instruction_start_hook (1);
    if (return_type == Type.void_type) {
      put1 (177); // return
      return;
    }
    Type type = pop_stack_type ();
    if (type == Type.int_type
	|| type == Type.short_type
	|| type == Type.byte_type
	|| type == Type.boolean_type
	|| type == Type.char_type)
      put1 (172); // ireturn
    else if (type == Type.long_type)
      put1 (173); // lreturn
    else if (type == Type.float_type)
      put1 (174); // freturn
    else if (type == Type.double_type)
      put1 (175); // dreturn
    else if (type == Type.void_type)
      throw new Error ("returning void type");
    else
      put1 (176); // arreturn
  }

  /*
  public Scope scope_start ();
  public scope_end ();
  public Variable new_temp (String name);
  public Variable free_temp (Variable temp_var);

  public void push_long_const (long i);

  */

  // public final void compile_int_add () { put1 (96); pop_stack_type ();}
  // public final void compile_long_add () { put1 (97); pop_stack_type ();}
  // public final void compile_float_add () { put1 (98); pop_stack_type ();}
  // public final void compile_double_add () { put1 (99); pop_stack_type ();}
  public final void compile_add () { compile_binop (96); }
  public final void compile_sub () { compile_binop (100); }
  public final void compile_mul () { compile_binop (104); }
  public final void compile_div () { compile_binop (108); }
  public final void compile_rem () { compile_binop (112); }

  private final void compile_binop (int base_code) {
    instruction_start_hook (1);
    Type type2 = pop_stack_type ().promote ();
    Type type1_raw = pop_stack_type ();
    Type type1 = type1_raw.promote ();
    if (type1 != type2)
      throw new Error ("non-matching types in binary operation");
    if (type1 == Type.int_type)
      put1 (base_code);
    else if (type1 == Type.long_type)
      put1 (base_code+1);
    else if (type1 == Type.float_type)
      put1 (base_code+2);
    else if (type1 == Type.double_type)
      put1 (base_code+3);
    else
      throw new Error ("bad type in binary operation");
    push_stack_type (type1_raw);
  }

  public void compile_primop (int opcode, int arg_count, Type retType)
  {
    instruction_start_hook (1);
    while (-- arg_count >= 0)
      pop_stack_type ();
    put1 (opcode);
    push_stack_type (retType);
  }

  public void compile_invoke_method (Method method, int opcode)
  {
    instruction_start_hook (opcode == 185 ? 5 : 3);
    int arg_count = method.arg_types.length;
    boolean is_invokestatic = opcode == 184;
    if (is_invokestatic != ((method.access_flags & Access.STATIC) != 0))
      throw new Error
	("compile_invoke_xxx static flag mis-match method.flags="+method.access_flags);
    if (!is_invokestatic)
      arg_count++;
    put1 (opcode);  // invokevirtual, invokespecial, or invokestatic
    put2 (CpoolRef.get_const (classfile, method).index);
    if (opcode == 185)  // invokeinterface
      {
	put1(arg_count);
	put1(0);
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
	    compile_store_value (used_locals [arg_slots]);
	  }
      }
    instruction_start_hook (5);
    int delta = - PC;
    if (delta < -32768)
      {
	put1 (200);  // goto_w
	put4 (delta);
      }
    else
      {
	put1 (167); // goto
	put2 (delta);
      }
    unreachable_here = true;
  }

  private void compile_fieldop (Field field, int opcode)
  {
    instruction_start_hook (3);
    put1 (opcode);
    put2 (CpoolRef.get_const (classfile, field).index);
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

  // The line number table.  Each even entry (starting with index 0) is a PC,
  // and the following odd entry is the linenumber.
  short[] linenumber_table;
  // The number of linenumber (pairs) in linenumber_table.
  int linenumber_count;

  public void compile_linenumber (int linenumber)
  {
    if (linenumber_table == null)
      linenumber_table = new short[32];
    else if (2 * linenumber_count >= linenumber_table.length)
      {
	short[] new_linenumbers = new short [2 * linenumber_table.length];
	System.arraycopy (linenumber_table, 0, new_linenumbers, 0,
			  2 * linenumber_count);
	linenumber_table = new_linenumbers;
      }
    linenumber_table[2 * linenumber_count] = (short) PC;
    linenumber_table[2 * linenumber_count + 1] = (short) linenumber;
    linenumber_count++;
  }

  void write (DataOutputStream dstr, ClassType classfile)
       throws java.io.IOException
  {
    Variable var;
    boolean have_code = PC > 0;
    short attributes_count = have_code ? (short) 1 : (short) 0;
    dstr.writeShort (access_flags);
    dstr.writeShort (name_index);
    dstr.writeShort (signature_index);
    dstr.writeShort (attributes_count);

    if (have_code)
      {
	int local_variable_count = 0;
	int linenumber_count = this.linenumber_count;
	VarEnumerator vars = null;
	if (classfile.emitDebugInfo)
	  {
	    vars = parameter_scope.allVars ();
	    while ((var = vars.nextVar ()) != null)
	      {
		if (var.isSimple () && var.name != null)
		  local_variable_count++;
	      }
	  }
	else
	  linenumber_count = 0;
	
	int code_attributes_count
	  = (local_variable_count > 0 ? 1 : 0)
	  + (linenumber_count > 0 ? 1 : 0);
	int code_attribute_size = 12 + PC + 8 * exception_table_length;
	if (local_variable_count > 0)
	  code_attribute_size += 8 + 10 * local_variable_count;
	if (linenumber_count > 0)
	  code_attribute_size += 8 + 4 * linenumber_count;
	dstr.writeShort (classfile.Code_name_index);
	dstr.writeInt (code_attribute_size);
	dstr.writeShort (max_stack);
	dstr.writeShort (max_locals);
	dstr.writeInt (PC);
	dstr.write (code, 0, PC);
	dstr.writeShort (exception_table_length);
	dstr.writeShort (code_attributes_count);

	if (local_variable_count > 0)
	  {
	    dstr.writeShort (classfile.LocalVariableTable_name_index);
	    dstr.writeInt (2 + 10 * local_variable_count);
	    dstr.writeShort (local_variable_count);
	    
	    for (vars.reset (); (var = vars.nextVar ()) != null; )
	      {
		if (var.isSimple () && var.name != null)
		  {
		    dstr.writeShort (var.start_pc);
		    dstr.writeShort (var.end_pc - var.start_pc);
		    dstr.writeShort (var.name_index);
		    dstr.writeShort (var.signature_index);
		    dstr.writeShort (var.offset);
		  }
	      }
	  }

	if (linenumber_count > 0)
	  {
	    dstr.writeShort (classfile.LineNumberTable_name_index);
	    dstr.writeInt (2 + 4 * linenumber_count);
	    dstr.writeShort (linenumber_count);
	    for (int i = 0;  i < linenumber_count;  i++)
	      {
		dstr.writeShort (linenumber_table[2 * i]);  // start_pc
		dstr.writeShort (linenumber_table[2 * i + 1]);  // line_number
	      }
	  }
      }
  }

  private byte[] signature;

  public byte[] getSignature ()
  {
    if (signature == null)
      {
	ByteArrayOutputStream bstr = new ByteArrayOutputStream ();
	int args_count = arg_types.length; 
	try
	  {
	    bstr.write ((byte)'(');
	    for (int i = 0; i < args_count; i++) {
	      bstr.write (arg_types[i].signature);
	    }
	    bstr.write ((byte)')');
	    bstr.write (return_type.signature);
	    signature = bstr.toByteArray();
	  }
	catch (IOException ex) {
	  throw new Error(ex.toString());
	}
      }
    return signature;
  }

  void assign_constants ()
  {
    if (name_index == 0 && name != null)
      name_index = classfile.get_utf8_const (name);

    if (PC > 0 && classfile.Code_name_index == 0)
      classfile.Code_name_index = classfile.get_utf8_const ("Code");

    if (classfile.SourceFile_name_index == 0
	&& classfile.sourcefile_index > 0)
      classfile.SourceFile_name_index
	= classfile.get_utf8_const ("SourceFile");

    if (classfile.emitDebugInfo)
      {
	if (classfile.LineNumberTable_name_index == 0
	     && linenumber_count > 0)
	  classfile.LineNumberTable_name_index
	    = classfile.get_utf8_const ("LineNumberTable");

	if (classfile.LocalVariableTable_name_index == 0)
	  classfile.LocalVariableTable_name_index
	    = classfile.get_utf8_const ("LocalVariableTable");

	VarEnumerator vars = parameter_scope.allVars ();
	Variable var;
	while ((var = vars.nextVar ()) != null)
	  {
	    if (var.isSimple () && var.name != null)
	      {
		if (var.name_index == 0)
		  var.name_index = classfile.get_utf8_const (var.name);
		if (var.signature_index == 0)
		  var.signature_index
		    = classfile.get_utf8_const (var.type.signature);
	      }
	  }
	
      }

    if (signature_index == 0)
      signature_index = classfile.get_utf8_const (getSignature ());
  }

  public ClassType getDeclaringClass() { return classfile; }

  public Type getReturnType() { return return_type; }

  public Type[] getParameterTypes() { return arg_types; }

  public String getName ()
  {
    // FIXME - only works for ASCII names!
    return name == null ? null : new String (name, 0);
  }
};
