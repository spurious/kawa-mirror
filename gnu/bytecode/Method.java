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
  public final CodeAttr getCode () { return code; }

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
    
  public Scope pushScope () {
    prepareCode(0);
    return code.pushScope();
  }

  /** True if control could reach here. */
  public boolean reachableHere () { return !code.unreachable_here; }

  public Scope popScope () { return code.popScope(); }

  /** Assign a local variable to a given slot.
   * @param local the Variable to assign
   * @param slot the local variable slot desired
   * @deprecated
   * @return true iff we succeeded (i.e. the slot was unused) */
  public boolean assign_local (Variable local, int slot)
  {
    return local.reserveLocal(slot, code);
  }

  /**
   * Allocate slots for a local variable (or parameter).
   * @param local the variable we need to allocate
   * @return the index of the (first) slot.
   * @deprecated
   */
  public void allocate_local (Variable local)
  {
    local.allocateLocal(code);
  }

  /** Allocate a Code attribute, and prepare to generate code. */

  public void initCode ()
  {
    if (classfile.constants == null)
      classfile.constants = new ConstantPool();
    prepareCode(0);
    code.pushScope();
  }

  public void init_param_slots ()
  {
    initCode ();
    if ((access_flags & Access.STATIC) == 0)
      code.addLocal(classfile).setParameter(true);
    int arg_count = arg_types.length;
    for (int i = 0;  i < arg_count;  i++) {
      code.addLocal(arg_types[i]).setParameter (true);
    }
  }

  void kill_local (Variable var) { var.freeLocal(code); }

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

  public void compile_checkcast (Type type)
  {
    code.emitCheckcast (type);
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
   * Comple code to push the contents of a local variable onto the statck.
   * @param var The variable whose contents we want to push.
   * @deprecated
   */
  public void push_var (Variable var) { code.emitLoad (var); }
  /**
   * @deprecated
   */
  public void compile_push_value (Variable var) { code.emitLoad(var); }

  /** 
    * @deprecated
   */
  public void compile_store_value (Variable var)
  {
    code.emitStore(var);
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
    code.emitTransfer (label, opcode);
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
    code.emitTransfer (new_if.end_label, opcode);
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
    code.emitTransfer(new_if.end_label, opcode);
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
	    code.emitStore(code.locals.used [arg_slots]);
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
