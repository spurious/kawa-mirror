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
  int exception_table_length;

  /* A chain of labels.  Unsorted, except that the Label with
     the lowest element in fixups must be the first one. */
  Label labels;

  /** The stack of currently active conditionals. */
  IfState if_stack;

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

  final void compileTransfer (Label label, int opcode)
  {
    put1(opcode);
    label.emit(this);
  }

  /** Compile an unconditional branch (goto).
   * @param label target of the branch (must be in this method).
   */
  public final void compileGoto (Label label)
  {
    reserve(5);
    if (label.defined ())
      {
	int delta = label.position - PC;
	if (delta < -32768)
	  {
	    put1(200);  // goto_w
	    put4(delta);
	  }
	else
	  {
	    put1(167); // goto
	    put2(delta);
	  }
      }
    else
      compileTransfer (label, 167); // goto label
    unreachable_here = true;
  }


  /** Compile start of else clause. */
  public final void compileElse ()
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
	compileGoto (end_label);
      }
    while (SP != if_stack.start_stack_size)
      popType();
    else_label.define (this);
    if_stack.doing_else = true;    
  }

  /** Compile end of conditional. */
  public final void compileFi ()
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
    int exception_table_length = 0;
    return 12 + getCodeLength() + 8 * exception_table_length
      + Attribute.getLengthAll(this);
  }

  public void write (DataOutputStream dstr) throws java.io.IOException
  {
    dstr.writeShort (max_stack);
    dstr.writeShort (max_locals);
    dstr.writeInt (PC);
    dstr.write (code, 0, PC);
    int exception_table_length = 0;  /* FIXME */
    dstr.writeShort (exception_table_length);

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
