// Copyright (c) 2014  Per M.A. Bothner.
// This is free software;  for terms and warranty disclaimer see ./COPYING.

package gnu.bytecode;
import java.io.*;

/** A CONSTANT_MethodHandle entry in the constant pool. */

public class CpoolMethodHandle extends CpoolEntry {
    int kind;
    CpoolRef reference;

    public CpoolMethodHandle() {
    } 

    public int getTag() { return ConstantPool.METHOD_HANDLE; }

    void write (DataOutputStream dstr) throws java.io.IOException {
	throw new Error();
    }

    public void print (ClassTypeWriter dst, int verbosity) {
	if (verbosity > 0) {
	    dst.print("MethodHandle ");
	    if (verbosity == 2) {
		dst.print(kind);
		dst.print('=');
	    }
	}
	String kindStr;
	switch (kind) {
	case 1: kindStr = "getField"; break;
	case 2: kindStr = "getStatic"; break;
	case 3: kindStr = "putField"; break;
	case 4: kindStr = "putStatic"; break;
	case 5: kindStr = "invokeVirtual"; break;
	case 6: kindStr = "invokeStatic"; break;
	case 7: kindStr = "invokeSpecial"; break;
	case 8: kindStr = "invokeNewSpecial"; break;
	case 9: kindStr = "invokeInterface"; break;
	default: kindStr = "<unknown kind>"; break;
	}
	dst.print(kindStr);
	if (verbosity == 2)
	    dst.print(" reference:");
	dst.print(' ');
	if (reference == null)
	    dst.print("(null)");
	else {
	    if (verbosity == 2)
		dst.printOptionalIndex(reference);
	    reference.print(dst, 0);
	}
    }
}
