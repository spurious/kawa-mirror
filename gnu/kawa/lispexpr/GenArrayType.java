package gnu.kawa.lispexpr;

import gnu.bytecode.*;
import gnu.expr.*;
import gnu.mapping.Procedure;

/** Type of multi-dimensional arrays.
 * These are implemented using gnu.lists.Array.
 * This type can refine Array with a specific rank and element type.
 */

public class GenArrayType extends ParameterizedType implements TypeValue {
    int rank;
    Type implementationType;

    public static final ClassType typeArray
        = ClassType.make("gnu.lists.Array");

    public static final GenArrayType generalInstance
        = new GenArrayType(-1, Type.objectType);

    public GenArrayType(int rank, Type elementType) {
        super(typeArray, elementType);
        this.rank = rank;
        Type elementImplementation = elementType.getImplementationType();
        if (elementImplementation instanceof PrimType)
            elementImplementation = ((PrimType) elementImplementation).boxedType();
        if (elementImplementation == elementType)
            implementationType = this;
        else
            implementationType =
                new ParameterizedType(typeArray, elementImplementation);
    }

    public Type getImplementationType() {
        return implementationType;
    }

    public int rank() { return rank; }

    public Type getComponentType() {
        return getTypeArgumentType(0);
    }

    @Override
    public int compare(Type other) {
        if (other instanceof GenArrayType) {
            GenArrayType aother = (GenArrayType) other;
            int elcomp = getComponentType().compare(aother.getComponentType());
            if (rank == aother.rank)
                return elcomp;
            if (rank != -1 && aother.rank != -1)
                return -3;
            int rcomp = rank == -1 ? 1 : -1;
            if (rcomp == elcomp)
                return elcomp;
            return -2;
        }
        int r = typeArray.compare(other);
        return r == 0 || r == -1 ? -1 : r == -3 ? -3 : -2;
    }

    @Override
    public Expression convertValue (Expression value) {
        return null;
    }

    @Override
    public Procedure getConstructor() {
        if (rank < 0 && getComponentType() == Type.objectType)
            return new PrimProcedure("kawa.lib.arrays", "array", 2);
        return null;
    }

    @Override
    public String encodeType(Language language) {
        StringBuilder sb = new StringBuilder("array");
        if (rank >= 0)
            sb.append(rank);
        Type elementType = getComponentType();
        if (elementType != Type.objectType && elementType != null) {
            sb.append('[');
            String el = language.encodeType(elementType);
            sb.append(el != null ? el : elementType.getName());
            sb.append(']');
        }
        return sb.toString();
    }

    public void emitCoerceFromObject(CodeAttr code) {
        code.emitCheckcast(implementationType);
    }

    public void emitIsInstance(Variable incoming,
                               Compilation comp, Target target) {
         gnu.kawa.reflect.InstanceOf.emitIsInstance(this, incoming, comp, target);
    }

    public void emitTestIf(Variable incoming, Declaration decl,
                           Compilation comp) {
        CodeAttr code = comp.getCode();
        if (incoming != null)
            code.emitLoad(incoming);
        implementationType.emitIsInstance(code);
        code.emitIfIntNotZero();
        if (decl != null) {
            code.emitLoad(incoming);
            emitCoerceFromObject(code);
            decl.compileStore(comp);
        }
    }
}
