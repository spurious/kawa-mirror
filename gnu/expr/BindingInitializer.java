package gnu.expr;
import gnu.bytecode.*;

public class BindingInitializer extends Initializer
{
  Declaration decl;
  Expression value;

  static final Method makeBindingMethod
  = Compilation.typeBinding.getDeclaredMethod("make", Compilation.string1Arg);

  public BindingInitializer(Declaration decl, Compilation comp)
  {
    this(decl, comp, null);
  }

  public BindingInitializer(Declaration decl, Compilation comp,
			    Expression value)
  {
    String fname = Compilation.mangleName(decl.getName());
    this.value = value;
    int fflags = Access.FINAL;
    if (! decl.isPrivate())
      fflags |= Access.PUBLIC;
    this.decl = decl;
    next = comp.initChain;
    comp.initChain = this;
    Type ftype = value == null ? Compilation.typeBinding : value.getType();
    field = comp.mainClass.addField (fname, ftype, fflags);
    decl.field = field;
  }

  boolean createNewBinding = false;

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();
    if (! field.getStaticFlag())
      code.emitPushThis();

    if (value == null)
      {
	// FIXME - this should be cached in a local Variable:
	if (! createNewBinding)
	  code.emitInvokeStatic(comp.getCurrentEnvironmentMethod);

	String name = decl.getName();
	if (name == null)
	  code.emitPushNull();
	else
	  code.emitPushString(name);
	if (createNewBinding)
	  code.emitInvokeStatic(makeBindingMethod);
	else
	  code.emitInvokeVirtual(comp.getBindingEnvironmentMethod);
      }
    else
      {
	value.compile (comp, field.getType());
      }

    if (field.getStaticFlag())
      code.emitPutStatic(field);
    else
      code.emitPutField(field);
  }
}
