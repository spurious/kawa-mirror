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
    int fflags = 0;
    if (decl.isPublic()
	&& ! decl.getFlag(Declaration.TYPE_SPECIFIED))
      decl.setIndirectBinding(true);
    if (decl.isIndirectBinding()
	|| decl.getFlag(Declaration.IS_CONSTANT))
      fflags |= Access.FINAL;
    if (! decl.isPrivate())
      fflags |= Access.PUBLIC;
    if (decl.getFlag(Declaration.STATIC_SPECIFIED))
      fflags |= Access.STATIC;
    this.decl = decl;
    Type ftype;
    if (decl.isIndirectBinding())
      {
	ftype = comp.getInterpreter().hasSeparateFunctionNamespace()
	  ? Compilation.typeBinding2 : Compilation.typeBinding;
      }
    else
      ftype = value != null ? value.getType() : decl.getType();
    field = comp.mainClass.addField (fname, ftype, fflags);
    decl.field = field;

    if (decl.isIndirectBinding() || value != null)
      {
	if ((fflags & Access.STATIC) != 0)
	  {
	    next = comp.clinitChain;
	    comp.clinitChain = this;
	  }
	else
	  {
	    next = comp.initChain;
	    comp.initChain = this;
	  }
      }
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
	else if (comp.getInterpreter().hasSeparateFunctionNamespace())
	  code.emitInvokeStatic(comp.getBinding2Method);
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
