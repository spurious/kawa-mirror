package gnu.expr;
import gnu.bytecode.*;
import gnu.mapping.*;

public class BindingInitializer extends Initializer
{
  Declaration decl;
  Expression value;

  static final Method makeSymbolMethod
  = Compilation.typeSymbol.getDeclaredMethod("makeUninterned", 1);

  public BindingInitializer(Declaration decl, Field field, Expression value)
  {
    this.decl = decl;
    this.value = value;
    this.field = field;
  }

  public void emit(Compilation comp)
  {
    CodeAttr code = comp.getCode();

    if (value instanceof QuoteExp)
      {
	Object val = ((QuoteExp) value).getValue();
	if (val != null && ! (val instanceof String))
	  {
	    Literal lit = comp.litTable.findLiteral(val);
	    if (lit.field == this.field)
	      return;
	  }
      }

    if (! field.getStaticFlag())
      code.emitPushThis();

    if (value == null)
      {
	boolean func = comp.getLanguage().hasSeparateFunctionNamespace();
	Object property
	  = func && decl.isProcedureDecl() ? EnvironmentKey.FUNCTION : null;

	Object name = decl.getSymbol();
	if (name instanceof String && ! decl.isAlias())
	  name = Namespace.EmptyNamespace.getSymbol((String) name);
	comp.compileConstant(name, Target.pushObject);
	if (decl.isAlias())
	  code.emitInvokeStatic(makeLocationMethod(name));
	else
	  {
	    ClassType t = ClassType.make("gnu.mapping.ThreadLocation");
	    if (decl.getFlag(Declaration.IS_UNKNOWN
			     |Declaration.IS_DYNAMIC|Declaration.IS_FLUID))
	      {
		if (property == null)
		  code.emitPushNull();
		else
		  comp.compileConstant(property, Target.pushObject);
		code.emitInvokeStatic(t.getDeclaredMethod("getInstance", 2));
	      }
	    else
	      {
		code.emitInvokeStatic(t.getDeclaredMethod("makePrivate", 1));
	      }
	  }
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

  public static Method makeLocationMethod (Object name)
  {
    Type[] atypes = new Type[1];
    if (name instanceof Symbol)
      atypes[0] = Compilation.typeSymbol;
    else
      atypes[0] = Type.string_type;
    return Compilation.typeLocation.getDeclaredMethod("make", atypes);
  }
}
