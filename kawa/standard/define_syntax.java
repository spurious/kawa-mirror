package kawa.standard;
import kawa.lang.*;
import gnu.expr.*;
import gnu.bytecode.ClassType;
import gnu.bytecode.Method;
import gnu.mapping.*;
import gnu.lists.*;

public class define_syntax extends Syntax
{
  public define_syntax ()
  {
    this.hygienic = true;
  }

  public define_syntax (Object name, boolean hygienic)
  {
    super(name);
    this.hygienic = hygienic;
  }

  static ClassType typeMacro = ClassType.make("kawa.lang.Macro");
  static Method makeMethod = typeMacro.getDeclaredMethod("make", 3);
  static Method makeNonHygienicMethod
    = typeMacro.getDeclaredMethod("makeNonHygienic", 3);
  static Method setExpanderMethod
    = typeMacro.getDeclaredMethod("setExpander", 1);

  boolean hygienic;

  public Expression rewriteForm (Pair form, Translator tr)
  {
    return tr.syntaxError("define-syntax not in a body");
  }

  public boolean scanForDefinitions (Pair st, java.util.Vector forms,
                                     ScopeExp defs, Translator tr)
  {
    SyntaxForm syntax = null;
    Object st_cdr = st.cdr;
    while (st_cdr instanceof SyntaxForm)
      {
	syntax = (SyntaxForm) st_cdr;
	st_cdr = syntax.form;
      }
    Object name = Translator.safeCar(st_cdr);
    if (! (name instanceof String || name instanceof Symbol))
      {
        forms.addElement(tr.syntaxError("Missing macro name for "+Translator.safeCar(st)));
        return false;
      }
    Object p = Translator.safeCdr(st_cdr);
    if (p == null || Translator.safeCdr(p) != LList.Empty)
      {
        forms.addElement(tr.syntaxError("invalid syntax for "+getName()));
        return false;
      }

    Declaration decl = defs.getDefine(name, 'w', tr);
    decl.setType(typeMacro); 
    tr.push(decl);

    Macro savedMacro = tr.currentMacroDefinition;
    Macro macro = Macro.make(decl);
    macro.setHygienic(hygienic);
    tr.currentMacroDefinition = macro;
    Expression rule = tr.rewrite_car((Pair) p, false);
    tr.currentMacroDefinition = savedMacro;
    macro.expander = rule;

    Object expander;
    if (rule instanceof LambdaExp)
      ((LambdaExp) rule).setFlag(LambdaExp.NO_FIELD);
    Expression args[] = new Expression[3];
    args[0] = new QuoteExp(name);
    args[1] = rule;
    if (tr.immediate || tr.getModule().isStatic())
      args[2] = new QuoteExp(defs);
    else
      args[2] = new ThisExp(defs);
    rule = new ApplyExp(new PrimProcedure(hygienic ? makeMethod : makeNonHygienicMethod), args);
    decl.noteValue(rule);
    decl.setProcedureDecl(true);
  
    if (decl.context instanceof ModuleExp)
      {
	SetExp result = new SetExp (decl, rule);
        result.setDefining (true);
	if (tr.getInterpreter().hasSeparateFunctionNamespace())
	  result.setFuncDef(true);

	tr.formStack.addElement(result);
      }

    return true;
  }
}
