package gnu.kawa.brl;
import gnu.mapping.*;
import kawa.standard.Scheme;
import gnu.lists.*;
import gnu.xml.*;
import gnu.expr.*;
import gnu.kawa.lispexpr.ReadTable;

public class BRL extends Scheme
{
  static final BRL krl_instance;
  static BRL brl_instance;
  static final Object emptyForm = new FString();

  protected static final SimpleEnvironment brlEnvironment
    = Environment.make("brl-environment", Scheme.kawaEnvironment);

  static BRLReaderString brlReader =  new BRLReaderString();

  static
  {
    krl_instance = new BRL();
    krl_instance.environ = brlEnvironment;
    CallContext ctx = CallContext.getInstance();
    Environment saveEnv = ctx.getEnvironmentRaw();
    try
      {
        ctx.setEnvironmentRaw(brlEnvironment);
        krl_instance.initBRL();
      }
    finally
      {
        ctx.setEnvironmentRaw(saveEnv);
      }
  }

  public BRL ()
  {
  }

  void initBRL ()
  {
    ModuleBody.setMainPrintValues(true);
    try
      {
	loadClass("gnu.brl.stringfun");
	loadClass("gnu.kawa.brl.progfun");
	loadClass("gnu.kawa.slib.HTTP");
      }
    catch (Throwable ex)
      {
	System.err.println("caught "+ex);
      }
  }

  public static Language getInstance(boolean brlCompatible)
  {
    return brlCompatible ? getBrlInstance() : getBrlInstance();
  }

  public static BRL getKrlInstance()
  {
    return krl_instance;
  }

  public static BRL getBrlInstance()
  {
    if (brl_instance == null)
      {
        brl_instance = new BRL ();
        brl_instance.environ = brlEnvironment;
        brl_instance.setBrlCompatible(true);
      }
    return brl_instance;
  }

  boolean brlCompatible = false;

  public boolean isBrlCompatible() { return brlCompatible; }
  public void setBrlCompatible(boolean compat) {  brlCompatible = compat; }

  public gnu.text.Lexer getLexer(InPort inp, gnu.text.SourceMessages messages)
  {
    Compilation.defaultCallConvention = Compilation.CALL_WITH_CONSUMER;
    BRLRead lexer = new BRLRead(inp, messages);
    lexer.setBrlCompatible(isBrlCompatible());
    return lexer;
  }

  public Consumer getOutputConsumer(java.io.Writer out)
  {
    if (isBrlCompatible())
      return super.getOutputConsumer(out);
    return new XMLPrinter(out, false);
  }

  /** The compiler insert calls to this method for applications and applets. */
  public static void registerEnvironment()
  {
    Language.setDefaults(getKrlInstance());
  }

  public Expression makeBody(Expression[] exps)
  {
    if (isBrlCompatible())
      return super.makeBody(exps);
    return new ApplyExp(gnu.kawa.functions.AppendValues.appendValues, exps);
  }

  public ReadTable createReadTable ()
  {
    ReadTable rt = super.createReadTable();
    rt.setBracketMode(1);
    rt.set(']', brlReader);
    return rt;
  }

  public Procedure getPrompter()
  {
    return new Prompter();
  }
}

class Prompter extends Procedure1
{
  public Object apply1 (Object arg)
  {
    InPort port = (InPort) arg;
    int line = port.getLineNumber() + 1;
    char state = port.readState;
    if (state == ']')
      return "<!--BRL:"+line+"-->";
    else
      {
	if (state == '\n')
	  state = '-';
	return "#|--BRL:"+line+state+"|#";
      }
  }
}
