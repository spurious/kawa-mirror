package gnu.jemacs.buffer;
import gnu.mapping.*;
import gnu.lists.*;
import gnu.jemacs.lang.ELisp;
import gnu.math.IntNum;
import javax.swing.*;
import javax.swing.text.*;

/** An Action that causes a command (such as a Procedure) to be excuted. */

public class Command extends javax.swing.text.TextAction
{
  Object command;
  private KeyStroke key;  // FIXME

  Command (Object command, String name, KeyStroke key)
  {
    super(name);
    if (command instanceof String)
      command = gnu.commonlisp.lang.Symbol.getBinding((String) command);
    this.command = command;
    this.key = key;
  }

  Command (Procedure command, KeyStroke key)
  {
    super(command.getName());
    this.command = command;
    this.key = key;
  }

  Command (Keymap keymap, KeyStroke key)
  {
    super(keymap.getName());
    this.command = keymap;
    this.key = key;
  }

  public final Object getCommand()
  {
    return resolveSymbol(command);
  }

  static Object resolveSymbol(Object command)
  {
    int count = 100;
    for (;;)
      {
	if (command instanceof String)
	  command = gnu.commonlisp.lang.Symbol.getBinding(command);
	if (command instanceof Binding)
	  {
	    Binding bind = (Binding) command;
	    command = bind.getFunctionValue(null);
	    if (command == null)
	      command = bind.getValue();
	  }
	else
	  return command;
	if (--count < 0)
	  throw new Error("circular binding for "+command);
      }
  }


  /** Perform a given command as appropriate for its class. */
  public static void perform(Object command)
  {
    try
      {
	if (command instanceof String || command instanceof Binding)
          {
            Object resolved = resolveSymbol(command);
            if (resolved == null)
              throw new Error("no function defined for "+command);
            command = resolved;
          }
	Procedure proc = (Procedure) command;
	Object interactive = proc.getProperty("emacs-interactive", null);
	if (interactive != null)
	  {
	    if (interactive instanceof String)
	      {
		proc.applyN(processInteractionString(interactive.toString()));
	      }
            else if (interactive == LList.Empty)
              proc.apply0();
	    else
	      {
		System.err.println("not implemented: interactive not a string");
		proc.apply0();
	      }
	  }
	else
          {
            // System.err.println("procedure "+proc+" is not interactive");
            proc.apply0();
          }
      }
    catch (CancelledException ex)
      {
	// Do nothing.
      }
    catch (RuntimeException ex)
      {
	throw ex;
      }
    catch (Error ex)
      {
	throw ex;
      }
    catch (Throwable ex)
      {
	throw new WrappedException(ex);
      }
  }

  public static Object[] processInteractionString (String str)
  {
    int len = str.length();
    int i = 0;
    int start = 0;
    int argCount = 0;
    Buffer buffer = Buffer.getCurrent();
    while (i < len)
      {
	char ch = str.charAt(i++);
	switch (ch)
	  {
	  case '\n':
	    continue;
	  case '@':
	    if (start == i-1) start = i;
	    // FIXME: select-window
	    break;
	  case '*':
	    if (start == i-1) start = i;
	    // FIXME:  check readonly
	    break;
	  case '_':
	    if (start == i-1) start = i;
	    // FIXME:  region stays:
	    break;
	  case 'r':
	    argCount++;
	    // ... fall through ...
	  default:
	    argCount++;
	    while (i < len)
	      {
		ch = str.charAt(i++);
		if (ch == '\n')
		  break;
	      }
	  }
      }
    Object[] args = new Object[argCount];
    int argIndex = 0;
    System.err.println("Get "+argCount+" args");
    i = start;
    while (i < len)
      {
	char ch = str.charAt(i++);
	int promptStart = i;
	int promptLength;
	for (;;)
	  {
	    if (i >= len)
	      {
		promptLength = i - promptStart;
		break;
	      }
	    char c = str.charAt(i++);
	    if (c == '\n')
	      {
		promptLength = i - 1 - promptStart;
		break;
	      }
	  }
	switch (ch)
	  {
	  case 'P':
	    args[argIndex++] = ELisp.FALSE; // FIXME
	    break;
	  case 'p':
	    args[argIndex++] = IntNum.one(); // FIXME
	    break;
          case 'r':
            int mark = buffer.checkMark() + 1;
            int point = buffer.getPoint();
            if (mark <= point)
              {
                args[argIndex++] = IntNum.make(mark);
                args[argIndex++] = IntNum.make(point);
              }
            else
              {
                args[argIndex++] = IntNum.make(point);
                args[argIndex++] = IntNum.make(mark);
              }
            break;
          case 'F':  // FIXME
          case 's':
          case 'S':
            String answer =
              Frame.selectedFrame.ask(str.substring(promptStart,
                                                    promptStart+promptLength));
            args[argIndex++]
              = (ch == 'S' ? (Object) answer.intern()
                 : (Object) new FString(answer));
            break;
	  default:
	    System.err.println("un-implemented interactive prompt:"+ch);
	    args[argIndex++] = ELisp.FALSE;
	  }
      }
    return args;
  }

  public void actionPerformed(java.awt.event.ActionEvent event)
  {
    Object comm = command;
    if (comm instanceof Binding)
      {
        comm = resolveSymbol(comm);
        if (comm == null)
          throw new Error("no function defined for "
                          + ((Binding) command).getName());
      }
    if (comm instanceof TextAction)
      ((TextAction) comm).actionPerformed(event);
    else
      {
	Buffer buffer = Window.getSelected().buffer;
	if (comm instanceof Keymap)
	  {
	    if (key != null)
	      buffer.keymap.pushPrefix(key);
	  }
	else
	  {
	    buffer.keymap.pendingLength = 0;
	    perform(comm);
	  }
      }
  }

  public String toString()
  {
    StringBuffer buf = new StringBuffer(100);
    buf.append("Command[");
    if (command instanceof Binding)
      buf.append(((Binding) command).getName());
    else if (command instanceof Keymap)
      {
        buf.append("Keymap[");
        buf.append(((Keymap) command).getName());
        buf.append(']');
      }
    else
      buf.append(command);
    buf.append(']');
    return buf.toString();
  }
}
