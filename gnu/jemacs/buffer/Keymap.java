package gnu.jemacs.buffer;
import gnu.mapping.Procedure;

public class Keymap
{
  public static void defineKey(javax.swing.text.Keymap keymap,
			       Object key, Object binding)
  {
    if (binding instanceof Procedure)
      {
	Procedure proc = (Procedure) binding;
	binding = new ProcCommand(proc, proc.getName());
      }
    if (key instanceof kawa.lang.FString)
      {
	kawa.lang.FString value = (kawa.lang.FString) key;
	if (value.length() != 1)
	  throw new Error("only 1-char string supported in defineKey");
	key = javax.swing.KeyStroke.getKeyStroke(value.charAt(0));
      }
    if (key instanceof kawa.lang.Char)
      {
	char value = ((kawa.lang.Char) key).charValue();
	key = javax.swing.KeyStroke.getKeyStroke(value);
      }
    keymap.addActionForKeyStroke((javax.swing.KeyStroke) key,
				 (javax.swing.Action) binding);
  }
}

