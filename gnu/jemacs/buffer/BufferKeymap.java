package gnu.jemacs.buffer;
import gnu.mapping.Procedure;
import gnu.math.IntNum;
import javax.swing.*;
import javax.swing.text.*;
import kawa.lang.FString;
import kawa.lang.Sequence;

/** This manages the keymaps active for a given buffer.
 *
 * It is also a Keymap that indirects through the Buffer's keymaps.
 * The reason is that in Emacs Keymaps ar associated with Buffers,
 * while in Swing they are associated with JTextComponents.
 * This way we can make this be a "compound" Keymap that all of
 * a Buffer's Windows share;  updating the set of keymaps active for
 * the Buffer will automatically do the same for the Buffer's Windows.
 */

public class BufferKeymap implements javax.swing.text.Keymap
{
  Buffer buffer;
  static Keymap defaultKeymap
    = JTextComponent.getKeymap(JTextComponent.DEFAULT_KEYMAP);
  static Keymap globalKeymap
    = JTextComponent.addKeymap("global", defaultKeymap);
  static
  {
    globalKeymap.setDefaultAction(new InsertAction(null)) ;
  }
  Keymap localKeymap;
  static int counter;
  Keymap[] activeKeymaps;
  int activeLength;
  KeyStroke[] pendingKeys = null;
  int pendingLength = 0;
  // private Keymap actual;
  /* Count of initial Keymaps in activeKeymaps that have been eliminated,
   * because of previous prefix keys. */
  int eliminated = 0;

  /* Provided for compatibility with the Keymap interface. */
  Keymap resolveParent;

  public void pushPrefix(KeyStroke prefix)
  {
    if (pendingKeys == null)
      pendingKeys = new KeyStroke[10];
    pendingKeys[pendingLength++] = prefix;
  }

  public static Keymap makeEmptyKeymap(String name)
  {
    if (name == null)
      name = "keymap-"+(++counter);
    return JTextComponent.addKeymap(name, null);
  }

  public BufferKeymap(Buffer buffer)
  {
    this.buffer = buffer;
    activeKeymaps = new Keymap[6];
    activeLength = 1;
    activeKeymaps[0] = globalKeymap;
  }

  public Keymap getLocalKeymap() { return localKeymap; }

  public void setLocalKeymap(Keymap map)
  {
    // First remove the old local map.
    if (localKeymap != null)
      {
        activeKeymaps[activeLength-2] = activeKeymaps[activeLength-1];
        activeLength--;
        localKeymap = null;
      }
    if (map != null)
      {
        activeKeymaps[activeLength] = activeKeymaps[activeLength-1];
        activeKeymaps[activeLength-1]= map;
        activeLength++;
        localKeymap = map;
      }
  }

  public void addActionForKeyStroke(KeyStroke key, Action a)
  {
    activeKeymaps[0].addActionForKeyStroke(key, a);
  }

  public static Object
  lookupKey(Keymap keymap, Sequence keys, boolean acceptDefaults)
  {
    int nKeys = keys.length();
    KeyStroke[] prefixKeys = new KeyStroke[nKeys];
    java.util.Enumeration enumKeys = keys.elements();
    for (int i = 0;  enumKeys.hasMoreElements();  i++)
      {
        prefixKeys[i] = asKeyStroke(enumKeys.nextElement());
      }
    Action action = lookupKey(keymap, prefixKeys, nKeys, null, acceptDefaults);
    if (action instanceof PrefixAction)
      return ((PrefixAction) action).getKeymap();
    if (action instanceof ProcAction)
      return ((ProcAction) action).getProcedure();
    if (action instanceof FinalAction)
      return ((FinalAction) action).action;
    if (action instanceof TooLongAction)
      return IntNum.make(((TooLongAction) action).getMaxValid());
    return action;
  }

  public static Action lookupKey(Keymap keymap,
                                 KeyStroke[] prefixKeys, int nPrefix,
                                 KeyStroke key,
                                 boolean acceptDefaults)
  {
    int nKeys = nPrefix + (key != null ? 1 : 0);
    if (nKeys == 0)
      return new PrefixAction(null, keymap);
    for (int i = 0;  ; )
      {
        KeyStroke key_i = i == nPrefix ? key : prefixKeys[i];
        Action action = keymap.getAction(key_i);
        i++;
        if (action == null)
          {
            if (acceptDefaults)
              return keymap.getDefaultAction();
            return action;
          }
        if (i == nKeys)
          return action;
        if (! (action instanceof PrefixAction))
          return new TooLongAction(i);
        keymap = ((PrefixAction) action).next;
      }
  }

  public Action getAction(KeyStroke key)
  {
    for (int j = 0;  j < activeLength;  j++)
      {
        Keymap actual = activeKeymaps[j];
        Action action = lookupKey(actual, pendingKeys, pendingLength,
                                  key, j < activeLength - 1);
        if (action != null)
          return action;
      }
    return null;
  }

  public Action[] getBoundActions()
  { // FIXME
    return activeKeymaps[0].getBoundActions();
  }

  public KeyStroke[] getBoundKeyStrokes() 
  { // FIXME
    return activeKeymaps[0].getBoundKeyStrokes();
  }

  public Action getDefaultAction()
  {
    Action action = activeKeymaps[activeLength-1].getDefaultAction();
    return action;
  }

  public KeyStroke[] getKeyStrokesForAction(Action a)
  { // FIXME
    return activeKeymaps[0].getKeyStrokesForAction(a);
  }

  public String getName() 
  {
    return activeKeymaps[0].getName();
  }

  public Keymap getResolveParent() 
  {
    return resolveParent;
  }

  public boolean isLocallyDefined(KeyStroke key) 
  { // FIXME
    return activeKeymaps[0].isLocallyDefined(key);
  }

  public void removeBindings() 
  {
    activeKeymaps[0].removeBindings();
  }

  public void removeKeyStrokeBinding(KeyStroke keys) 
  {
    activeKeymaps[0].removeKeyStrokeBinding(keys);
  }

  public void setDefaultAction(Action a) 
  {
    activeKeymaps[0].setDefaultAction(a);
  }

  public void setResolveParent(Keymap parent) 
  {
    resolveParent = parent;
  }

  public static KeyStroke asKeyStroke(Object key)
  {
    if (key instanceof kawa.lang.Char)
      {
	char value = ((kawa.lang.Char) key).charValue();
	return javax.swing.KeyStroke.getKeyStroke(value);
      }
    return (KeyStroke) key;
  }

  public static void defineKey(javax.swing.text.Keymap keymap,
			       Object keySpec, Object binding)
  {
    if (binding instanceof Procedure)
      binding = new ProcAction((Procedure) binding);
    else if (binding instanceof Keymap)
      binding = new PrefixAction(null, (Keymap) binding);
    KeyStroke key;
    if (keySpec instanceof Sequence
        && ! (keySpec instanceof kawa.lang.List))
      {
	Sequence value = (Sequence) keySpec;
        int len = value.length();
        key = null;
        for (int i = 0;  i < len; )
          {
            key = asKeyStroke(value.elementAt(i));
            i++;
            if (i < len)
              {
                Action curAction = keymap.getAction((KeyStroke) key);
                Keymap next;
                if (curAction == null)
                  {
                    next = makeEmptyKeymap(null);
                    keymap.addActionForKeyStroke((KeyStroke) key,
                                                 new PrefixAction(key, next));
                  }
                else if (curAction instanceof PrefixAction)
                  {
                    next = ((PrefixAction) curAction).getKeymap();
                  }
                else
                  {
                    throw new Error("prefix command cannot override exiting action");
                  }
                keymap = next;
              }
          }
      }
    else
      key = asKeyStroke(keySpec);
    keymap.addActionForKeyStroke(key,
				 (javax.swing.Action) binding);
  }
}
