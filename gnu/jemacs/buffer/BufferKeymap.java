package gnu.jemacs.buffer;
import gnu.mapping.Procedure;
import gnu.math.IntNum;
import javax.swing.*;
import javax.swing.text.*;
import gnu.kawa.util.*;
import java.awt.event.KeyEvent;

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

  /** The magic KeyStrok that indicates a (Emacs) meta prefix.
   * I.e. we saw either an Escape or a meta modifier. */
  public static KeyStroke metaKey
    = javax.swing.KeyStroke.getKeyStroke('\033');

  // Not sure if we want/need this anymore. */
  static Keymap defaultKeymap
    = JTextComponent.getKeymap(JTextComponent.DEFAULT_KEYMAP);

  /** The Emacs global map. */
  public static Keymap globalKeymap
    = JTextComponent.addKeymap("global", defaultKeymap);

  /** The Emacs global escape (meta) map. */
  public static Keymap metaKeymap
    = JTextComponent.addKeymap("ESC-map", null);

  /** The metaKeymap wrapped in an Action. */
  static Action metaAction = new Command(metaKeymap, metaKey);

  static InsertAction defaultInsertAction = new InsertAction(null);

  static
  {
    globalKeymap.setDefaultAction(defaultInsertAction);
    globalKeymap.addActionForKeyStroke(metaKey, metaAction);
  }
  static final int CTRL_MASK = java.awt.event.InputEvent.CTRL_MASK;
  static final int SHIFT_MASK = java.awt.event.InputEvent.SHIFT_MASK;
  // Note ALT_MASK and META_MASK are shifted!
  static final int META_MASK = java.awt.event.InputEvent.ALT_MASK;
  static final int ALT_MASK = java.awt.event.InputEvent.META_MASK;
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
    return asNonAction(action);
  }

  /**
   * Inverse of asAction method.
   */
  public static Object asNonAction(Action action)
  {
    if (action instanceof Command)
      return ((Command) action).getCommand();
    if (action instanceof FinalAction)
      return ((FinalAction) action).action;
    if (action instanceof TooLongAction)
      return IntNum.make(((TooLongAction) action).getMaxValid());
    return action;
  }

  /**
   * Convert a "command" to action.
   */
  public static Action asAction(Object binding)
  {
    return asAction(binding, null);
  }

  public static Action asAction(Object binding, KeyStroke key)
  {
    if (binding instanceof Procedure)
      return new Command((Procedure) binding, key);
    if (binding instanceof String)
      return new Command(binding, (String) binding, key);
    if (binding instanceof Keymap)
      return new Command((Keymap) binding, key);
    return (Action) binding;
  }

  /**
   * True for a KeyStroke if the default action should be to ignore it.
   * For example, pressing a shift key should not be an action!
   * We als have the complication that both KEY-PRESSED and KEY_TYPED
   * events and we typically want to ignore one but not both.
   * (If both are handled, we have problems with default actions, as
   * well as when to abort a prefix sequence.  Swing does not have
   * this problem because it does not have prefix sequences and hence state.)
   */
  public static boolean ignorable (KeyStroke key)
  {
    if (key.isOnKeyRelease())
      return true;
    int mods = key.getModifiers();
    // If there are no modifiers, and it's a normal non-control character,
    // we prefer the KEY_TYPED (keyChar) event.
    // Otherwise, we prefer to KEY_PRESSED (keyCode) event.
    int code = key.getKeyCode();
    if (code == 0)
      { // It's a KEY_TYPED (keyChar) event.
        char ch = key.getKeyChar();
        return ch < ' ' || ch > 127;
      }
    else
      { // It's a KEY_PRESSED (keyCODE) event.
        /*
        if ((mods & ~SHIFT_MASK) != 0)
          return false;
        if (code >= KeyEvent.VK_A && code <= KeyEvent.VK_Z)
          return true;
        if ((mods & SHIFT_MASK) != 0)
          return false;
        // FIXME Basically, in the case of KEY_PRESSED events that will
        // map wihout loss of information into normal KEY_TYPED events,
        // we prefer the KEY_TYPED events (as they don't depend on the
        // keyboard layout).
        // ',', '-', '.', '/', '0' .. '9'', '=', ';'
        if ((code >= KeyEvent.VK_COMMA && code <= KeyEvent.VK_9)
            || code == KeyEvent.VK_EQUALS
            || code == KeyEvent.VK_SEMICOLON)
          return true;
        */
        return true;
      }
  }

  public static Action lookupKey(Keymap keymap,
                                 KeyStroke[] prefixKeys, int nPrefix,
                                 KeyStroke key,
                                 boolean acceptDefaults)
  {
    int nKeys = nPrefix + (key != null ? 1 : 0);
    boolean pendingMeta = false;
    if (nKeys == 0)
     throw new Error("no keys");
    for (int i = 0;  ; )
      {
        KeyStroke key_i = i == nPrefix ? key : prefixKeys[i];
        if (pendingMeta)
          key_i = KeyStroke.getKeyStroke(key_i.getKeyCode(), META_MASK);
        Action action = keymap.getAction(key_i);
        Action metaAction;
        if (action == null
            && (key_i.getModifiers() & META_MASK) != 0
            && (metaAction = keymap.getAction(metaKey)) instanceof Command
            && ((Command) metaAction).getCommand() instanceof Keymap)
          {
            Keymap metaMap = (Keymap) ((Command) metaAction).getCommand();
            metaAction = keymap.getAction(metaKey);
            action = metaMap.getAction(stripMeta(key_i));
          }
        i++;
        if (action == null)
          {
            if (ignorable(key))
              return null;
            else
              return keymap.getDefaultAction();
          }
        if (i == nKeys)
          return action;
	Object comm;
	if (action instanceof Command)
	  comm = ((Command) action).getCommand();
	else
	  comm = null;
	if (comm instanceof String)
	  comm = Command.resolveSymbol(comm);
	if (comm instanceof Keymap)
	  keymap = (Keymap) comm;
	else
	  return null;
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
    return ignorable(key) ? null : new TooLongAction(pendingLength);
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
    return null;
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

  public static KeyStroke asKeyStroke(char ch, int mods)
  {
    char c = ch;
    if (ch < ' ')
      {
        mods |= java.awt.event.InputEvent.CTRL_MASK;
        ch = ch == '\0' ? ' ' : (char) ('@' + (ch & 31));
      }
    if (mods != 0)
      return KeyStroke.getKeyStroke(ch, mods);
    else
      return KeyStroke.getKeyStroke(ch);
  }

  public static KeyStroke asKeyStroke(Object key)
  {
    int m = 0;
    while (key instanceof Pair)
      {
	Pair pair = (Pair) key;
	if (pair.cdr == LList.Empty)
	  key = pair.car;
	else
	  {
	    if (pair.car == "control")
	      m |= CTRL_MASK;
	    if (pair.car == "meta")
	      m |= META_MASK;
	    if (pair.car == "shift")
	      m |= SHIFT_MASK;
	    if (pair.car == "alt")
	      m |= ALT_MASK;
	    key = pair.cdr;
	  }
      }
    if (key instanceof Char)
      {
	return asKeyStroke(((Char) key).charValue(), m);
      }
    if (key instanceof IntNum)
      {
	return asKeyStroke((char) ((IntNum) key).intValue(), m);
      }
    if (key instanceof String)
      {
	String name = (String) key;
	if (name.length() == 1)
	  {
	    char ch = name.charAt(0);
	    if (m == 0)
	      return javax.swing.KeyStroke.getKeyStroke(ch);
	    else
	      {
		ch = Character.toUpperCase(ch);
		return javax.swing.KeyStroke.getKeyStroke(ch, m);
	      }
	  }
	if (name == "backspace")
	  return javax.swing.KeyStroke.getKeyStroke(KeyEvent.VK_BACK_SPACE, m);
	if (name == "prior")
	  return javax.swing.KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_UP, m);
	if (name == "next")
	  return javax.swing.KeyStroke.getKeyStroke(KeyEvent.VK_PAGE_DOWN, m);
        if (name == "return")
          name = "enter";
	return javax.swing.KeyStroke.getKeyStroke(name.toUpperCase());
      }
    return (KeyStroke) key;
  }

  /** Get or create keymap associate with a prefix key in a given keymap. */
  public static Keymap definePrefix(Keymap keymap, KeyStroke key)
  {
    Action curAction = keymap.getAction(key);
    if (curAction == null)
      {
        Keymap next = makeEmptyKeymap(null);
        keymap.addActionForKeyStroke((KeyStroke) key, new Command(next, key));
        return next;
      }
    else if (curAction instanceof Command)
      {
        Object command = ((Command) curAction).getCommand();
        Object x;
        if (command instanceof Keymap)
          return (Keymap) command;
        else if (command instanceof String
                 && ((x = Command.resolveSymbol(command)) instanceof Keymap))
          return (Keymap) x;
        else
          throw new Error("keymap entry " + command
                          + " for key " + key + " is not a prefix");
      }
    else
      {
        throw new Error("prefix command cannot override exiting action: "
                        +((Command)curAction).getCommand());
      }
  }

  public static KeyStroke stripMeta(KeyStroke key)
  {
    int mods = key.getModifiers();
    if ((mods & META_MASK) == 0)
      return key;
    mods &= ~ META_MASK;
    int code = key.getKeyCode();
    boolean onRelease = key.isOnKeyRelease();
    if ((mods & ~SHIFT_MASK) != 0 || onRelease
        || code > 127 || code < ' ')
      return KeyStroke.getKeyStroke(code, mods, onRelease);
    else
      {
        if (code >= 'A' && code <= 'Z'&& mods != SHIFT_MASK)
          code = code + 'a' - 'A';
        return KeyStroke.getKeyStroke((char) code);
      }
  }

  public static void defineKey(javax.swing.text.Keymap keymap,
			       Object keySpec, Object binding)
  {
    if (keySpec instanceof Sequence && ! (keySpec instanceof LList))
      {
        // Handle key sequence.
	Sequence value = (Sequence) keySpec;
        boolean hackMeta = keySpec instanceof FString;
        int len = value.length();
        //key = null;
        KeyStroke pending = null;
        for (int i = 0;  i < len; )
          {
            Object keyValue = value.get(i);
            boolean sawMeta = false;
            i++;
            if (hackMeta)
              {
                char ch = ((Char) keyValue).charValue();
                if (ch > 127 && ch <= 255)
                  {
                    sawMeta = true;
                    ch = (char) (ch - 128);
                    keyValue = javax.swing.KeyStroke.getKeyStroke(ch);
                  }
              }
            if (keyValue instanceof Pair
                && ((Pair) keyValue).car == "meta")
              {
                sawMeta = true;
                keyValue = ((Pair) keyValue).cdr;
              }
            if (sawMeta)
              keymap = definePrefix(keymap, metaKey);
            if (i < len)
              keymap = definePrefix(keymap, asKeyStroke(keyValue));
            else
              defineKey(keymap, keyValue, binding);
          }
      }
    else
      {
        // Handle single key.
        boolean sawMeta = false;
        if (keySpec instanceof Pair
            && ((Pair) keySpec).car == "meta")
          {
            sawMeta = true;
            keySpec = ((Pair) keySpec).cdr;
          }
        KeyStroke key = asKeyStroke(keySpec);
        if ((key.getModifiers() & META_MASK) != 0)
          {
            key = stripMeta(key);
            sawMeta = true;
          }
        if (sawMeta)
          keymap = definePrefix(keymap, metaKey);
        Action action = (Action) asAction(binding, key);
        keymap.addActionForKeyStroke(key, action);
      }
  }
}
