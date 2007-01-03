package kawa;

import java.awt.*;
import java.awt.event.*;
import gnu.mapping.*;
import gnu.text.Path;

import gnu.expr.Language;
import kawa.standard.Scheme;

/** A Frame containing a Kawa read-eval-print loop.
  * @author Albert Ting <alt@artisan.com> (original base)
  * @author Per Bothner (extensive changes).
  */

public class GuiConsole extends Frame implements ActionListener {
  private static String CLOSE = "Close";
  private static String EXIT = "Exit";
  private static String NEW = "New";
  private static String NEW_SHARED = "New (Shared)";
  private static String PURGE_MESSAGE = "Purge Buffer";

  static int window_number = 0;

  Language language;
  Environment environment;
  Future thread;

  gnu.text.QueueReader in_r;
  OutPort out_p, err_p;

  MessageArea message = null;

  public static void main(String[] args) {
    Language language = Scheme.getInstance();
    new GuiConsole();
  }

  public GuiConsole()
  {
    this(Language.getDefaultLanguage(), Environment.getCurrent(), false);
  }

  public GuiConsole(Language language, Environment penvironment, boolean shared)
  {
    super("Kawa");
    this.language = language;

    in_r = new gnu.text.QueueReader ();
    message = new MessageArea(in_r);
    window_number++;
    kawa.repl.exitIncrement();

    out_p = new OutPort(message.getStdout(), true,
                        Path.valueOf("<msg_stdout>"));
    err_p = new OutPort(message.getStderr(), true,
                        Path.valueOf("<msg_stderr>"));
    InPort in_p = new GuiInPort(in_r, Path.valueOf("<msg_stdin>"),
                                out_p, message);

    this.setLayout(new BorderLayout(0,0));

    this.add("Center",message);

    setupMenus();
    //pack();
    setLocation(100 * window_number, 50 * window_number);
    setSize(700,500);
    setVisible(true);

    thread = new Future (new kawa.repl(language),
			 penvironment, in_p, out_p, err_p);
    Environment env = thread.getEnvironment();
    if (shared)
      env.setIndirectDefines();
    this.environment = env;
    thread.start();
  }

  void close () {
    in_r.appendEOF();
    dispose();
    // Give thread chance to finish and clean up
    try {
      Thread.sleep(100);
    } catch (InterruptedException ex) {
    }
    // Thread.stop is deprecated in JDK 1.2, but I see no good
    // alternative.  (Thread.destroy is not implemented!)
    thread.stop(); 
    kawa.repl.exitDecrement();
 }

  private void setupMenus() {
    MenuBar menubar;
    Menu fileMenu;
    Menu utilitiesMenu;

    MenuItem menuItem;

    WindowListener windowExitCmd = new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	close();
      }
    };

    // Create the menubar
    menubar = new MenuBar();
    fileMenu = new Menu("File");
    utilitiesMenu = new Menu("Utilities");
   
    menubar.add(fileMenu);
    menubar.add(utilitiesMenu);
    
    menuItem = new MenuItem(NEW);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(NEW_SHARED);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(CLOSE);
    menuItem.addActionListener(this);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(EXIT);
    menuItem.addActionListener(this);
    this.addWindowListener(windowExitCmd);	
    fileMenu.add(menuItem);

    menuItem = new MenuItem(PURGE_MESSAGE);
    menuItem.addActionListener(this);	
    utilitiesMenu.add(menuItem);

    this.setMenuBar(menubar);
  }

  public void actionPerformed(ActionEvent e) {
    String cmd = e.getActionCommand();

    if (cmd.equals(NEW))
      new GuiConsole(language, Environment.getGlobal(), false);
    else if (cmd.equals(NEW_SHARED))
      new GuiConsole(language, environment, true);
    else if (cmd.equals(EXIT))
      System.exit(0);
    else if (cmd.equals(CLOSE))
      close();
    else if (cmd.equals(PURGE_MESSAGE)) {
      message.deleteOldText();
    }
    else
      OutPort.outDefault().println("Unknown menu action: "+cmd);
  }
}
