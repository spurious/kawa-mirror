package kawa;

import java.awt.*;
import javax.swing.*;
import java.awt.event.*;
import gnu.mapping.*;
import gnu.expr.Language;
import kawa.standard.Scheme;

/** A Frame containing a Kawa read-eval-print loop.
  * @author Albert Ting <alt@artisan.com> (original base)
  * @author Per Bothner (extensive changes).
  */

public class GuiConsole extends JFrame implements ActionListener {
  private static String CLOSE = "Close";
  private static String EXIT = "Exit";
  private static String NEW = "New";
  private static String NEW_SHARED = "New (Shared)";
  private static String PURGE_MESSAGE = "Purge Buffer";

  static int window_number = 0;

  ReplPane pane;
  ReplDocument document;

  public static void main(String[] args) {
    new GuiConsole();
  }

  public GuiConsole()
  {
    this(Language.getDefaultLanguage(), Environment.getCurrent(), false);
  }

  public GuiConsole(Language language, Environment penvironment, boolean shared)
  {
    super("Kawa");

    document = new ReplDocument(language, penvironment, shared);
    pane = new ReplPane(document);
    window_number++;
    kawa.repl.exitIncrement();
    OutPort out_p = pane.getStdout();
    OutPort err_p = pane.getStderr();
    this.setLayout(new BorderLayout(0,0));
    this.add("Center", new JScrollPane(pane));
    // Code for testing same ReplDocument in two JFrames.
    if (false)
      {
        JFrame other = new JFrame("frame2");
        other.setLayout(new BorderLayout(0,0));
        other.add("Center", new JScrollPane(new ReplPane(pane.document)));
        other.setSize(700,500);
        other.setVisible(true);
      }

    setupMenus();
    //pack();
    setLocation(100 * window_number, 50 * window_number);
    setSize(700,500);
    setVisible(true);

  }

  void close () {
    pane.document.close(); 
    dispose();
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

  public void actionPerformed(ActionEvent e)
  {
    String cmd = e.getActionCommand();

    if (cmd.equals(NEW))
      new GuiConsole(document.language, Environment.getGlobal(), false);
    else if (cmd.equals(NEW_SHARED))
      new GuiConsole(document.language, document.environment, true);
    else if (cmd.equals(EXIT))
      System.exit(0);
    else if (cmd.equals(CLOSE))
      close();
    else if (cmd.equals(PURGE_MESSAGE)) {
      pane.document.deleteOldText();
    }
    else
      OutPort.outDefault().println("Unknown menu action: "+cmd);
  }
}
