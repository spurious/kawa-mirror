package kawa;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import kawa.lang.InPort;
import kawa.lang.TtyInPort;
import kawa.lang.OutPort;
import kawa.standard.Scheme;
import kawa.lang.Interpreter;
import kawa.lang.Future;

/** A Frame containing a Kwa read-eval-print loop.
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

  public static int numConsoles = 0;

  Interpreter interp;
  Future thread;

  kawa.lang.QueueReader in_r;
  OutPort out_p, err_p;

  MessageArea message = null;

  public static void main(String[] args) {
    new GuiConsole(new Scheme());
  }

  public GuiConsole(Interpreter interp) {
    super("Kawa");
    this.interp = interp;

    in_r = new kawa.lang.QueueReader ();
    message = new MessageArea(false, in_r);
    window_number++;
    numConsoles++;

    out_p = new OutPort(message.getStdout(),"<msg_stdout>");
    err_p = new OutPort(message.getStderr(),"<msg_stderr>");
    InPort in_p = new TtyInPort(in_r, "<msg_stdin>", out_p);

    this.setLayout(new BorderLayout(0,0));

    this.add("Center",message);

    setupMenus();
    setLocation(100 * window_number, 50 * window_number);
    setSize(700,500);
    setVisible(true);

    thread = new Future (new kawa.Shell(interp, in_p, out_p, err_p));
    thread.start();
  }

  private void close () {
    in_r.appendEOF();
    numConsoles--;
    dispose();
    // Give thread chance to finish and clean up
    try {
      Thread.sleep(100);
    } catch (InterruptedException ex) {
    }
    if (numConsoles <= 0)
      System.exit(0);
    // Thread.stop is deprecated in JDK 1.2, but I see no good
    // alternative.  (Thread.destroy is not implemented!)
    thread.stop();
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
      new GuiConsole(new Scheme());
    else if (cmd.equals(NEW_SHARED))
      new GuiConsole(interp);
    else if (cmd.equals(EXIT))
      System.exit(0);
    else if (cmd.equals(CLOSE))
      close();
    else if (cmd.equals(PURGE_MESSAGE))
      message.setText("");
    else
      OutPort.outDefault().println("Unknown menu action: "+cmd);
  }
}
