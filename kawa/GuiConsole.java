package kawa;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import kawa.lang.InPort;
import kawa.lang.TtyInPort;
import kawa.lang.OutPort;
import kawa.standard.Scheme;
import kawa.lang.Interpreter;

/** A Frame containing a Kwa read-eval-print loop.
  * @author Albert Ting <alt@artisan.com> (original base)
  * @author Per Bothner (extensive changes).
  */

public class GuiConsole extends Frame {
  private static String EXIT = "Exit";
  private static String NEW = "New";
  private static String PURGE_MESSAGE = "Purge Message";
  private static Scheme scheme;

  kawa.lang.QueueReader in_r;
  OutPort out_p, err_p;

  MessageArea message = null;

  public static void main(String[] args) {
    scheme = new Scheme();
    new GuiConsole(scheme);
  }

  public GuiConsole(Interpreter interp) {
    super("Kawa");

    in_r = new kawa.lang.QueueReader ();
    message = new MessageArea(false, in_r);
    
    out_p = new OutPort(message.getStdout(),"<msg_stdout>");
    err_p = new OutPort(message.getStderr(),"<msg_stderr>");
    InPort in_p = new TtyInPort(in_r, "<msg_stdin>", out_p);

    this.setLayout(new BorderLayout(0,0));

    this.add("Center",message);

    setupMenus();
    // setLocation(100,100);
    setSize(700,500);
    setVisible(true);

    new kawa.lang.Future (new kawa.Shell(interp, in_p, out_p, err_p)).run();
  }

  private void setupMenus() {
    MenuBar menubar;
    Menu fileMenu;
    Menu utilitiesMenu;

    MenuItem menuItem;

    ActionListener actionCmd = new _MenuAction();
    WindowListener windowExitCmd = new WindowAdapter() {
      public void windowClosing(WindowEvent e) {
	System.exit(0); // FIXME
      }
    };

    // Create the menubar
    menubar = new MenuBar();
    fileMenu = new Menu("File");
    utilitiesMenu = new Menu("Utilities");
   
    menubar.add(fileMenu);
    menubar.add(utilitiesMenu);
    
    menuItem = new MenuItem(NEW);
    menuItem.addActionListener(actionCmd);
    fileMenu.add(menuItem);

    menuItem = new MenuItem(EXIT);
    menuItem.addActionListener(actionCmd);
    this.addWindowListener(windowExitCmd);	
    fileMenu.add(menuItem);

    menuItem = new MenuItem(PURGE_MESSAGE);
    menuItem.addActionListener(actionCmd);	
    utilitiesMenu.add(menuItem);

    this.setMenuBar(menubar);
  }

  class _MenuAction implements ActionListener {
    public void actionPerformed(ActionEvent e) {
      String cmd = e.getActionCommand();

      if (cmd.equals(NEW)) {
	new GuiConsole(scheme);
      } else if (cmd.equals(EXIT)) {
	System.exit(0);
      } else if (cmd.equals(PURGE_MESSAGE)) {
	message.setText("");
      } else {
	OutPort.outDefault().println("Unknown menu action: "+cmd);
      }
    }
  }
}
