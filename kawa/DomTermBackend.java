package kawa;

import org.domterm.*;
import org.domterm.util.DomTermErrorWriter;
import org.domterm.util.StyleSheets;
import org.domterm.util.Utf8WriterOutputStream;
import org.domterm.websocket.DomServer;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.io.*;
import java.awt.Desktop;
import java.io.*;
import java.net.UnknownHostException;
import java.net.URI;

/** An implementation of DomTerm's Backend that runs a Kawa REPL. */

public class DomTermBackend extends Backend implements Runnable {

    public static class WebSocketServer extends org.domterm.websocket.DomServer {
        private static WebSocketServer instance = null;

        public WebSocketServer(int port) throws UnknownHostException {
            super(port, new String[0]);
        }
        @Override
        protected Backend createBackend() {
            return new DomTermBackend();
        }
        public synchronized static WebSocketServer getInstance() {
            if (instance == null) {
                try {
                    instance = new WebSocketServer(0);
                } catch (UnknownHostException ex) {
                    throw new RuntimeException(ex);
                }
                instance.start();
            }
            return instance;
        }
        public static int getInstancePort() {
            return getInstance().getPort();
        }
    }

    public static String startDomTermConsole(String command) throws Throwable {
        int htport = 0;
        if (command.startsWith("serve=")) {
            String portArg = command.substring(6);
            try {
                htport = Integer.parseInt(portArg);
            } catch (NumberFormatException ex) {
                return "bad port specifier in -w"+command+" option";
            }
        }
        boolean exitOnClose = ! command.startsWith("serve");
        int wsport = DomTermBackend.WebSocketServer.getInstancePort();
        DomServer.setExitOnClose(exitOnClose);
        String defaultUrl = "/repl-client.html#ws=//localhost:"+wsport+"/";
        String kawaHome = System.getProperty("kawa.home");
        if (kawaHome == null)
            return "kawa.home not set";
        File domtermJar = new File(kawaHome+"/lib/domterm.jar");
        if (! domtermJar.exists())
            return domtermJar.toString()+" does not exist";
        String pathPrefix = "jar:file:" + domtermJar + "!";
        gnu.kawa.servlet.KawaHttpHandler.addStaticFileHandler("/", pathPrefix, defaultUrl, exitOnClose);
        com.sun.net.httpserver.HttpServer httpHandler =
            gnu.kawa.servlet.KawaHttpHandler.startServer(htport, System.err);
        htport = httpHandler.getAddress().getPort();
        String webUrl = "http://127.0.0.1:"+htport+"/";
        if (command.equals("browser")) {
            if (! Desktop.isDesktopSupported())
                return "using default desktop browser not supported";
            Desktop.getDesktop().browse(new URI(webUrl));
            return null;
        } else if (command.startsWith("browser=")) {
            String cmd = command.substring(8);
            if (cmd.indexOf('%') < 0)
                cmd = cmd  + " %U";
            cmd = cmd.replace("%U", webUrl).replace("%W", Integer.toString(wsport));
            Runtime.getRuntime().exec(cmd);
            return null;
        }
        else if (command.startsWith("serve")) {
            return null;
        }
        return "unrecognized -w subcommand '"+command+"'";
    }

    Language language;
    QueueReader inIn;
    Appendable inOut;
    OutputStream inOutS;
    boolean usingJLine;
    TtyInPort in_p;
    PipedInputStream inPipe; // only if usingJLine
    java.lang.reflect.Method setSizeMethod;
    public static final ThreadLocation<DomTermBackend> instance
        = new ThreadLocation<DomTermBackend>("domterm-backend");

    public DomTermBackend(Language language, Environment penvironment,
                          boolean shared) {
        this.language = language;
    }
    public DomTermBackend() {
        this(Language.getDefaultLanguage(), Environment.getCurrent(), false);
    }

    @Override
    public void reportEvent(String name, String str) {
        if (name.equals("KEY") && str.equals("-3 \"\\u0003\"") // ctrl-C
            && in_p.sigIntHandler != null)
            in_p.sigIntHandler.run();
        else if (name.equals("KEY") && str.equals("-4 \"\\u0004\"") // ctrl-D
                 && inIn != null) {
            inIn.appendEOF();
        } else
            super.reportEvent(name, str);
    }

    public void run() {
        Writer errWriter = new DomTermErrorWriter(termWriter);
        OutPort outp = new OutPort(termWriter, true, true,
                                   Path.valueOf("/dev/stdout"));
        Path inPath = Path.valueOf("/dev/stdin");
        int useJLine = CheckConsole.useJLine();
        instance.set(this);
        if (useJLine >= 0) {
            try {
                inPipe = new PipedInputStream();

                inOutS = new PipedOutputStream(inPipe);
                Class JLineClass = Class.forName("gnu.kawa.io.JLineInPort");
                in_p = (TtyInPort)
                    JLineClass
                    .getConstructor(java.io.InputStream.class,
                                    gnu.kawa.io.Path.class,
                                    java.io.OutputStream.class,
                                    gnu.kawa.io.OutPort.class)
                    .newInstance(inPipe, (Path) inPath, (OutputStream) (new Utf8WriterOutputStream(outp)), (OutPort) outp);
                setSizeMethod =
                    JLineClass.getMethod("setSize", Integer.TYPE, Integer.TYPE);
                usingJLine = true;
            } catch (Throwable ex) {
                inOutS = null;
                if (useJLine > 0) {
                    // FIXME error in this case only
                    ex.printStackTrace();
                }
            }
        }
        if (in_p == null)
        {
            QueueReader inQ = new QueueReader() {
                @Override
                public void checkAvailable() {
                    //checkingPendingInput(); // See ReplDocument
                };
            };
            inIn = inQ;
            inOut = inQ;
            in_p = new TtyInPort(inIn, inPath, outp);
        }
        in_p.setInDomTerm(true);
        InPort.setInDefault(in_p);
        OutPort.setOutDefault(outp);
        OutPort errp = new OutPort(errWriter, true, true,
                                   Path.valueOf("/dev/stderr"));
        outp.setDomTerm(true);
        errp.setDomTerm(true);
        OutPort.setErrDefault(errp);
        Environment env = Environment.getCurrent();
        /*
        if (shared)
            env.setIndirectDefines();
        environment = env;
        */
        try {
            sendInputMode(usingJLine ? 'c' : 'p');
            setAutomaticNewline(true);
            termWriter.write("\033]0;Kawa\007");
        } catch (Throwable ex) { ex.printStackTrace(); }
        if (this.nrows >= 0)
            setWindowSize(nrows, ncols, pixw, pixh);
        Shell.run(language, env);
        try {
            termWriter.close();
        } catch (Throwable ex) {
            // ignore
        }
    }
    public volatile int nrows = -1, ncols = -1, pixw, pixh;

    public void run(Writer out) throws Exception {
        this.termWriter = out;
        //addVersionInfo(???");
        Thread thread = new Thread(this);
        thread.start();
    }
    public void processInputCharacters(String text) {
        try {
            if (inOutS != null) {
                // FIXME should probably not use getBytes
                inOutS.write(text.getBytes());
                inOutS.flush();
            }
            else if (inOut != null)
                inOut.append(text, 0, text.length());
        } catch (IOException ex) {
            throw new RuntimeException(ex);
        }
    }

    @Override
    public void setWindowSize(int nrows, int ncols, int pixw, int pixh) {
        this.nrows = nrows;
        this.ncols = ncols;
        this.pixw = pixw;
        this.pixh = pixh;
        if (in_p != null && setSizeMethod != null) {
            try {
                setSizeMethod.invoke(in_p, ncols, nrows);
            } catch (Throwable ex) {
                // ignore
            }
        }
    }

    public static void loadStyleSheet(String name, String fname)
            throws IOException {
        String command = StyleSheets.loadStyleSheetRequest(name, fname);
        Writer commandWriter;
        InPort breader;
        DomTermBackend backend = instance.get(null);
        if (backend != null) {
            breader = backend.usingJLine ? new BinaryInPort(backend.inPipe)
                : backend.in_p;
            commandWriter = backend.termWriter;
        } else {
            OutPort outDefault = OutPort.getSystemOut();
            InPort inDefault = InPort.inDefault();
            if (! outDefault.isDomTerm() || ! (inDefault instanceof TtyInPort))
                return; // ERROR
            commandWriter = outDefault;
            breader = inDefault;
        }
        commandWriter.write(command);
        commandWriter.flush();
        breader.mark(1);
        int ch = breader.read();
        if (ch == 0x9D) {
            String response = breader.readLine();
            //System.err.println("loadStyleSheet response: "+response);
        }
        if (ch >= 0)
            breader.reset();
        //System.err.println("(no response received)");
    }
}
