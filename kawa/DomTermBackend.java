package kawa;

import org.domterm.*;
import org.domterm.util.DomTermErrorWriter;
import org.domterm.util.Utf8WriterOutputStream;
import java.io.*;
import gnu.expr.*;
import gnu.mapping.*;
import gnu.kawa.io.*;

/** An implementation of DomTerm's Backend that runs a Kawa REPL. */

public class DomTermBackend extends Backend implements Runnable {
    Language language;
    QueueReader inIn;
    Appendable inOut;
    OutputStream inOutS;
    boolean usingJLine;

    public DomTermBackend(Language language, Environment penvironment,
                          boolean shared) {
        this.language = language;
    }
    public DomTermBackend() {
        this(Language.getDefaultLanguage(), Environment.getCurrent(), false);
    }

    public void run() {
        Writer errWriter = new DomTermErrorWriter(termWriter);
        OutPort outp = new OutPort(termWriter, true, true,
                                   Path.valueOf("/dev/stdout"));
        outp.setDomTerm(true);
        Path inPath = Path.valueOf("/dev/stdin");
        TtyInPort in_p = null;
        int useJLine = CheckConsole.useJLine();
        if (useJLine >= 0) {
            try {
                PipedInputStream in = new PipedInputStream();

                inOutS = new PipedOutputStream(in);
                in_p = (TtyInPort)
                    Class.forName("gnu.kawa.io.JLineInPort")
                    .getConstructor(java.io.InputStream.class,
                                    gnu.kawa.io.Path.class,
                                    java.io.OutputStream.class,
                                    gnu.kawa.io.OutPort.class)
                    .newInstance(in, (Path) inPath, (OutputStream) (new Utf8WriterOutputStream(outp)), (OutPort) outp);
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
        OutPort.setErrDefault(new OutPort(errWriter, true, true,
                                          Path.valueOf("/dev/stderr")));
        Environment env = Environment.getCurrent();
        /*
        if (shared)
            env.setIndirectDefines();
        environment = env;
        */
        try {
            sendInputMode(usingJLine ? 'c' : 'p');
            setAutomaticNewline(true);
        } catch (Throwable ex) { ex.printStackTrace(); }
        Shell.run(language, env);
    }

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
}
