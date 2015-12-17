package gnu.kawa.io;

import gnu.lists.Consumer;

/** Helper class to decide if we have an interactive console.
 * This needs to be separate from InPort, since the latter uses haveConsole
 * in its static constructor, but we may need to call setHaveConsole first.
 */

public class CheckConsole {

    /** Have --console or --non-console been specified?
     * The value of haveConsole is initially 0;
     * it is 1 if {@code --console} was specified on the command line;
     * it is -1 if {@code --no-console} was specified on the command line.
     */
    private static int haveConsole;

    public static void setHaveConsole(boolean value) {
        haveConsole = value ? 1 : -1;
    }

    public static boolean haveConsole() {
        if (haveConsole > 0 || getDomTermVersionInfo() != null)
            return true;
        if (haveConsole < 0)
            return false;
        /* #ifdef JAVA6 */
        return System.console() != null;
        /* #else */
        // return true;
        /* #endif */
    }

    /** Check if parameter is a DomTerm console. */
    public static boolean forDomTerm(Consumer out) {
        return out == OutPort.getSystemOut()
            && getDomTermVersionInfo() != null;
    }

    /** Return DomTerm version info, or null if not running under DomTerm.
     * The version info is found from either the "org.domterm" property,
     * or the DOMTERM environment variable, tried in that order.
     */
    public static String getDomTermVersionInfo() {
        return versionInfoDomTerm;
    }

    static String versionInfoDomTerm;
    static {
        try {
            String version = System.getProperty("org.domterm");
            if (version == null)
                version = System.getenv("DOMTERM");
            if (version != null) {
                version = version.trim();
                if (version.length() > 0)
                    versionInfoDomTerm = version;
            }
        } catch (Throwable ex) {
            // ignore
        }
    }
}
