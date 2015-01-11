package gnu.kawa.io;

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
        if (haveConsole < 0)
            return false;
        if (haveConsole > 0)
            return true;
        /* #ifdef JAVA6 */
        return System.console() != null;
        /* #else */
        // return true;
        /* #endif */
    }
}
