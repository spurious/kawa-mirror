import kawa.lang.*;
import kawa.standard.*;

import java.io.*;

class kawa {

   public static void main(java.lang.String args[]) {

      boolean usingSystem = true;
      InputStream stream = null;
      if (args.length>0) {
         try {
            stream = new FileInputStream(args[0]);
            usingSystem = false;
         } catch (FileNotFoundException e) {
            System.out.println("Cannot open file "+args[0]);
            System.exit(1);
         }
      } else {
         stream = System.in;
      }
      kawa.standard.StandardInterpreter interpreter =
         new kawa.standard.StandardInterpreter(
            new kawa.lang.iport(stream),
            new kawa.lang.oport(System.out),
            new kawa.lang.oport(System.err)
         );

      kawa.Shell shell = new kawa.Shell(
         interpreter,
         usingSystem,  // Display prompt only if input is from System.in
         usingSystem   // Display result only if input is from System.in
      );

      shell.run();

   }
}
