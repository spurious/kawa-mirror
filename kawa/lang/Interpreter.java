package kawa.lang;

//-- Java dependancies

import java.util.Hashtable;
import java.util.Vector;

//-- kawa Primitives
import kawa.lang.Printable; 
import kawa.lang.Executable; 
import kawa.lang.Syntaxable; 
import kawa.lang.snull;
import kawa.lang.iport;
import kawa.lang.oport;
import kawa.lang.symbol;
import kawa.lang.Exit;

// Exceptions
import kawa.lang.EOFInString;
import kawa.lang.EOFInComment;
import kawa.lang.InvalidPoundConstruct;
import kawa.lang.UnexpectedCloseParen;
import kawa.lang.NumberTooLong;
import kawa.lang.InvalidCharacterName;
import kawa.lang.MalformedList;
import kawa.lang.UnboundSymbol;
import kawa.lang.WrongArguments;
import kawa.lang.WrongType;

// To be fixed things
import kawa.lang.NotImplemented;

public class Interpreter extends Object {

   public java.lang.Boolean   trueObject;
   public java.lang.Boolean   falseObject;
   public kawa.lang.snull  nullObject;
   public kawa.lang.Undefined undefinedObject;

   private final int EOFChar = -1;
   private char buffer[];
   private char ibuffer[];
   private int bufferLength;
   private int bindex;
   private boolean initialized;

   protected java.util.Hashtable symbolTable;
   protected java.util.Vector executionFrames;
   //-- Environment variables
   protected java.util.Hashtable globals;
   protected Lambda lambda;
   protected Quote           quote;
   protected QuasiQuote      quasiquote;
   protected Unquote         unquote;
   protected UnquoteSplicing unquotesplicing;

   public kawa.lang.iport in;
   public kawa.lang.oport out;
   public kawa.lang.oport err;

   public Interpreter(
      kawa.lang.iport i,
      kawa.lang.oport o,
      kawa.lang.oport e
    ) {

      in = i;
      out = o;
      err = e;

      bufferLength    = 1024;
      buffer          = new char[bufferLength];
      ibuffer         = new char[bufferLength];
      trueObject      = new java.lang.Boolean(true);
      falseObject     = new java.lang.Boolean(false);
      nullObject      = new kawa.lang.snull();
      undefinedObject = new kawa.lang.Undefined();
      symbolTable     = new java.util.Hashtable();
      executionFrames = new java.util.Vector();
      globals         = new java.util.Hashtable();
      lambda          = new kawa.lang.Lambda();
      quote           = new kawa.lang.Quote();
      quasiquote      = new kawa.lang.QuasiQuote();
      unquote         = new kawa.lang.Unquote();
      unquotesplicing = new kawa.lang.UnquoteSplicing();

      define(lambda.name,lambda);
      define(quote.name,quote);
      define(quasiquote.name,quasiquote);
      define(unquote.name,unquote);
      define(unquotesplicing.name,unquotesplicing);

   }

   public void define(java.lang.String name,Object p) {
      globals.put(name,p);
   }

   public Object lookup(java.lang.String name) {
      return globals.get(name);
   }

   public kawa.lang.pair copy(kawa.lang.pair list) {
      kawa.lang.pair newlist = new kawa.lang.pair(list.car,list.cdr);
      kawa.lang.pair current = newlist;
      while (list.cdr instanceof kawa.lang.pair) {
         list = (kawa.lang.pair)list.cdr;
         kawa.lang.pair pair = new kawa.lang.pair(list.car,list.cdr);
         current.cdr = pair;
         current = pair;
      }
      current.cdr = list.cdr;
      return newlist;
   }

   public kawa.lang.pair lastpair(kawa.lang.pair list) {
      while (list.cdr instanceof kawa.lang.pair) {
         list = (kawa.lang.pair)list.cdr;
      }
      return list;
   }

   public Object evalSymbol(kawa.lang.symbol symbol,java.util.Vector frames) {
      if (frames!=null) {
         int length = frames.size();
         for (int i=length-1; i>=0; i--) {
            java.util.Hashtable frame = (java.util.Hashtable)frames.elementAt(i);
            Object p = frame.get(symbol.name);
            if (p!=null) {
               return p;
            }
         } 
      }
      return globals.get(symbol.name);
   }

   public Object evalpair(kawa.lang.pair pair,java.util.Vector frames) 
      throws kawa.lang.UnboundSymbol,
             kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      Object rator = eval(pair.car,frames);

      //((Printable)rator).print(System.out);
      //System.out.println("");

      //-- TODO: must handle macro

      if (rator instanceof kawa.lang.Executable) {
         //System.out.println("executing procedure...");
         Object randsarg = nullObject;
         pair current = null;
         Object rands = pair.cdr;
         while (rands instanceof kawa.lang.pair) {
            kawa.lang.pair randspair = (kawa.lang.pair)rands;
            //if (randspair.car instanceof Printable) {
            //   System.out.println("Before eval: ");
            //   ((kawa.lang.Printable)randspair.car).print(System.out);
            //   System.out.println("");
            //}
            if (current==null) {
               current = new kawa.lang.pair(eval(randspair.car,frames),nullObject);
               randsarg = current;
            } else {
               current.cdr = new kawa.lang.pair(eval(randspair.car,frames),nullObject);
               current = (kawa.lang.pair)current.cdr;
            }
            //if (randspair.car instanceof Printable) {
            //   System.out.println("After eval: ");
            //   ((kawa.lang.Printable)randspair.car).print(System.out);
            //   System.out.println("");
            //}
            rands = randspair.cdr;
         }
         return apply(rator,randsarg,frames);
      } else if (rator instanceof kawa.lang.Syntaxable) {
         //System.out.println("executing syntax...");
         return apply(rator,pair.cdr,frames);
      } else {
         throw new kawa.lang.WrongType("eval pair",0,"syntax or procedure");
      }
   }
                                                          
   public Object apply(Object rator,Object rands,java.util.Vector frames)
      throws kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError,
             kawa.lang.UnboundSymbol
   {
      if (rator instanceof kawa.lang.Executable) {
         Executable proc = (Executable)rator;
         return proc.execute(this,frames,rands);
      } else if (rator instanceof kawa.lang.Syntaxable) {
         Syntaxable s = (Syntaxable)rator;
         return s.execute(this,frames,rands);
      } else {
         //-- TODO: Must handle closures and continuations
         return undefinedObject;
      }
   }

   public Object eval(Object obj) 
      throws kawa.lang.UnboundSymbol,
             kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      return eval(obj,executionFrames);
   }

   public Object eval(Object obj,java.util.Vector frames) 
      throws kawa.lang.UnboundSymbol,
             kawa.lang.WrongArguments,
             kawa.lang.WrongType,
             kawa.lang.GenericError
   {
      if (obj instanceof kawa.lang.symbol) {
         //System.out.println("Looking up symbol '"+((kawa.lang.symbol)obj).name+"'...");
         Object eobj = evalSymbol((kawa.lang.symbol)obj,frames);
         if (eobj==null) {
            throw new kawa.lang.UnboundSymbol(((kawa.lang.symbol)obj).name);
         }
         return eobj;
      } else if (obj instanceof kawa.lang.pair) {
         //System.out.println("Evaling pair...");
         return evalpair((kawa.lang.pair)obj,frames);
      } else {
         return obj;
      }
   }
 
   public Object read()
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      return read(in);
   }

   public Object read(iport ip)
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      //System.out.println("interpreter.read():");
      int c;
      while (true) {
         c = ip.peek();
         while (java.lang.Character.isSpace((char)c)) {
            ip.flushPeek();
            c = ip.peek();
         } 
         switch (c) {
            case EOFChar:
               return nullObject;
            case ';':
               do {
                  c = ip.read();
                  if (c==EOFChar) {
                     return nullObject;
                  }
               } while (c!='\n');
            break;
            case ')':
               throw new UnexpectedCloseParen();
            case '(':
               ip.flushPeek();
               return readList(ip);
            case '"':
               ip.flushPeek();
               return readString(ip);
            case '\'':
               ip.flushPeek();
               return readQuote(ip);
            case '`':
               ip.flushPeek();
               return readQuasiQuote(ip);
            case ',':
               ip.flushPeek();
               if (ip.peek()=='@') {
                  return readUnquoteSplicing(ip);
               } else {
                  return readUnquote(ip);
               }
            case '+':
            case '-':
               if (java.lang.Character.isDigit((char)ip.peek())) {
                  return readNumber(ip);
               } else {
                  return readSymbol(ip);
               }
            case '#':
               ip.flushPeek();
               switch (ip.read()) {
                  case '(':
                     return readVector(ip);
                  case '\\':
                     return readCharacter(ip);
                  case 't':
                     return trueObject;
                  case 'f':
                     return falseObject;
                  case 'x':
                     return readHexNumber(ip);
                  case 'b':
                     return readBinaryNumber(ip);
                  case 'o':
                     return readOctalNumber(ip);
                  case '|':
                     boolean notAtEnd = true;
                     do {
                        c = ip.read();
                        if (c==EOFChar) {
                           throw new EOFInComment();
                        }
                        if (c=='|' && ip.read()=='#') {
                           notAtEnd = false;
                        }
                     } while (notAtEnd);
                  break;
                  default:
                     throw new InvalidPoundConstruct();
               }
            break;
            default:
               if (java.lang.Character.isDigit((char)c)) {
                  return readNumber(ip);
               } else {
                  return readSymbol(ip);
               }
         }
      } 
   }

   protected void skipWhitespaceAndComments(iport ip)
      throws java.io.IOException
   {
      
      boolean notAtEnd = true;
      int c;
      while (notAtEnd) {
         while ((c = ip.peek())!=EOFChar && java.lang.Character.isSpace((char)c)) {
            ip.flushPeek();
         }
         if (c==';') {
            while ((c = ip.read())!='\n' && c!=EOFChar);
         } else {
            notAtEnd = false;
         }
      }

      ip.unpeek();
      
   }


   protected Object readList(iport ip)
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      //System.out.println("interpreter.readList():");
      skipWhitespaceAndComments(ip);
      //-- null Primitive
      int c;
      if ((c = ip.peek())==')') {
         ip.flushPeek();
         return nullObject;
      }

      ip.unpeek();

      //-- Car of the list
      Object car = read(ip);
      Object cdr = nullObject;
      skipWhitespaceAndComments(ip);

      if ((c = ip.peek())!=')') {
         if (c=='.') {
            int next;
            if ((next = ip.peek())!=EOFChar && java.lang.Character.isSpace((char)next)) {
               //-- Read the cdr for the pair
               cdr = read(ip);
               skipWhitespaceAndComments(ip);
               if (ip.read()!=')') {
                  throw new MalformedList();
               }
            } else {
               //-- Read the rest of the list
               ip.unpeek();
               ip.unpeek();
               cdr = readList(ip);
            }
         } else {
            //-- Read the read of the list
            ip.unpeek();
            cdr = readList(ip);
         }           
      } else {
         ip.flushPeek();
      }
      return new kawa.lang.pair(car,cdr);
   }

   protected Object readString(iport ip)
      throws EOFInString,
             java.io.IOException
   {
      java.lang.StringBuffer obj = new java.lang.StringBuffer();
      boolean inString = true;
      int c;
      bindex = 0;
      do {
         c = ip.read();
         switch (c) {
            case '"':
               inString = false;
            break;
            case '\\':
               switch (c = ip.read()) {
                  case '"':
                  case '\\':
                     buffer[bindex++] = (char)c;
                  break;
                  default:
                     if (c==EOFChar) {
                        // Throw EOF in String exception
                        throw new EOFInString();
                     } else {
                        buffer[bindex++] = '\\';
                        buffer[bindex++] = (char)c;
                     }
                  break;
               }
            break;
            default:
               if (c==EOFChar) {
                  // Throw EOF in string exception
                  throw new EOFInString();
               } else {
                  buffer[bindex++] = (char)c;
                  if (bindex>bufferLength) {
                     obj.append(buffer);
                     bindex = 0;
                  }
               }
            break;
         }
      } while (inString);
      if (bindex!=0) {
         obj.append(buffer,0,bindex);
      }
      return obj;
   }

   protected Object readQuote(iport ip) 
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      return new kawa.lang.pair(
         quote,
         new kawa.lang.pair(
            read(ip),
            nullObject
         )
      );
   }

   protected Object readQuasiQuote(iport ip) 
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      return new kawa.lang.pair(
         quasiquote,
         new kawa.lang.pair(
            read(ip),
            nullObject
         )
      );
   }

   protected Object readUnquoteSplicing(iport ip) 
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      return new kawa.lang.pair(
         unquotesplicing,
         new kawa.lang.pair(
            read(ip),
            nullObject
         )
      );
   }

   protected Object readUnquote(iport ip) 
      throws java.io.IOException,
             UnexpectedCloseParen,
             EOFInString,
             EOFInComment,
             InvalidPoundConstruct,
             NumberTooLong,
             InvalidCharacterName,
             MalformedList,
             NotImplemented
   {
      return new kawa.lang.pair(
         unquote,
         new kawa.lang.pair(
            read(ip),
            nullObject
         )
      );
   }

   protected Object readNumber(iport ip) 
      throws java.io.IOException,
             NumberTooLong
   {
      Object obj = nullObject;
      int c = ip.read();

      boolean isFloat = false;
      bindex = 0;
      if (c=='+') {
         buffer[bindex++] = (char)c;
         c = ip.read();
      } else if (c=='-') {
         buffer[bindex++] = (char)c;
         c = ip.read();
      }
      do {
         if (bindex>bufferLength) {
            throw new NumberTooLong();
         } else if (c=='.' || c=='e' || c=='E') {
            isFloat = true;
         }
         buffer[bindex++] = (char)c;
      } while (java.lang.Character.isDigit((char)(c = ip.read())) || c=='.' || c=='e' || c=='E');

      ip.putback(c);

      if (isFloat) {
         obj = Double.valueOf(java.lang.String.copyValueOf(buffer,0,bindex));
      } else {
         obj = Integer.valueOf(java.lang.String.copyValueOf(buffer,0,bindex));
      }
      return obj;
   }

   protected Object readSymbol(iport ip) 
      throws java.io.IOException 
   {
      bindex = 0;
      int c;
      while (!java.lang.Character.isSpace((char)(c = ip.read())) &&
             c!=')' && c!='(' && c!='"' && c!=';' && c!=EOFChar) {
         buffer[bindex++] = (char)java.lang.Character.toLowerCase((char)c);
      }
      if (c!=EOFChar) {
         ip.putback(c);
      }
      if (bindex!=0) {
         java.lang.String symname = java.lang.String.copyValueOf(buffer,0,bindex);
         kawa.lang.symbol symbol = (kawa.lang.symbol)symbolTable.get(symname);
         if (symbol==null) {
            symbol = new kawa.lang.symbol(symname);
         }
         return symbol;
      } else {
         return nullObject;
      }
   }

   protected Object readVector(iport ip) throws NotImplemented {
      throw new NotImplemented();
   }

   protected boolean matches(iport ip,java.lang.String s)
      throws java.io.IOException
   {
      int length = s.length();
      int i;
      for (i=0; i<length; i++) {
         if (java.lang.Character.toLowerCase((char)ip.read())!=s.charAt(i)) {
            break;
         }
      }
      if (i<length) {
         return false;
      } else {
         // TODO: Should check to see if there is garbage on the end
         return true;
      }
   }

   protected Object readCharacter(iport ip)
      throws java.io.IOException,
             InvalidCharacterName
   {
      int c;
      int nc;
      Object obj = nullObject;
      switch (c = ip.read()) {
         case 'n': // newline
         case 'N':
            nc = ip.peek();
            if (nc=='e' || nc=='E') {
               ip.flushPeek();
               if (matches(ip,"wline")) {
                  obj = new java.lang.Character('\n');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 's': // space
         case 'S':
            nc = ip.peek();
            if (nc=='p' || nc=='P') {
               ip.flushPeek();
               if (matches(ip,"ace")) {
                  obj = new java.lang.Character(' ');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 'r': // rubout
         case 'R':
            nc = ip.peek();
            if (nc=='u' || nc=='U') {
               ip.flushPeek();
               if (matches(ip,"bout")) {
                  obj = new java.lang.Character((char)0x7f);
               } else {
                  throw new InvalidCharacterName();
               }
            } else if (nc=='e' || nc=='E') {
               ip.flushPeek();
               if (matches(ip,"turn")) {
                  obj = new java.lang.Character('\r');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 'p': // page
         case 'P':
            nc = ip.peek();
            if (nc=='a' || nc=='A') {
               ip.flushPeek();
               if (matches(ip,"ge")) {
                  obj = new java.lang.Character('\f');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 't': // tab
         case 'T':
            nc = ip.peek();
            if (nc=='a' || nc=='A') {
               ip.flushPeek();
               if (matches(ip,"ab")) {
                  obj = new java.lang.Character('\t');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 'b': // backspace
         case 'B':
            nc = ip.peek();
            if (nc=='a' || nc=='A') {
               ip.flushPeek();
               if (matches(ip,"ckspace")) {
                  obj = new java.lang.Character('\b');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         case 'l': // linefeed
         case 'L':
            nc = ip.peek();
            if (nc=='i' || nc=='I') {
               ip.flushPeek();
               if (matches(ip,"nefeed")) {
                  obj = new java.lang.Character('\n');
               } else {
                  throw new InvalidCharacterName();
               }
            } else {
               ip.unpeek();
               obj = new java.lang.Character((char)c);
            }
         break;
         default:
            obj = new java.lang.Character((char)c);
         break;
      }
      return obj;
   }

   protected Object readHexNumber(iport ip) throws NotImplemented {
      throw new NotImplemented();
   }

   protected Object readOctalNumber(iport ip) throws NotImplemented {
      throw new NotImplemented();
   }

   protected Object readBinaryNumber(iport ip) throws NotImplemented {
      throw new NotImplemented();
   }

}
