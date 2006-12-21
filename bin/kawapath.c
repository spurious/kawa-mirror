#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifndef GCJ_COMPILED
const char *kawalib = KAWALIB;
const char *kawajar = KAWAJAR;

char *
get_classpath(const char *progname)
{
  char *classpath;
  int i;
  const char *path = getenv("KAWALIB");
  if (path == NULL)
    {
      const char *rp = progname == NULL ? NULL : strrchr(progname, '/');
      path = kawalib;
      if (rp != NULL)
	{
	  /* If kawa is executed "in place" as $builddir/bin/kawa
	     (i.e. not the installed $bindir/kawa), then the kawajar
	     is $buildir/kawa-$version.jar.  Since it's nice to be able to
	     execute in place, look for it there first.  If we're executing
	     an installed kawa then the jar won't be there. */
	  char *buf = malloc (strlen(progname) + strlen(kawajar) + 5);
	  if (buf != NULL)
	    {
	      sprintf(buf, "%.*s/../%s", rp - progname, progname, kawajar);
	      if (access (buf, R_OK) == 0)
		path = buf;
	    }
	}
    }
  i = strlen (path);
  if (i > 4 && strcmp (path+i-4, ".jar") == 0)
    {
      if (access (path, R_OK) < 0)
	{
	  perror ("KAWALIB does not specify a readable .jar file");
	  exit(0);
	}
    }
  else
    {
      char *buf = malloc (i + 20);
      sprintf (buf, "%s/kawa/repl.class", path);
      if (access (buf, R_OK) < 0)
	{
	  perror ("KAWALIB does not contain kawa/repl.class");
	  exit(0);
	}
    }
  
  classpath = getenv ("CLASSPATH");
  if (classpath == NULL)
    {
      char *buf = malloc (strlen (path) + 20);
      sprintf (buf, "CLASSPATH=.:%s", path);
      classpath = buf;
    }
  else
    {
      char *buf = malloc (strlen (path) + strlen (classpath) + 20);
      sprintf (buf, "CLASSPATH=%s:%s", classpath, path);
      classpath = buf;
    }
  return classpath;
}
#endif
