AC_DEFUN(LIBGCJ_CONFIGURE,
[
dnl Default to --enable-multilib
AC_ARG_ENABLE(multilib,
[  --enable-multilib       build many library versions (default)],
[case "${enableval}" in
  yes) multilib=yes ;;
  no)  multilib=no ;;
  *)   AC_MSG_ERROR(bad value ${enableval} for multilib option) ;;
 esac], [multilib=yes])dnl

dnl We may get other options which we dont document:
dnl --with-target-subdir, --with-multisrctop, --with-multisubdir

if test "[$]{srcdir}" = "."; then
  if test "[$]{with_target_subdir}" != "."; then
    libgcj_basedir="[$]{srcdir}/[$]{with_multisrctop}../$1"
  else
    libgcj_basedir="[$]{srcdir}/[$]{with_multisrctop}$1"
  fi
else
  libgcj_basedir="[$]{srcdir}/$1"
fi
AC_SUBST(libgcj_basedir)

AC_CANONICAL_HOST

dnl This shouldn't be needed, as long as top-level dependencies are
dnl defined correctly and shared-library paths are set up so that
dnl execution tests succeed.  FIXME.
define([AC_PROG_CC_WORKS],[])
define([AC_PROG_CXX_WORKS],[])

AC_PROG_CC
AC_PROG_CXX

dnl version is pulled out to make it a bit easier to change using sed.
version=0.0.7
dnl Still use "libjava" here to placate dejagnu.
AM_INIT_AUTOMAKE(libjava, $version)

# AC_CHECK_TOOL does AC_REQUIRE (AC_CANONICAL_BUILD).  If we dont
# run it explicitly here, it will be run implicitly before
# LIBGCJ_CONFIGURE, which doesn't work because that means that it will
# be run before AC_CANONICAL_HOST.
AC_CANONICAL_BUILD

AC_CHECK_TOOL(AS, as)
AC_CHECK_TOOL(AR, ar)
AC_CHECK_TOOL(RANLIB, ranlib, :)

AC_PROG_INSTALL

AM_MAINTAINER_MODE

# We need AC_EXEEXT to keep automake happy in cygnus mode.  However,
# at least currently, we never actually build a program, so we never
# need to use $(EXEEXT).  Moreover, the test for EXEEXT normally
# fails, because we are probably configuring with a cross compiler
# which cant create executables.  So we include AC_EXEEXT to keep
# automake happy, but we dont execute it, since we dont care about
# the result.
if false; then
  AC_EXEEXT
fi

# configure.host sets the following important variables
#	libgcj_cflags    - host specific C compiler flags
#	libgcj_cxxflags  - host specific C++ compiler flags
#	libgcj_javaflags - host specific Java compiler flags

libgcj_cflags=
libgcj_cxxflags=
libgcj_javaflags=

. [$]{libgcj_basedir}/configure.host

case [$]{libgcj_basedir} in
/* | [A-Za-z]:[/\\]*) libgcj_flagbasedir=[$]{libgcj_basedir} ;;
*) libgcj_flagbasedir='[$](top_builddir)/'[$]{libgcj_basedir} ;;
esac

LIBGCJ_CFLAGS="[$]{libgcj_cflags}"
LIBGCJ_CXXFLAGS="[$]{libgcj_cxxflags}"
LIBGCJ_JAVAFLAGS="[$]{libgcj_javaflags}"
AC_SUBST(LIBGCJ_CFLAGS)
AC_SUBST(LIBGCJ_CXXFLAGS)
AC_SUBST(LIBGCJ_JAVAFLAGS)
])dnl

sinclude(./libtool.m4)
dnl The lines below arrange for aclocal not to bring libtool.m4
dnl AM_PROG_LIBTOOL into aclocal.m4, while still arranging for automake
dnl to add a definition of LIBTOOL to Makefile.in.
ifelse(yes,no,[
AC_DEFUN([AM_PROG_LIBTOOL],)
AC_DEFUN([AC_LIBTOOL_DLOPEN],)
AC_DEFUN([AC_LIBLTDL_CONVENIENCE],)
AC_DEFUN([LT_AC_PROG_GCJ],)
AC_SUBST(GCJ)
AC_SUBST(LIBTOOL)
])
