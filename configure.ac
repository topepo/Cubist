AC_INIT([Cubist], [0.03])
AC_PROG_CC

PKG_CFLAGS="-g"
if test "x$GCC" = xyes; then
  PKG_CFLAGS="-ffloat-store -g"
fi

AC_SUBST(PKG_CFLAGS)
AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
