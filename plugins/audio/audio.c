#include <windows.h>
#include <mmsystem.h>
#include <mmreg.h>
#include "radRmodule.h"

// maximum size of string (data or error) returned by mci functions, according to
// http://msdn.microsoft.com/en-us/library/ms709479(VS.85).aspx

#define MCI_RV_BUFFSIZE 128  

SEXP mci_send_strings (SEXP strings) {
  // send a vector of strings to the mci system
  // returning the corresponding vector of returned strings
  int n;
  char rvs[MCI_RV_BUFFSIZE + 1];
  SEXP rv;
  int i;
  DWORD err;

  n = LENGTH(strings);
  PROTECT(rv = allocVector(STRSXP, n));
  for (i = 0; i < n; ++i) {
    if ((err =  mciSendString((LPCTSTR) CHAR(STRING_ELT(strings, i)), (LPTSTR) rvs, MCI_RV_BUFFSIZE, 0))) {
      mciGetErrorString (err, (LPTSTR) rvs, MCI_RV_BUFFSIZE);
      error("command '%s' caused error:\n%s\n", CHAR(STRING_ELT(strings, i)), rvs);
    }
    SET_STRING_ELT(rv, i, mkChar(rvs));
  }
  UNPROTECT(1);
  return rv;
}

R_CallMethodDef audio_call_methods[] = {
  MKREF(mci_send_strings, 1),
  {NULL, NULL, 0}
};

void
R_init_audio(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, audio_call_methods, NULL, NULL);
}

void
R_unload_audio(DllInfo *info)
{
  /* Release resources. */
}

