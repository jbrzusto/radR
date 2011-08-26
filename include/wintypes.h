/* create definitions of standard windows types needed for parsing
   Rutter's seascan headers */
   
#ifdef Win32

#include <windows.h>

#define MS_STRUCT 
#define MS_STRUCT_FILLER(X, A) 

#else
// Define filler elements so the alignment of structs works correctly
// on unix.
// FIXME:  how does this work in MINGW?  Seems I've tried every possible
// compiler switch and __attributes__(()) approach to making this work!

#define MS_STRUCT __attribute__((packed))
#define MS_STRUCT_FILLER(NAME, BYTES) char __filler__##NAME [BYTES]

// typedefs needed for IDLCONST.H and IDLTYPES.H

typedef long LONG;
typedef unsigned long ULONG;
typedef unsigned short USHORT;
typedef unsigned char BYTE;
typedef int BOOL;
typedef short SHORT;
typedef unsigned short WORD;
typedef unsigned char UCHAR;
typedef char CHAR;
typedef void * HANDLE;
typedef long long LONGLONG;
typedef unsigned long long ULONGLONG;
typedef float FLOAT;
typedef double DOUBLE;
typedef unsigned int DWORD;
#define __stdcall 

typedef union _LARGE_INTEGER {
  struct {
    DWORD LowPart;
    LONG HighPart;
  } u;
  LONGLONG QuadPart;
}  MS_STRUCT LARGE_INTEGER, *PLARGE_INTEGER;

typedef union _ULARGE_INTEGER {
  struct {
    DWORD LowPart;
    DWORD HighPart;
  } u;
  ULONGLONG QuadPart;
}  MS_STRUCT ULARGE_INTEGER, *PULARGE_INTEGER;

typedef struct _SYSTEMTIME {
        WORD wYear;
        WORD wMonth;
        WORD wDayOfWeek;
        WORD wDay;
        WORD wHour;
        WORD wMinute;
        WORD wSecond;
        WORD wMilliseconds;
}   MS_STRUCT SYSTEMTIME,*LPSYSTEMTIME;
#endif
