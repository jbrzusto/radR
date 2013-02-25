/* create definitions of standard windows types needed for parsing
   Rutter's seascan headers */
   
#ifdef Win32

#include <windows.h>

#define MS_STRUCT 
#define MS_STRUCT_FILLER(X, A) 

#else

#include <stdint.h>

// Define filler elements so the alignment of structs works correctly
// on unix.
// FIXME:  how does this work in MINGW?  Seems I've tried every possible
// compiler switch and __attributes__(()) approach to making this work!

#define MS_STRUCT __attribute__((packed))
#define MS_STRUCT_FILLER(NAME, BYTES) char __filler__##NAME [BYTES]
//#define MS_STRUCT_FILLER(NAME, BYTES)
// typedefs needed for IDLCONST.H and IDLTYPES.H

typedef int32_t LONG;
typedef uint32_t ULONG;
typedef uint16_t USHORT;
typedef unsigned char BYTE;
typedef int32_t BOOL;
typedef int16_t SHORT;
typedef uint16_t WORD;
typedef unsigned char UCHAR;
typedef char CHAR;
typedef void * HANDLE;
typedef int64_t LONGLONG;
typedef uint64_t ULONGLONG;
typedef float FLOAT;
typedef double DOUBLE;
typedef uint32_t DWORD;
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
