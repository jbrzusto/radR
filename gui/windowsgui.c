#include "windowsgui.h"

/*

  Windows-specific gui functions for radR

*/

// Windows GUI variables

static HWND hwnd_plot = NULL;	            // handle of the radar plotting window
static PBITMAPINFO offscreen_bitmap_header = NULL;     // pointer to header for the offscreen bitmap
static HDC offscreen_dc = NULL;	            // offscreen device context
static HBITMAP offscreen_bitmap_handle = NULL;     // offscreen bitmap
static BITMAP offscreen_bitmap;
static int plot_width = -1;
static int plot_height = -1;
static WINDOWPLACEMENT plot_win_info;
static Tcl_Interp *my_tcl = NULL;             // the tcl interpreter created by R 
static typeof(extmat_to_sexp) *pextmat_to_sexp;

// the extmat into which radR scan-converts. We manage its
// memory here in a DIBSection.
static t_extmat pix_mat = CREATE_FOREIGN_EXTMAT(EXTMAT_TYPE_UINT, "radR matrix of 32-bit raw RGB values for the plot window");

SEXP
get_tcl_interp(SEXP hwnd) {
  // when passed the Windows HWND of a tk window, finds
  // the tcl interpreter associated with it
  // as an integer SEXP; It will be 0 if no interpreter was found.
  Tk_Window tw;
  tw = (Tk_Window) Tk_HWNDToWindow((HWND) INTEGER(AS_INTEGER(hwnd))[0]);
  return ScalarInteger((int) (my_tcl = (tw && ((TkWindow *) tw)->mainPtr) ? ((TkWindow *) tw)->mainPtr->interp : 0));
}

void
get_window_info() {
  plot_win_info.length = sizeof(WINDOWPLACEMENT);
  GetWindowPlacement(hwnd_plot, &plot_win_info);
  plot_width = plot_win_info.rcNormalPosition.right - plot_win_info.rcNormalPosition.left;
  plot_height = plot_win_info.rcNormalPosition.bottom - plot_win_info.rcNormalPosition.top;
}

void
ensure_offscreen_bitmap(int width, int height) {
  HDC hdc;

  if (width == pix_mat.cols && height == pix_mat.rows)
    return;

  if (offscreen_bitmap_handle)
    DeleteObject (offscreen_bitmap_handle);

  if (offscreen_dc)
    ReleaseDC (hwnd_plot, offscreen_dc);

  // create the offscreen hdc and bitmap
  hdc = GetDC (NULL);
  offscreen_dc = CreateCompatibleDC (hdc);
  //  offscreen_bitmap_handle = CreateCompatibleBitmap (hdc, width, height);
  offscreen_bitmap_header->bmiHeader.biWidth = width;
  offscreen_bitmap_header->bmiHeader.biHeight = -height;
  offscreen_bitmap_handle = CreateDIBSection (hdc, offscreen_bitmap_header, DIB_RGB_COLORS, (void **) &pix_mat.ptr, NULL, 0);
  pix_mat.rows = plot_height;
  pix_mat.cols = plot_width;
  pix_mat.slop = 0;
  pix_mat.alloc = EXTMAT_TYPE_UINT_SIZE * plot_width * plot_height;
  SelectObject (offscreen_dc, offscreen_bitmap_handle);
}
    

SEXP
have_plot_window(void) {
  return LOGICAL_SEXP(hwnd_plot != NULL);
}

SEXP
set_plot_window(SEXP s) {
  hwnd_plot = (HWND) INTEGER(AS_INTEGER(s))[0];
  return PASS_SEXP;
}

SEXP
prepare_for_plot() {
  get_window_info();
  ensure_offscreen_bitmap(plot_width, plot_height);
  GdiFlush();
  return (*pextmat_to_sexp)(&pix_mat);
}

SEXP
repaint_plot_window(SEXP coords) {
  // copy the offscreen plot bitmap to the window
  // or some subrectangle thereof
  // if coords is NULL, the entire
  // window is repainted, otherwise
  // coords should be left, top, width, height

  RECT r;
  int left, top, width, height;
  HDC hdc = GetDC(hwnd_plot);
  int rv;

  if (plot_win_info.showCmd == SW_SHOWMINIMIZED)
    return PASS_SEXP;

  if(coords && 0 != GET_LENGTH(coords)) {
    PROTECT(coords = AS_INTEGER(coords));
    left = INTEGER(coords)[0];
    top = INTEGER(coords)[1];
    width = INTEGER(coords)[2];
    height = INTEGER(coords)[3];
    UNPROTECT(1);
  } else {
    GetClientRect((HWND) hwnd_plot, &r);
    left = r.left;
    top = r.top;
    width = r.right - r.left;
    height = r.bottom - r.top;
  }
  rv = BitBlt( hdc,
	  left, top,
	  width, height,
	  offscreen_dc,
	  left, top,
	  SRCCOPY
	 ); 
  if (!rv)
    rv = GetLastError();
  ReleaseDC((HWND) hwnd_plot, hdc);
  return PASS_SEXP;
}

SEXP
show_no_plot(void)
{
  get_window_info();
  if (plot_win_info.showCmd == SW_SHOWMINIMIZED)
    return PASS_SEXP;

  ensure_offscreen_bitmap (plot_width, plot_height);
  SelectObject (offscreen_dc, GetStockObject(BLACK_PEN));
  SelectObject (offscreen_dc, GetStockObject(BLACK_BRUSH));
  Rectangle (offscreen_dc, 0, 0, plot_width, plot_height);
  repaint_plot_window (NULL);
  return PASS_SEXP;
}

SEXP
plot_range_rings(SEXP n, SEXP centre, SEXP spacing, SEXP rgb) {
  int i;
  int delta;
  RECT r;
  int wh;
  int red, green, blue;
  HGDIOBJ range_ring_pen;	            // a pen for plotting range rings
  int max_rings;

  PROTECT(spacing = AS_NUMERIC(spacing));
  PROTECT(centre = AS_INTEGER(centre));
  PROTECT(n = AS_INTEGER(n));
  PROTECT(rgb = AS_INTEGER(rgb));
  red = INTEGER(rgb)[0];
  green = INTEGER(rgb)[1];
  blue = INTEGER(rgb)[2];
  range_ring_pen = CreatePen(PS_DOT, 0, RGB(red, green, blue));
  max_rings = min(50, INTEGER(n)[0]);

  GetClientRect((HWND) hwnd_plot, &r);
  wh = r.bottom - r.top;

  SetBkMode(offscreen_dc, TRANSPARENT);
  SelectObject(offscreen_dc, range_ring_pen);
  SelectObject(offscreen_dc, GetStockObject(NULL_BRUSH));
  for (i=1; i <= max_rings; ++i) {
    delta = (int) (i * REAL(spacing)[0]);
    Ellipse(offscreen_dc, INTEGER(centre)[0] - delta, INTEGER(centre)[1] - delta, INTEGER(centre)[0] + delta, INTEGER(centre)[1] + delta);
  }
  UNPROTECT(4);
  DeletePen(range_ring_pen);
  return PASS_SEXP;
}

#define DIST(x, y) ({ double _X_ = x; double _Y_ = y; sqrt(_X_ * _X_ + _Y_ * _Y_);})

SEXP
plot_compass(SEXP centre, SEXP ticklengths, SEXP north_angle, SEXP radius, SEXP rgb) {

  int red, green, blue;
  int cx, cy;
  int ang;
  double scrang;
  int x, y, ex, ey;
  char lab_buf[20];
  int nchar;
  int tlen;
  int labwidth[3];
  int labheight[3];
  char *dummylabs[3] = {"0", "00", "000"};
  int anglab;
  int anglabs[] = {1, 2, 5, 10, 20, 30, 45, 90};
  int i;

  HGDIOBJ compass_pen;
  HGDIOBJ compass_font;
  SIZE labdims;

  PROTECT(ticklengths = AS_INTEGER(ticklengths));
  PROTECT(centre = AS_INTEGER(centre));
  PROTECT(north_angle = AS_NUMERIC(north_angle));
  PROTECT(radius = AS_INTEGER(radius));
  PROTECT(rgb = AS_INTEGER(rgb));
  
  red = INTEGER(rgb)[0];
  green = INTEGER(rgb)[1];
  blue = INTEGER(rgb)[2];
  cx = INTEGER(centre)[0];
  cy = INTEGER(centre)[1];
  compass_pen = CreatePen(PS_SOLID, 0, RGB(red, green, blue));
  compass_font = CreateFont(14, 0, 0, 0, FW_THIN, 0, 0, 0, ANSI_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DRAFT_QUALITY, FIXED_PITCH + FF_MODERN, NULL);

  SetBkMode(offscreen_dc, TRANSPARENT);
  SetTextColor(offscreen_dc, RGB(red, green, blue));
  SelectObject(offscreen_dc, compass_pen);
  SelectObject(offscreen_dc, compass_font);
  SetTextAlign(offscreen_dc, TA_BOTTOM | TA_LEFT);

  // set the labelling angle to the smallest one that still provides
  // comfortable spacing around the labels

  for(i = 0; i < 3; ++i ) {
    GetTextExtentPoint32(offscreen_dc, dummylabs[i], i + 1, &labdims);
    labwidth[i] = labdims.cx;
    labheight[i] = labdims.cy;
  }

  for (i = 0; i < sizeof(anglabs) / sizeof(int); ++i)
    if ((INTEGER(radius)[0] * anglabs[i] * M_PI / 180 > 2.5 * (max(labwidth[2], labheight[2]))))
      break;

  if (i == sizeof(anglabs) / sizeof(int))
    --i;

  anglab = anglabs[i];

  for (ang = 0; ang < 360; ++ang) {
    tlen = INTEGER(ticklengths)[0] * ((ang % 5) ? 1 : (ang % 10) ? 2 : 3);
    scrang = (90 - (ang + REAL(north_angle)[0])) * M_PI / 180;
    x = cx + INTEGER(radius)[0] * cos(scrang);
    y = cy - INTEGER(radius)[0] * sin(scrang);
    ex = cx + (INTEGER(radius)[0] + tlen) * cos(scrang);
    ey = cy - (INTEGER(radius)[0] + tlen) * sin(scrang);
    MoveToEx(offscreen_dc, x, y, NULL);
    LineTo(offscreen_dc, ex, ey);
    if (ang % anglab == 0) {
      nchar = sprintf(lab_buf, "%d", ang);
      ex += (-labwidth[nchar - 1] / 2 * (1 - cos(scrang))) + 5 * cos(scrang);
      ey -= - labheight[nchar - 1] / 2 * (1 - sin(scrang)) + 5 * sin(scrang);
      TextOut(offscreen_dc, ex, ey, lab_buf, nchar);
    }
  }
  DeleteFont(compass_font);
  DeletePen(compass_pen);
  UNPROTECT(5);
  return PASS_SEXP;
}


void 
create_plot_bitmap_headers() {

  /* 
     we use a 32bpp offscreen pixmap; wastes memory but simplifies coding
  */

  offscreen_bitmap_header = calloc(sizeof(BITMAPINFO) + 256 * sizeof(RGBQUAD), 1);
  offscreen_bitmap_header->bmiHeader.biSize = sizeof(BITMAPINFOHEADER);
  offscreen_bitmap_header->bmiHeader.biPlanes = 1;
  offscreen_bitmap_header->bmiHeader.biBitCount = 32;
  offscreen_bitmap_header->bmiHeader.biCompression = BI_RGB;
  offscreen_bitmap_header->bmiHeader.biSizeImage = 0;
  offscreen_bitmap_header->bmiHeader.biXPelsPerMeter = 0;
  offscreen_bitmap_header->bmiHeader.biYPelsPerMeter = 0;
  offscreen_bitmap_header->bmiHeader.biClrUsed = 256;
}


void
destroy_plot_bitmap_headers() {
  if (offscreen_bitmap_header) {
    free(offscreen_bitmap_header);
    offscreen_bitmap_header = NULL;
  }
  if (offscreen_bitmap_handle) {
    GetObject(offscreen_bitmap_handle, sizeof(BITMAP), (LPSTR)&offscreen_bitmap);
    if (!offscreen_bitmap.bmBits)
      Free(offscreen_bitmap.bmBits);
    DeleteObject(offscreen_bitmap_handle);
  }
  pix_mat.alloc = 0;
  pix_mat.ptr = 0;
  pix_mat.rows = pix_mat.cols = pix_mat.slop = 0;
}


R_CallMethodDef windowsgui_call_methods[]  = {
  // R hook functions
  MKREF(get_tcl_interp          , 1),
  MKREF(have_plot_window	, 0),
  MKREF(plot_compass		, 5),
  MKREF(plot_range_rings	, 4),
  MKREF(repaint_plot_window	, 1),
  MKREF(set_plot_window		, 1),
  MKREF(show_no_plot		, 0),
  MKREF(prepare_for_plot        , 0),
  {NULL, NULL, 0}
};

void
R_init_windowsgui(DllInfo *info)
{
  /* initialize the windows gui */
  
  R_registerRoutines(info, NULL, windowsgui_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  pextmat_to_sexp = (typeof(extmat_to_sexp)*) R_GetCCallable("extmat", "extmat_to_sexp");

  create_plot_bitmap_headers();
}

void
R_unload_windowsgui(DllInfo *info)
{
  /* Release resources. */

  destroy_plot_bitmap_headers();
}


