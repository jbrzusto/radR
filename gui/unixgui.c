#include "unixgui.h"
/*

  unix (i.e. X11) specific gui functions for radR

*/

// X11 GUI variables

static Display *plot_display;		// the display for the window
static Window plot_win;		        // of the radar plotting window
static GC plot_gc;                      // plot window graphics context
static XGCValues gc_opts;               // options for plot graphics context
static unsigned long gc_opts_mask;      // mask for gc options
static XWindowAttributes plot_win_attr; // window attributes
static Pixmap plot_pixmap=0;            // the pixmap structure describing the plot data
static XShmSegmentInfo plot_shminfo;    // the X shared memory segment info
static int plot_depth = 24;             // depth (in bits) of the pixmap
static int plot_width;                  // plot width in pixels
static int plot_height;                 // plot height in pixels
static Tcl_Interp *my_tcl;

// the extmat into which radR scan-converts. We manage its
// memory here in a segment shared with the X server.
static t_extmat pix_mat = CREATE_FOREIGN_EXTMAT(EXTMAT_TYPE_UINT, "radR matrix of 32-bit raw RGB values for the plot window");

// workaround R's library having redefined round
#define round(x) (int)(x + 0.5)

SEXP
get_tcl_interp(SEXP win) {
  // when passed the X window ID of a tk window, finds
  // the tcl interpreter associated with it
  // as an EXTPTRSEXP; It will be NULL if no interpreter was found.
  Tk_Window tw;
  TkDisplay *dispPtr;
  Tcl_HashEntry *hPtr;

  dispPtr = TkGetDisplayList();
  if (dispPtr == NULL)
    return R_NilValue;

  hPtr = Tcl_FindHashEntry(&dispPtr->winTable, (char *) (long long) INTEGER(AS_INTEGER(win))[0]);
  if (hPtr == NULL)
    return R_NilValue;
  tw = (Tk_Window) Tcl_GetHashValue(hPtr);
  return PTR_TO_EXTPTR((my_tcl=((TkWindow *) tw)->mainPtr->interp));
}

void
delete_plot_pixmap() {
  if (!plot_pixmap)
    return;
  XShmDetach (plot_display, &plot_shminfo);
  XFreePixmap (plot_display, plot_pixmap);
  shmdt (plot_shminfo.shmaddr);
  shmctl (plot_shminfo.shmid, IPC_RMID, 0);
  plot_pixmap = 0;
  pix_mat.alloc = 0;
  pix_mat.ptr = 0;
  pix_mat.rows = pix_mat.cols = pix_mat.slop = 0;
}

void
ensure_plot_pixmap(short width, short height) {
  // make sure there is a shared memory XImage of the
  // correct size to hold the plot
  // this is boilerplate from various X documentation
  // FIXME: check return values of calls for errors; don't reallocate shared
  // memory if the required amount decreases!

  if (!plot_pixmap || width != pix_mat.cols || height != pix_mat.rows) {
    if (plot_pixmap)
      delete_plot_pixmap();
    plot_shminfo.shmid = shmget (IPC_PRIVATE, width * height * sizeof(t_pixel), IPC_CREAT|0777);
    plot_shminfo.shmaddr = shmat (plot_shminfo.shmid, 0, 0);
    plot_shminfo.readOnly = False;
    XShmAttach (plot_display, &plot_shminfo);
    plot_pixmap = XShmCreatePixmap (plot_display, plot_win, plot_shminfo.shmaddr, & plot_shminfo, width, height, plot_depth);
    pix_mat.rows = plot_height = height;
    pix_mat.cols = plot_width = width;
    pix_mat.slop = 0;
    pix_mat.alloc = width * height * sizeof(t_pixel);
    pix_mat.ptr = plot_shminfo.shmaddr;
    pix_mat.changed_fun = pix_mat.changed_fun_extra = NULL;
  }
  // create the offscreen gc

  gc_opts_mask = GCForeground | GCBackground | GCLineWidth | GCGraphicsExposures;
  gc_opts.foreground = 0xFFFFFFFF;
  gc_opts.background = 0x00000000;
  gc_opts.line_width = 0;
  gc_opts.graphics_exposures = 1;
  plot_gc = XCreateGC(plot_display, plot_win, gc_opts_mask, & gc_opts);
}

int
get_window_info() {

  if (plot_win) {
    XGetWindowAttributes(plot_display, plot_win, &plot_win_attr);
    plot_width = plot_win_attr.width;
    plot_height = plot_win_attr.height;
    return TRUE;
  }
  return FALSE;
}

SEXP
have_plot_window(void) {

  return LOGICAL_SEXP(plot_win != 0);
}

SEXP
set_plot_window(SEXP s) {
  if (!plot_display)
    plot_display = XOpenDisplay(NULL);
  plot_win = (Window) INTEGER(AS_INTEGER(s))[0];
  return PASS_SEXP;
}

SEXP
prepare_for_plot() {
  if(! get_window_info())
    return FAIL_SEXP;
  ensure_plot_pixmap(plot_width, plot_height);
  return (*pextmat_to_sexp)(&pix_mat);
}

SEXP
repaint_plot_window(SEXP coords) {

  // copy the shared Pixmap to the window
  // or some subrectangle thereof
  // if coords is NULL, the entire
  // window is repainted, otherwise
  // coords should be left, top, width, height

  short left, top, width, height;

  if (plot_win_attr.map_state != IsViewable)
    return PASS_SEXP;

  if(coords && 0 != GET_LENGTH(coords)) {
    // a subrectangle of the window
    PROTECT(coords = AS_INTEGER(coords));
    left = (short) INTEGER(coords)[0];
    top = (short) INTEGER(coords)[1];
    width = (short) INTEGER(coords)[2];
    height = (short) INTEGER(coords)[3];
    UNPROTECT(1);
  } else {
    // the whole window
    left = 0;
    top = 0;
    width = plot_width;
    height = plot_height;
  }
  XCopyArea(plot_display, plot_pixmap, plot_win, plot_gc,
	    left, top,
	    width, height,
	    left, top);
  XFlush(plot_display);
  return PASS_SEXP;
}

SEXP
show_no_plot(void)
{

  if(!get_window_info())
    return FAIL_SEXP;
  if (plot_win_attr.map_state != IsViewable)
    return PASS_SEXP;
  ensure_plot_pixmap(plot_width, plot_height);
  XSetForeground(plot_display, plot_gc, 0);
  XFillRectangle(plot_display, plot_pixmap, plot_gc, 0, 0, plot_width, plot_height);
  repaint_plot_window(NULL);
  return PASS_SEXP;
}

SEXP
plot_range_rings(SEXP n, SEXP centre, SEXP spacing, SEXP rgb) {

  int i;
  int delta;
  int red, green, blue;
  int max_rings;

  PROTECT(spacing = AS_NUMERIC(spacing));
  PROTECT(centre = AS_INTEGER(centre));
  PROTECT(n = AS_INTEGER(n));
  PROTECT(rgb = AS_INTEGER(rgb));
  red = INTEGER(rgb)[0];
  green = INTEGER(rgb)[1];
  blue = INTEGER(rgb)[2];

  XSetForeground(plot_display, plot_gc, (red << 16) | (green << 8) | blue);
  XSetLineAttributes(plot_display, plot_gc, 0, LineOnOffDash, CapButt, JoinMiter);
  max_rings = min(50, INTEGER(n)[0]);

  for (i=1; i <= max_rings; ++i) {
    delta = (int) (i * REAL(spacing)[0]);
    XDrawArc(plot_display, plot_pixmap, plot_gc, INTEGER(centre)[0] - delta, (INTEGER(centre)[1] - delta), 2 * delta, 2 * delta, 0, 360 * 64);
  }
  UNPROTECT(4);
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
  int tmp;
  int anglab;
  int anglabs[] = {1, 2, 5, 10, 20, 30, 45, 90};
  int i;

  XCharStruct labdims;

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

  XSetForeground(plot_display, plot_gc, (red << 16) | (green << 8) | blue);
  XSetLineAttributes(plot_display, plot_gc, 0, LineSolid, CapButt, JoinMiter);

  for (i = 0; i < 3; ++i) {
    XQueryTextExtents(plot_display, XGContextFromGC(plot_gc), dummylabs[i], i+1, &tmp, &tmp, &tmp, &labdims);
    labwidth[i] = labdims.width;
    labheight[i] = labdims.ascent;
  }

  // set the labelling angle to the smallest one that still provides
  // comfortable spacing around the labels

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
    XDrawLine(plot_display, plot_pixmap, plot_gc, x, y, ex, ey);
    if (ang % anglab == 0) {
      nchar = sprintf(lab_buf, "%d", ang);
      ex += -labwidth [nchar - 1] / 2 * (1 - cos(scrang)) + 5 * cos(scrang);
      ey +=  labheight[nchar - 1] / 2 * (1 - sin(scrang)) - 5 * sin(scrang);
      XDrawString(plot_display, plot_pixmap, plot_gc, ex, ey, lab_buf, nchar);
    }
  }

  UNPROTECT(5);
  return PASS_SEXP;
}

#define X_ERR_BUFF_LEN 1024
char x_error_buff[X_ERR_BUFF_LEN];
int (*old_x_err_handler)(Display *disp, XErrorEvent *evt);
int (*old_x_io_err_handler)(Display *disp);

int
x_error_handler(Display *disp, XErrorEvent *evt) {
  // handle an X11 error by returning an R error with the X11 error text
  // We enable this only on debug builds because there appears to be a
  // tcltk package or Tk library bug that triggers a "Bad Window" error
  // which can cause an infinite error handling loop when readline is used.
  // Weird.
#ifdef RADR_DEBUG
  strcpy(x_error_buff, "unixgui got X11 error: ");
  XGetErrorText(disp, evt->error_code, x_error_buff + strlen(x_error_buff), X_ERR_BUFF_LEN - strlen(x_error_buff) - 1);
  // JMB 2023  error(x_error_buff);
#endif
  return(0);
}

int
x_io_error_handler(Display *disp) {
  // handle an X11 IO error by returning an R error with the X11 error text
  error("unixgui got X11 I/O error");
  return(0);
}


R_CallMethodDef unixgui_call_methods[]  = {
  // R hook functions
  MKREF(get_tcl_interp          , 1),
  MKREF(have_plot_window	, 0),
  MKREF(plot_compass		, 5),
  MKREF(plot_range_rings	, 4),
  MKREF(prepare_for_plot	, 0),
  MKREF(repaint_plot_window	, 1),
  MKREF(set_plot_window		, 1),
  MKREF(show_no_plot		, 0),
  {NULL, NULL, 0}
};

void
R_init_unixgui(DllInfo *info)
{
  /* initialize the windows gui */
  R_registerRoutines(info, NULL, unixgui_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  old_x_err_handler = XSetErrorHandler(&x_error_handler);
  old_x_io_err_handler = XSetIOErrorHandler(&x_io_error_handler);
  pextmat_to_sexp = (typeof(extmat_to_sexp)*) R_GetCCallable("extmat", "extmat_to_sexp");
}

void
R_unload_unixgui(DllInfo *info)
{
  /* Release resources. */
  delete_plot_pixmap();
  XSetErrorHandler(old_x_err_handler);
  XSetIOErrorHandler(old_x_io_err_handler);
  XCloseDisplay (plot_display);
}
