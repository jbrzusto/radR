#ifndef _RADRGUI_H
#define _RADRGUI_H

/* definitions for the C portion of the device-independent gui */


extern SEXP gui_set_plot_pps(double *pps);
extern SEXP gui_plot_window_geometry(SEXP rps);
extern SEXP gui_set_plot_origin(SEXP coords, SEXP absolute);
extern SEXP gui_set_plot_window(SEXP hwnd);
extern SEXP gui_repaint_plot_window(SEXP coords);
extern SEXP gui_update_plot_parms (void);
extern SEXP gui_update_plot_window(SEXP reconv);
extern SEXP gui_set_sample_palette(SEXP class, SEXP colours);







#endif /* _RADRGUI_H */
