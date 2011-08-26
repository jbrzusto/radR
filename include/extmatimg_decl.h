//  declarations of functions in extmatimg.c
extern void do_image_extmat_changed (t_extmat *m, int x, int y, int width, int height, int imgWidth, int imgHeight);
extern SEXP radR_attach_image_to_extmat (SEXP name, SEXP tclinterp, SEXP matsxp);
extern SEXP copy_image_channel_to_extmat (SEXP name, SEXP tclinterp, SEXP matsxp, SEXP chansxp);
extern SEXP radR_image_extmat_changed (SEXP matsxp);
extern SEXP radR_image_swap_RB (SEXP matsxp);

