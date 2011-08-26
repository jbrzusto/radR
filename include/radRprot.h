/* extmatimg.c */
SEXP radR_attach_image_to_extmat(SEXP name, SEXP tclinterp, SEXP matsxp);
void do_image_extmat_changed(t_extmat *m, int x, int y, int width, int height, int imgWidth, int imgHeight);
SEXP radR_image_extmat_changed(SEXP matsxp);
SEXP radR_image_swap_RB(SEXP matsxp);
/* patchify.c */
int ensure_image(t_image image);
void free_image(t_image image);
int save_row(t_image image, t_dim row, t_class *raw, t_class background_val, unsigned skip);
t_dim compress_row(t_image image, t_class *row, t_dim row_num, t_class foreground_val, unsigned skip);
t_cell_run *get_patch_from_rc(t_image image, t_dim row, t_dim col);
void patchify_image(t_image image, int use_diags, int vertical_wrap);
void link_patches(t_image image);
int patchify_buffer(t_class *buf, t_image image, unsigned skip);
void shut_down_patchify(t_image image);
void enumerate_patches(t_image image, patch_function f);
t_cell_run *enumerate_all_patches(t_image image, patch_function f, int *npat);
t_pf_rv pf_reactivate_all(t_cell_run *run);
void reactivate_all_patches(t_image image);
SEXP get_active_runbuf(SEXP patchessxp);
SEXP set_runbuf(SEXP patchessxp, SEXP runsxp);
SEXP get_indexes_from_runbuf(SEXP runsxp);
SEXP get_runbuf_info(SEXP runsxp);
SEXP index_extmat_by_runbuf(SEXP matsxp, SEXP runsxp);
SEXP assign_extmat_by_runbuf(SEXP matsxp, SEXP runsxp, SEXP valsxp);
/* pulses.c */
void get_pulse_metadata(unsigned np, double t0, double dur, double orientation, double ang_offset, const double *rot_ax, unsigned na, const double *tilt, unsigned nt, double *ts, double *azi, double *elev, double *wg_azi, double *wg_elev, double NA);
/* radR.c */
void rpv(SEXP s);
SEXP int_wrapped_pointer_to_sexp(SEXP s);
SEXP radR_get_and_set(SEXP symbol, SEXP value, SEXP rho);
SEXP radR_critical_eval(SEXP expr, SEXP rho);
SEXP call_R_function(char *name, SEXP env, ...);
SEXP make_R_vector(int sxp_type, int n, ...);
int is_R_hook_active(int hook);
SEXP radR_update_stats(SEXP scansxp, SEXP classsxp, SEXP celldims, SEXP meansxp, SEXP devsxp, SEXP ksxp, SEXP modesxp, SEXP numlearningscanssxp);
SEXP radR_classify_samples(SEXP scoresxp, SEXP classsxp, SEXP prevclasssxp, SEXP threshsxp);
SEXP radR_calculate_scores(SEXP scansxp, SEXP meansxp, SEXP devsxp, SEXP scoresxp);
t_pf_rv pf_filter_by_stats(t_cell_run *r);
t_pf_rv pf_filter_by_logical_vector(t_cell_run *run);
t_pf_rv pf_patch_and_blip_hooks(t_cell_run *r);
t_pf_rv pf_paint_blip(t_cell_run *r);
SEXP radR_find_patches(SEXP classsxp, SEXP usediagsxp, SEXP patchessxp, SEXP skipsxp);
SEXP radR_process_patches(SEXP filtersxp, SEXP scansxp, SEXP scoresxp, SEXP classsxp, SEXP patchbuff, SEXP statssxp, SEXP sampnumrange, SEXP arearange, SEXP angularrange, SEXP radialrange, SEXP rangeelevpersample, SEXP areaweighting, SEXP scaninfo, SEXP pulses);
SEXP radR_unfilter_patches(SEXP classsxp, SEXP patchbuff);
SEXP radR_get_patch_image(void);
SEXP radR_patch_at_sp(SEXP sp, SEXP patchbuff);
SEXP radR_get_all_blips(SEXP patchbuff, SEXP blipnumsxp, SEXP linearsxp);
void radR_fix_readline_problem(void);
SEXP radR_install_handler(SEXP handler_function_name);
SEXP radR_remove_handler(void);
SEXP radR_enable_handler(SEXP enab);
SEXP radR_process_UI_events(void);
SEXP radR_sleep(SEXP ms);
SEXP radR_get_error(void);
SEXP radR_get_error_msg(void);
SEXP first_empty_slot(SEXP list);
SEXP into_first_empty_slot(SEXP list, SEXP item);
SEXP num_full_slots(SEXP list);
SEXP which_slots_full(SEXP list);
SEXP get_addr(SEXP s);
SEXP radR_estimate_from_approx(SEXP approx, SEXP span, SEXP time_win);
SEXP radR_get_pulse_metadata(SEXP np, SEXP ts, SEXP dur, SEXP orient, SEXP ang_offset, SEXP rot_ax, SEXP tilt, SEXP rv, SEXP naval);
void init_vars(void);
void R_init_radR(DllInfo *info);
void R_unload_radR(DllInfo *info);
/* radRvars.c */
/* random.c */
SEXP random_bytes(SEXP num);
/* scancvt.c */
SEXP radR_convert_scan(SEXP sampsxp, SEXP pixsxp, SEXP classsxp, SEXP palettesxp, SEXP centresxp, SEXP scalesxp, SEXP rotsxp, SEXP bitshiftsxp, SEXP visiblesxp, SEXP cvtsxp, SEXP geomsxp, SEXP firstrangesxp, SEXP isrectangularsxp);
/* statefun.c */
void estimate_from_approx(const double *ap, unsigned size, unsigned span, double t_start, double t_step, unsigned n_t, double NA, double *est);
