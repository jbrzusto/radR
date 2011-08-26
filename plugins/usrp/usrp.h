/*  svn $Id$ 

    This file is part of radR.
    Copyright (C) 2011 John Brzustowski

*/

#include "radRmodule.h"
#include "usrp_radR_plugin.h"

/*! \brief
 * a structure to hold data opaque to the gnuradio usrp code but needed by this client
 * in callbacks 
*/

typedef
struct _URP_client_data {
  URP	 urp;			/**< usrp radR plugin object */
  SEXP	 scan_info_vector;	/**< pre-allocated list with metadata items to be filled in */
  SEXP   *rv;                	/**< slot into which scan_info_vector address is copied when scan is obtained */ 
  int    have_scan_data;        /**< has get_scan_info() been called without a subsequent completed call to get_scan_data() ? */
  double *timestamp_rv;         /**< location to store timestamp, once known */
  double *duration_rv;		/**< location to store duration, once known */
  double *range_cell_size_rv;	/**< location to store range cell size, once known */
  double *radar_PRF_rv;		/**< location to store radar PRF, once known */
} URP_client_data;











