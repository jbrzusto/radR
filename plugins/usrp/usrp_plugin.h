/*  svn $Id$ 

    This file is part of radR.
    Copyright (C) 2011 John Brzustowski

*/

extern "C" {
#include "radRmodule.h"
}

class usrp_plugin {

private:
  /* 
  int			which		   ;
  unsigned int		decim		   ;
  float			vid_gain	   ;
  float			trig_gain	   ;
  float			trig_thresh_excite ;
  float			trig_thresh_relax  ;
  unsigned short	trig_latency	   ;
  unsigned short	trig_delay	   ;
  float                 ARP_gain	   ;
  float			ARP_thresh_excite  ;
  float			ARP_thresh_relax   ;
  unsigned short	ARP_latency	   ;
  float                 ACP_gain	   ;
  float			ACP_thresh_excite  ;
  float			ACP_thresh_relax   ;
  unsigned short	ACP_latency	   ;
  unsigned short	n_samples	   ;
  int                   n_pulses	   ;
  bool                  counting	   ;
  bool                  vid_negate	   ;
  bool                  all_pulses         ;
  bool                  own_thread         ;
  unsigned int          bbprx_mode         ;
  int			fusb_block_size	   ;
  int			fusb_nblocks	   ;



#endif
