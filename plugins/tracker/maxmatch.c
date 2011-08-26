/* 
   maxmatch.c - the Hungarian method for maximum matching in a complete bipartite graph

   slightly modified from Donald Knuth's Stanford GraphBase:
   ftp://ftp.cs.stanford.edu/pub/sgb/gb_lisa.w

   We do a maximum weight matching. Row and column label feasibility 
   is given by: 
   row_inc[r] + col_inc[c] >= wt[r, c]

   John Brzustowski, 2007.  Any errors in this file are mine, not Knuth's.

*/


#include "radR.h"

typedef short t_node_num; 				// the type used to store a node number
#define NO_NODE -1      				// a value that fits in t_node_num, used to represent "no node"
typedef int t_weight;   				// the type used for arc and node weights
#define INF (1UL << (8 * sizeof(t_weight) - 1)) - 1     // the maximum value possible for t_weight; change for t_weight=float or double

// access the matrix element at row k, column l (row by row storage to match R)
#define aa(k, l) *(mtx + k + m * l) 

void
do_matching (t_node_num m, t_node_num n, t_weight *mtx, t_node_num *col_mate, t_node_num *row_mate) {

  // find a maximum weight matching in a bipartite graph
  //
  // m 			- number of rows
  // n 			- number of columns  NOTE! m <= n is required for this algorithm to terminate
  // mtx 		- matrix of weights, organized row by row; each entry of mtx >= 0
  // col_mate 		- the column matching each row; NO_NODE if unmatched; must be allocated to length m
  // row_mate 		- the row matching each column; NO_NODE if unmatched; must be allocated to length n
  //
  // NOTE: to allow for incomplete bipartite graphs, edges can be "forbidden" by giving them
  // weight zero:
  // - a matching of the allowed subgraph can be extended to a matching of the
  //   complete graph by adding zero-weight edges, and this doesn't change the weight of the matching.
  // - a matching of the complete graph induces a matching of the same weight in the allowed
  //   subgraph, by removing edges of weight zero.
  // So a maximum weight matching in the complete graph induces a maximum weight matching in
  // the allowed subgraph.
                                
  t_node_num  *parent_row;	/* ancestor of a given column's mate, or NO_NODE */         
  t_node_num  *unchosen_row;	/* node in the forest */                                 
  t_node_num  t;		/* total number of nodes in the forest */                
  t_node_num  q;		/* total number of explored nodes in the forest */       
  t_weight    *row_inc;		/* the amount subtracted from a given row */ 
  t_weight    *col_inc;		/* the amount added to a given column */   
  t_weight    *slack;		/* minimum uncovered entry seen in a given column */     
  t_node_num  *slack_row;	/* where the slack in a given column can be found */   
  t_node_num  unmatched;	/* this many rows have yet to be matched */              

  parent_row 	= Calloc(n, t_node_num);
  unchosen_row 	= Calloc(m, t_node_num);
  row_inc 	= Calloc(m, t_weight);
  col_inc 	= Calloc(n, t_weight);
  slack 	= Calloc(n, t_weight);
  slack_row 	= Calloc(n, t_node_num);

  int j, k, l, s;

  t = 0;
  for(l = 0; l < n; l++){
    row_mate[l] 	= NO_NODE;
    parent_row[l] 	= NO_NODE;
    col_inc[l] 		= 0;
    slack[l] 		= INF;
  }

  for(k = 0; k < m; k++) {
    s = aa(k, 0);
    for(l = 1; l < n; l++)
      if (aa(k, l) > s)
	s = aa(k, l);
    row_inc[k] = s;
    // immediately match this row to a column by a tight edge, if one exists
    for(l = 0; l < n; l++)
      if ((s == aa(k, l)) && (row_mate[l] == NO_NODE)){
	col_mate[k] = l;
	row_mate[l] = k;
	goto row_done;
      }
    // otherwise, mark this row as unmatched and add it to the unchosen row list
    col_mate[k] = NO_NODE;
    unchosen_row[t++] = k;
  
  row_done:
    ;
  }

  if (t==0)
    goto done;

  unmatched = t;
  while(1) {
    q = 0;
    while(1) {
      // process the unchosen rows, extending the forest and looking for 
      // a breakthrough (i.e. augmenting path along tight edges)
      while(q < t) {
	k = unchosen_row[q];
	s = row_inc[k];
	// find the smallest positive slack for this column;
	// actual slack may have changed since we last computed slack[l],
	// due to changes in col_inc or row_inc
	// if slack[l] == 0, this column has already been examined
	// and found to be in the forest
	for(l = 0; l < n; l++)
	  if (slack[l]) {
	    t_weight del;
	    del = s + col_inc[l] - aa(k, l);
	    if (del < slack[l]) {
	      if (del == 0){
		if (row_mate[l] == NO_NODE) 
		  // this unmatched row has a tight edge to an unmatched column 
		  // so augment the matching
		  goto breakthru;
		// indicate this column has a tight edge to an unchosen row
		// and record its parent
		slack[l] = 0;
		parent_row[l] = k;
		// the column is matched, so its mate will be called an unchosen row
		unchosen_row[t++] = row_mate[l];
	      } else {
		// this non-tight edge is less slack than the current least slack
		// edge for this column
		slack[l] = del;
		slack_row[l] = k;
	      }
	    }
	  }
	q++;
      }

      s = INF;
      // find delta, the smallest positive slack
      for(l = 0; l < n; l++)
	if (slack[l] && slack[l] < s)
	  s = slack[l];

      // add delta to (negative) weights of unchosen rows
      for(q = 0; q < t; q++)
	row_inc[unchosen_row[q]] -= s;
      // add delta to weights of chosen columns
      for(l = 0; l < n; l++) {
	if (slack[l]) {
	  slack[l] -= s;
	  if (slack[l] == 0) {
	    k = slack_row[l];
	    if (row_mate[l] == NO_NODE) {
	      // this is a newly tight edge to an unmatched row, and
	      // so is a breakthrough, but we must finish processing
	      // the remaining column weights
	      for(j = l + 1; j < n; j++)
		if (slack[j] == 0)
		  col_inc[j] += s;
	      goto breakthru;
	    } else {
	      // add this column to the growing forest,
	      // and mark its mate as an unchosen row
	      parent_row[l] = k;
	      unchosen_row[t++] = row_mate[l];
	    }
	  }
	} else {
	  col_inc[l] += s;
	}
      }
    }
  breakthru:
    // augment the matching back along the path
    // to the initial unchosen row
    while(1) {
      j = col_mate[k];
      col_mate[k] = l;
      row_mate[l] = k;
      if (j == NO_NODE)
	break;
      k = parent_row[j];
      l = j;
    }
    
    if (--unmatched == 0)
      goto done;

    // clear the "descended from unchosen rows" forest 
    // and reset slackness
    for(l = 0; l < n; l++){
      parent_row[l] = NO_NODE;
      slack[l] = INF;
    }
    // reinitialize the forest to the set of unmatched rows
    t = 0;
    for(k = 0; k < m; k++)
      if (col_mate[k] == NO_NODE) {
	unchosen_row[t++] = k;
      }
  }
 done:
#ifdef RADR_DEBUG
#if 0
  // spit out the graph
  for (k=0; k < m; ++k)
    for (l=0; l < n; ++l)
      if (aa(k, l) > 0)
	printf("%3d->%3d: w=%5d\n", k+1, l+1, aa(k, l));

#endif

  // code to verify the complementary slackness conditions

  for (k=0; k < m; ++k) {
    if (row_inc[k] < 0) 
      printf("Error: row_inc[%d]<0\n", k);
    for (l=0; l < n; ++l) {
      if (col_inc[l] < 0) 
	printf("Error: col_inc[%d]<0\n", l);
      if (row_inc[k]+col_inc[l] < aa(k, l))
	printf("Error: negative slack for edge(%d,%d)\n", k, l);
    }
    if (col_mate[k] < 0 || row_inc[k] + col_inc[col_mate[k]] != aa(k, col_mate[k]))
      printf("Error: unmatched row or matched edge is not tight for row %d\n", k);
  }
#endif
  return;
}

SEXP
bipartite_matching(SEXP wt) {

  // for the matrix of weights passed in wt
  // find a maximum weight matching between rows and columns
  // NOTE: we must have nrow(wt) <= ncol(wt)
  //
  // returns a vector of the column matched to each row
  
  SEXP dim;
  SEXP rv;
  int i, nr, nc;
  t_node_num *rm, *cm;
  dim = GET_DIM(wt);
  nr = INTEGER(dim)[0];
  nc = INTEGER(dim)[1];
  if (nr > nc)
    error("bipartite_matching: number of rows must not exceed number of columns");
  PROTECT(wt = AS_INTEGER(wt));
  cm = Calloc(nr, t_node_num);
  rm = Calloc(nc, t_node_num);
 
  do_matching(nr, nc, INTEGER(wt), cm, rm);
  rv = allocVector(INTSXP, nr);

  // provide mate indexes; a zero weight edge means no mate

  for (i = 0; i < nr; ++i)
    if (INTEGER(wt)[i + nr * cm[i]] != 0)
      INTEGER(rv)[i] = cm[i] + 1;
    else
      INTEGER(rv)[i] = NA_INTEGER;
  
  Free(cm);
  Free(rm);
  UNPROTECT(1);
  return rv;
}

  
/*================================================================

maxmatch.dll method registration, initialization, and destruction

==================================================================*/


R_CallMethodDef maxmatch_call_methods[]  = {
  // R hook functions
  MKREF(bipartite_matching		, 1),
  {NULL, NULL, 0}
};

void
R_init_maxmatch(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, maxmatch_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);

}

void
R_unload_maxmatch(DllInfo *info)
{
  /* Release resources. */
}
