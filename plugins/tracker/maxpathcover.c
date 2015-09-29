/* 
   This code is modified from the Hungarian method in Donald Knuth's Stanford GraphBase:

      ftp://ftp.cs.stanford.edu/pub/sgb/gb_lisa.w

   THIS FILE IS NOT PART OF THE STANFORD GRAPHBASE PACKAGE!
   Any errors in this file are mine, not Knuth's.

   I believe the licence terms of Knuth's Stanford GraphBase permit this clearly identified and
   significantly modified derivative to be given the following licence:

      (C) 1993 by Stanford University 1993; 2007 by John Brzustowski  
      License: GPL

   If not, then this file is hereby placed in the public domain with only the condition
   that any derivative be clearly identified as NOT PART OF THE STANFORD GRAPHBASE.

   maxpathcover.c - given a directed acyclic graph with positive edge weights, find a maximum
   weight path cover (i.e. a set of node-disjoint directed paths whose sum of edge weights is
   a maximum).  When the DAG is bipartite, this reduces to a maximum weight matching.

   I've adapted it to find a maximum matching in an incomplete bipartite graph where each node
   has a list of the edges from it to other nodes.  Missing edges are implicitly treated
   as having zero weight.

   To have do_max_path_cover verify the correctness of its solutions, compile with -DRADR_DEBUG,
   which will check dual feasibility and complementary slackness.
   Dual feasibility is given by: 

      y_out >= 0
      y_in >= 0
      y_out[n_out] + y_in[n_in] >= wt[n_out, n_in]

   and complementary slackness requires that all matched edges (i.e. all edges in a path of the cover)
   satisfy the latter exactly; i.e.  y_out[n_out] + y_in[n_in] == wt[n_out, n_in]

   I've modified Knuth's terminology to make this algorithm more comprehensible to me.
   Each node in the graph is implicitly represented as both a "row" and "column" in Knuth's
   original terms.  These represent the two nodes of the split graph, the first being an "out-node"
   (i.e. edges go out from it), the second being an "in-node" (i.e. edges come into it).

*/


#include "radR.h"
#include "maxpathcover.h"

void
do_max_path_cover (t_DAG *dag, t_node_handle *pred, t_node_handle *succ, int *n_edges) {

  // find a maximum weight path cover in a DAG with positive edge weights
  //
  // dag                - a DAG
  // succ 		- the in-node matching each out-node; NO_NODE if unmatched; must be allocated to length n
  // pred 		- the out-node matching each in-node; NO_NODE if unmatched; must be allocated to length n
  // n_edges            - if not NULL, return location for the number of (positive-weight) edges in the cover
                                
  t_node_handle *tree_pred;	/* ancestor of a given in-node's mate, or NO_NODE */         
  t_node_handle *unchosen_node;	/* node in the forest */                                 
  t_node_handle  t;		/* total number of nodes in the forest */                
  t_node_handle  q;		/* total number of explored nodes in the forest */       
  t_weight    	*y_out;		/* the dual variable for out-nodes */
  t_weight    	*y_in;		/* the dual variable for in-nodes */
  t_weight    	*slack;		/* for each innode, minimum slack among edges entering it */
  t_node_handle *slack_pred;	/* for each innode, an out-node from which the edge has minimum slack */
  t_node_handle  unmatched;	/* this many outnodes have yet to be matched */              

  int 		 n     = dag->n;
  t_node 	*nodes = dag->nodes;
  t_node_handle  j, k, l;
  int 		 ei;
  t_weight 	 w, s;

  tree_pred 	= Calloc(n, t_node_handle);
  unchosen_node = Calloc(n, t_node_handle);
  y_out 	= Calloc(n, t_weight);
  y_in 		= Calloc(n, t_weight);
  slack 	= Calloc(n, t_weight);
  slack_pred 	= Calloc(n, t_node_handle);

  t = 0;
  for(l = 0; l < n; l++) {
    pred[l] 	 = NO_NODE;
    succ[l] 	 = NO_NODE;
    tree_pred[l] = NO_NODE;
    y_in[l] 	 = 0;
    slack[l] 	 = INF;
  }

  for(k = 0; k < n; k++) {
    // find the edge from this node with the largest weight
    s = 0;
    for(ei=0; ei < nodes[k].deg; ++ei) {
      w = nodes[k].e[ei].wt;
      if (w > s)
	s = w;
    }
    y_out[k] = s;
    // immediately match this node to a an out-mate by a tight edge, if one exists
    for(l = 0, ei=0; l < n; l++) {
      if (ei < nodes[k].deg && nodes[k].e[ei].nh == l)
	w = nodes[k].e[ei++].wt;
      else
	w = 0;
      if ((s == w) && (pred[l] == NO_NODE)) {
	succ[k] = l;
	pred[l] = k;
	goto node_done;
      }
    }
    // otherwise, mark this node as unmatched and add it to the unchosen node list
    succ[k] = NO_NODE;
    unchosen_node[t++] = k;
  
  node_done:
    ;
  }

  if (t==0)
    goto done;

  unmatched = t;
  while(1) {
    q = 0;
    while(1) {
      // process the unchosen out-nodes, extending the forest and looking for 
      // a breakthrough (i.e. augmenting path along tight edges)
      while(q < t) {
	k = unchosen_node[q];
	s = y_out[k];
	// for all mates of out-node k, see whether the slack for the edge from
	// node k has changed
	// actual slack may have changed since we last computed slack[l],
	// due to changes in y_in or y_out
	// if slack[l] == 0, this node has already been examined
	// as a possible successor and found to be in the forest
	for(l = 0, ei=0; l < n; l++) {
	  if (ei < nodes[k].deg && nodes[k].e[ei].nh == l)
	    w = nodes[k].e[ei++].wt;
	  else
	    w = 0;
	  if (slack[l]) {
	    t_weight del;
	    del = s + y_in[l] - w;
	    if (del < slack[l]) {
	      if (del == 0){
		if (pred[l] == NO_NODE) 
		  // this unmatched out-node has a tight edge to an unmatched in-node
		  // so augment the matching
		  goto breakthru;
		// indicate this in-node has a tight edge to an unchosen out-node
		// and record its parent
		slack[l] = 0;
		tree_pred[l] = k;
		// the in-node is matched, so its mate will be called an unchosen out-node
		unchosen_node[t++] = pred[l];
	      } else {
		// this non-tight edge is less slack than the current least slack
		// edge for this in-node
		slack[l] = del;
		slack_pred[l] = k;
	      }
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

      // add delta to dual vars for out-nodes
      for(q = 0; q < t; q++)
	y_out[unchosen_node[q]] -= s;
      // add delta to weights of chosen in-nodes
      for(l = 0; l < n; l++) {
	if (slack[l]) {
	  slack[l] -= s;
	  if (slack[l] == 0) {
	    k = slack_pred[l];
	    if (pred[l] == NO_NODE) {
	      // this is a newly tight edge to an unmatched out-node, and
	      // so is a breakthrough, but we must finish processing
	      // the remaining in-node dual vars
	      for(j = l + 1; j < n; j++)
		if (slack[j] == 0)
		  y_in[j] += s;
	      goto breakthru;
	    } else {
	      // add this in-node to the growing forest,
	      // and mark its mate as an unchosen out-node
	      tree_pred[l] = k;
	      unchosen_node[t++] = pred[l];
	    }
	  }
	} else {
	  y_in[l] += s;
	}
      }
    }
  breakthru:
    // augment the matching back along the path
    // to the initial unchosen out-node
    while(1) {
      j = succ[k];
      succ[k] = l;
      pred[l] = k;
      if (j == NO_NODE)
	break;
      k = tree_pred[j];
      l = j;
    }
    
    if (--unmatched == 0)
      goto done;

    // clear the "descended from unchosen out-nodes" forest 
    // and reset slackness
    for(l = 0; l < n; l++){
      tree_pred[l] = NO_NODE;
      slack[l] = INF;
    }
    // reinitialize the forest to the set of unmatched out-nodes
    t = 0;
    for(k = 0; k < n; k++)
      if (succ[k] == NO_NODE)
	unchosen_node[t++] = k;
  }
 done:
#ifdef RADR_DEBUG
  // check that weights are non-negative
  for (k=0; k < n; ++k) {
    for(ei=0; ei < nodes[k].deg; ++ei) {
      if (nodes[k].e[ei].wt < 0)
	printf("Error: negative edge weight: %3d->%3d: w=%5d\n", k, 1 + nodes[k].e[ei].nh, nodes[k].e[ei].wt);
    }
  }
#endif

#ifdef RADR_DEBUG
  // dump the graph as received
  for (k=0; k < n; ++k) {
    for(l = 0, ei=0; l < n; l++) {
      if (ei < nodes[k].deg && nodes[k].e[ei].nh == l)
	w = nodes[k].e[ei++].wt;
      else
	w = 0;
      if (w > 0)
	printf("%3d->%3d: w=%5d\n", k, l, w);
    }
  }

  // code to verify complementary slackness
  for (k=0; k < n; ++k) {
    if (y_out[k] < 0) 
      printf("Error: y_out[%d]<0\n", k);
    if (y_in[k] < 0) 
      printf("Error: y_in[%d]<0\n", k);
    if (succ[k] < 0)
      printf("Error: unmatched node %d\n", k);
    for(l = 0, ei=0; l < n; l++) {
      if (ei < nodes[k].deg && nodes[k].e[ei].nh == l)
	w = nodes[k].e[ei++].wt;
      else
	w = 0;
      if (y_out[k] + y_in[l] < w)
	printf("Error: negative slack for edge(%d,%d)\n", k, l);
      if (l == succ[k] && y_out[k] + y_in[l] != w)
	printf("Error: matched edge is not tight for node %d\n", k);
    }
  }
#endif

  // if a node is matched to another by a zero weight edge, whether
  // explicit or implicit, mark
  // its successor and its mate's predecessor as NO_NODE 
  // Count positive-weight edges by starting with n and decreasing
  // for each zero-weight edge.
  j = n;
  for (k=0; k < n; ++k) {
    if (succ[k] == NO_NODE) {
      nodes[k].mate = -1;
      continue;
    }
    for (ei=0; ei < nodes[k].deg; ++ei)
      if (nodes[k].e[ei].nh == succ[k]) {
        nodes[k].mate = ei;
	break;
      }
    if (ei == nodes[k].deg || nodes[k].e[ei].wt == 0) {
      // node is unmatched or matched by a zero-weight edge
      pred[succ[k]] = NO_NODE;
      succ[k] = NO_NODE;
      --j;
    }
  }
  if (n_edges != NULL)
    *n_edges = j;
  return;
}

SEXP
max_path_cover(SEXP wt, SEXP PRED, SEXP SUCC) {

  // NOTE: this function is for testing the do_max_path_cover function
  // by comparison with the bipartite_matching function.
  // Calling do_max_path_cover through this wrapper removes
  // the space-saving benefits of the sparse DAG representation used
  // therein (although the time-saving benefits will still appear).

  // For the matrix of edge weights passed in wt
  // find a maximum path cover, treating the rows as
  // "out-nodes", and the columns as "in-nodes", so that
  // each matrix entry is treated as the weight of the edge
  // from its row to its column.
  // Zero-weight edges are considered missing, and the DAG created
  // for do_max_path_cover omits them.
  //
  // If the graph represented by wt is not acyclic, then the 
  // result might not be a path cover (it might contain cycles).
  //
  // wt is a square integer matrix of edge weights
  // SUCC, PRED are integer vectors with length equal to dim(wt)[1]
  //
  // Returns the total weight of the path cover
  // side effects: SUCC will hold the index of the successor to each node (or NA)
  // PRED will hold the index of the predecessor to each node (or NA)

  
  SEXP dim;
  SEXP rv;
  int i, j;
  int n;  // number of nodes
  int m; // number of edges
  t_node_handle *pred, *succ;
  t_edge *k;
  t_weight w, total_weight;
  t_DAG dag;

  dim = GET_DIM(wt);
  n = INTEGER(dim)[0];
  if (n != INTEGER(dim)[1])
    error("max path cover: number of rows and columns must be equal");
  if (n != LENGTH(PRED) || n != LENGTH(SUCC))
    error("max path cover: length of predecessor and successor vectors must equal number of nodes");
  if (TYPEOF(PRED) != INTSXP || TYPEOF(SUCC) != INTSXP)
    error("max path cover: predecessor and successor vectors must be of class integer");
     
  PROTECT(wt = AS_INTEGER(wt));

  pred = INTEGER(PRED);
  succ = INTEGER(SUCC);

  // count number of (non-zero-weight) edges
  m = 0;
  for (i=0; i < LENGTH(wt); ++i)
    if (INTEGER(wt)[i] > 0)
      ++m;

  dag = create_dag(n, m);

  // add edges to each node's edge list
  for (i = 0, k = dag.edges; i < n; ++i) {
    dag.nodes[i].e = k;
    for (j = 0; j < n; ++j) {
      if ((w = INTEGER(wt)[i + j*n]) > 0) {
	++dag.nodes[i].deg;
	k->nh = j;
	k->wt = w;
	++k;
      }
    }
  }
  
  do_max_path_cover(&dag, pred, succ, NULL);

  free_dag(&dag);

  total_weight = 0;
  for (i=0; i < n; ++i) {
    if (succ[i] != NO_NODE) {
      total_weight += INTEGER(wt)[ i + succ[i] * n];
      ++succ[i]; // adjust to origin 1
    }
    if (pred[i] != NO_NODE)
      ++pred[i];
  }
  rv = allocVector(INTSXP, 1);
  INTEGER(rv)[0] = total_weight;
  UNPROTECT(1);
  return rv;
}
  
/*================================================================

maxpathcover.dll method registration, initialization, and destruction

==================================================================*/


R_CallMethodDef maxpathcover_call_methods[]  = {
  // R hook functions
  MKREF(max_path_cover		, 3),
  {NULL, NULL, 0}
};

void
R_init_maxpathcover(DllInfo *info)
{
  /* Register routines, allocate resources. */
  
  R_registerRoutines(info, NULL, maxpathcover_call_methods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);

}

void
R_unload_maxpathcover(DllInfo *info)
{
  /* Release resources. */
}
