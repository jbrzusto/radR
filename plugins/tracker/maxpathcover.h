/* 
   maxpathcover.h - definitions for the maxpathcover function

   John Brzustowski, 2007.

*/

typedef int 	t_weight; 	/* type used for edge weights and node weights */
typedef int 	t_node_handle;	/* type used to refer to a node  */
#define INF 	(1UL << (8 * sizeof(t_weight) - 1)) - 1	/* the maximum value possible for t_weight; change for t_weight = float or double */
#define NO_NODE NA_INTEGER     	/* a value that fits in t_node_handle, used to represent "no node" */

  /* graph structure: edge */
typedef struct {
  t_node_handle nh;  // handle of the node at the head of this edge
  t_weight 	wt;  // weight of this edge
} t_edge;

  /* graph structure: node */
typedef struct {
  int 		 deg; 		/* number of out-edges */
  t_edge 	*e; 		/* pointer to first outedge in array edges */
  int            mate;          /* output: if node is in cover, index in e[] of the corresponding edge */
} t_node;


/* structure for Directed Acyclic Graph */

typedef struct {
  int 		 n; 		/* number of nodes in graph */
  int 		 m; 		/* number of edges in graph */
  t_node 	*nodes; 	/* nodes in graph; size is n */
  t_edge 	*edges; 	/* edges in graph; size is m */
} t_DAG;

#define create_dag(N, M) ({int _N_ = N, _M_ = M; t_DAG _D_; _D_.n = _N_; _D_.m = _M_; _D_.nodes = Calloc(_N_, t_node); _D_.edges = Calloc(_M_, t_edge); _D_;})
#define free_dag(D) {t_DAG *_D_ = D; if (_D_) {if (_D_->nodes) Free(_D_->nodes); if (_D_->edges) Free(_D_->edges); _D_->nodes = NULL; _D_->edges = NULL; _D_->n = _D_->m = 0;}}
#define empty_dag() ({t_DAG _D_; _D_.n = 0; _D_.m = 0; _D_.nodes = NULL; _D_.edges = NULL; _D_;})
void do_max_path_cover (t_DAG *dag, t_node_handle *pred, t_node_handle *succ, int *n_edges);
