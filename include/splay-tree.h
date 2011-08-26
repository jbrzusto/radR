/* 
   An implementation of splay trees in the spirit of BSD's tree.h, but
   using parent pointers.  TODO: figure out BSD's parent-pointerless
   non-recursive top-down splaying and redo this code similarly.

   Copyright 2007 by John Brzustowski.  Licence GPL.

*/

#ifndef _SPLAY_TREE_H
#define _SPLAY_TREE_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

  /* create typedefs for splay trees in the namespace given by NSPC, whose key values
     will be of type KEYTYPE, and whose node values will be of type NODETYPE, and
     whose root will be added to the item ROOTTYPE
     NODETYPE must be a forward-declared structure, and the SPLAY_ITEM macro below
     must be included in its definition to create the fields needed for splaying.
  */
  
  /* create the components to be added to the structure of type NODETYPE
     which are needed for the splay tree; a line of the form
     STRUCT_SPLAY_NODE_ITEM(struct my_node)
     must be included in the definition of struct my_node,
     which must have a forward declaration
     
     e.g.

     struct my_node;

     typedef struct my_node {
     int key;
     ...
     STRUCT_SPLAY_NODE_ITEM(mine, struct my_node);
     ...
     } t_my_node;

     typedef struct my_tree {
     ...
     STRUCT_SPLAY_ROOT_ITEM(mine, struct my_node);
     ...
     } t_my_tree;

     t_my_tree tree;

     DECLARE_SPLAY_FUNCTIONS(mine, t_my_node, int, key);
     DEFINE_SPLAY_FUNCTIONS(mine,  t_my_node, int, key);

     // Then splay functions are called as 

     t_my_node *n, node;

     SPLAY_INIT(mine, &tree);
     n = SPLAY_INSERT(mine, &tree, &node);
     n = SPLAY_FIND(mine, &tree, key);
     n = SPLAY_REMOVE(mine, &tree, key);

     "mine" is the literal namespace prefix used in function definitions.

     You must define a key comparison function with this signature:

        int mine_splay_compare_keys(KEYTYPE key1, KEYTYPE key2);

     and it must return an integer value <0, 0, >0 according to whether key1
     is less than, equal to, or greater than key2, respectively.

  */

#define STRUCT_SPLAY_NODE_ITEM(NSPC, NODETYPE) \
  /* Pointers to parent and the left and right children.  */ \
  NODETYPE *NSPC##_parent; \
  NODETYPE *NSPC##_child[2]; 

#define STRUCT_SPLAY_ROOT_ITEM(NSPC, NODETYPE) \
  /* Pointers to the root node of the splay tree.  */ \
  NODETYPE *NSPC##_root; 

typedef enum {SPLAY_OP_INSERT, SPLAY_OP_FIND, SPLAY_OP_REMOVE} t_splay_op;
#define SPLAY_ROOT(NSPC, tree)         (tree)->NSPC##_root
#define SPLAY_INIT(NSPC, tree)         SPLAY_ROOT(NSPC, tree) = NULL
#define SPLAY_INSERT(NSPC, tree, node) NSPC##_splay_util(&SPLAY_ROOT(NSPC, tree), SPLAY_OP_INSERT, (void *)(node))
#define SPLAY_FIND(NSPC, tree, key)    NSPC##_splay_util(&SPLAY_ROOT(NSPC, tree), SPLAY_OP_FIND, (void *)(key))
#define SPLAY_REMOVE(NSPC, tree, key)  NSPC##_splay_util(&SPLAY_ROOT(NSPC, tree), SPLAY_OP_REMOVE, (void *)(key))

#define DECLARE_SPLAY_FUNCTIONS(NSPC, NODETYPE, KEYTYPE, KEYFIELD)                      \
typedef NODETYPE **NSPC##_splay_tree;                                                   \
extern NODETYPE* NSPC##_splay_util(NSPC##_splay_tree sp, t_splay_op op, void *param); 

#define DEFINE_SPLAY_FUNCTIONS(NSPC, NODETYPE, KEYTYPE, KEYFIELD)                       \
											\
/* a utility function to find, insert, or remove a node in a splay tree */    		\
                                                                                        \
extern NODETYPE*									\
NSPC##_splay_util (NSPC##_splay_tree tree, t_splay_op op, void *param)	                \
{											\
  NODETYPE *newnode = NULL;								\
  KEYTYPE key = (KEYTYPE) 0;								\
  											\
  NODETYPE *next, *n, *parent, *gparent, *ggparent;					\
  											\
  int comp;										\
  /* for nodes n, parent, and gparent record which child of its parent it		\
     is (CI = "Child Index");  */							\
  int n_CI, parent_CI, gparent_CI=0 /*-Wall*/;						\
											\
  n = *tree;										\
											\
  switch(op) {										\
  case SPLAY_OP_INSERT:									\
    newnode = (NODETYPE *) param;							\
    key = (KEYTYPE) newnode->KEYFIELD;							\
    if (!n) {										\
      *tree = n = newnode;								\
      n->NSPC##_child[0] = n->NSPC##_child[1] = n->NSPC##_parent = NULL;		\
      return n;										\
    }											\
    break;										\
  case SPLAY_OP_FIND:									\
  case SPLAY_OP_REMOVE:									\
    if (!n) 										\
      /* tree is empty */								\
      return NULL;									\
    key = (KEYTYPE) param;								\
    break;										\
  default:										\
    /* this is an error; we fail by returning NULL */					\
    return NULL;									\
  }											\
											\
  for (;;) {										\
    /* find the node with the given key, or the node which would be			\
       the parent of a node with the given key */					\
    comp = NSPC##_splay_compare_keys (key, n->KEYFIELD);				\
    if (comp == 0)									\
      break;										\
    next = n->NSPC##_child[comp > 0];							\
    if (!next)										\
      break;										\
    n = next;										\
  }											\
											\
  if (op == SPLAY_OP_INSERT) {								\
    if (comp == 0) {									\
      /* a node with this key already existed, replace it in the tree			\
	 with the new node */								\
      NODETYPE *tmp = n;								\
      int i;                                                                            \
      newnode->NSPC##_parent = NULL;							\
      for (i = 0; i < 2; ++i) {								\
	if ((newnode->NSPC##_child[i] = n->NSPC##_child[i]))				\
	  newnode->NSPC##_child[i]->NSPC##_parent = newnode;				\
	n->NSPC##_child[i] = NULL;							\
      }											\
      n = newnode;									\
      newnode = tmp; /* record the old node, to be returned after splaying */		\
    } else {										\
      newnode->NSPC##_parent = n;							\
      newnode->NSPC##_child[0] = newnode->NSPC##_child[1] = NULL;			\
      n->NSPC##_child[comp > 0] = newnode;						\
      n = newnode;									\
    }											\
  }											\
    											\
  /* n now points to a node which should be splayed to the root */ 			\
											\
  /* determine whether n is the right child of its parent */				\
  if ((parent = n->NSPC##_parent)) 							\
    n_CI = n == parent->NSPC##_child[1];						\
											\
  for (;;) {										\
    if (!parent) {									\
      /* set the tree to point to n, which is the root */				\
      *tree = n;									\
      break;										\
    }	         									\
											\
    /* any other condition for halting the rotation of n up to the			\
       root should be inserted here as if( ... ) break; */				\
											\
    /* if there is no grandparent, we do a ZIG and are finished */			\
    if (!(gparent = parent->NSPC##_parent)) {						\
      if ((parent->NSPC##_child[n_CI] = n->NSPC##_child[1 - n_CI]))			\
	n->NSPC##_child[1 - n_CI]->NSPC##_parent = parent;				\
											\
      n->NSPC##_child[1 - n_CI] = parent;						\
      parent->NSPC##_parent = n;							\
      break;										\
    }											\
											\
    parent_CI = parent == gparent->NSPC##_child[1];					\
											\
    /* get the greatgrandparent, if any */						\
    if ((ggparent = gparent->NSPC##_parent))						\
      gparent_CI = gparent == ggparent->NSPC##_child[1];				\
											\
    /* do either a ZIGZAG or a ZIGZIG */						\
    											\
    if (n_CI != parent_CI) {								\
      /* ZIGZIG */									\
      if ((parent->NSPC##_child[n_CI] = n->NSPC##_child[1 - n_CI]))			\
	parent->NSPC##_child[n_CI]->NSPC##_parent = parent;				\
      if ((gparent->NSPC##_child[parent_CI] = n->NSPC##_child[n_CI]))			\
	gparent->NSPC##_child[parent_CI]->NSPC##_parent = gparent;			\
      n->NSPC##_child[1 - n_CI] = parent;						\
      n->NSPC##_child[n_CI] = gparent;							\
      parent->NSPC##_parent = gparent->NSPC##_parent = n;				\
    } else {										\
      /* ZIGZIG */									\
      if ((gparent->NSPC##_child[parent_CI] = parent->NSPC##_child[1 - n_CI]))		\
	gparent->NSPC##_child[parent_CI]->NSPC##_parent = gparent;			\
      if ((parent->NSPC##_child[n_CI] = n->NSPC##_child[1 - n_CI]))			\
	parent->NSPC##_child[n_CI]->NSPC##_parent = parent;				\
      parent->NSPC##_child[1 - n_CI] = gparent;						\
      gparent->NSPC##_parent = parent;							\
      n->NSPC##_child[1 - n_CI] = parent;						\
      parent->NSPC##_parent = n;							\
    }											\
    /* move up two generations in the tree Note: we don't fix pointers			\
     between n and ggparent, because in the next iteration, these are			\
     overwritten without being read */							\
    parent = ggparent;									\
    n_CI = gparent_CI;									\
  }											\
											\
  /* n is now at the root */								\
  *tree = n;                                                                            \
  n->NSPC##_parent = NULL;                                                          	\
  switch (op) {										\
  case SPLAY_OP_INSERT:									\
    return newnode;									\
  case SPLAY_OP_FIND:									\
    return (comp == 0) ? n : NULL;							\
  case SPLAY_OP_REMOVE:									\
    if (comp != 0)									\
      return NULL;									\
    if (!n->NSPC##_child[0]) {								\
      /* no left subtree, so new tree is right subtree */				\
      *tree = n->NSPC##_child[1];							\
    } else {										\
      /* unhook and splay the left subtree using the removed node's key */		\
      n->NSPC##_child[0]->NSPC##_parent = NULL;                                 	\
      NSPC##_splay_util(&n->NSPC##_child[0], SPLAY_OP_FIND, (void *) n->KEYFIELD);  	\
      /* the root of the left subtree is now a node with no right child,		\
	 so hook in the right subtree as its right child */				\
      if ((n->NSPC##_child[0]->NSPC##_child[1] = n->NSPC##_child[1]))			\
	n->NSPC##_child[1]->NSPC##_parent = n->NSPC##_child[0];				\
      /* set the root of the whole tree to be the root of the left subtree */		\
      *tree = n->NSPC##_child[0];							\
    }											\
    if (*tree)										\
      (*tree)->NSPC##_parent = NULL;							\
    n->NSPC##_child[0] = n->NSPC##_child[1] = n->NSPC##_parent = NULL;			\
    return n;										\
  default:										\
    /* fail safely */									\
    return NULL;									\
  };											\
}


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _SPLAY_TREE_H */
