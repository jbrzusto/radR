/* 
   An implementation of a doubly-linked list, for easy inclusion
   in existing structured types.

   Copyright 2007 by John Brzustowski.  Licence GPL.

*/

#ifndef _DLLIST_H
#define _DLLIST_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

  /* create typedefs for doubly-linked lists in the namespace given by
     NSPC, whose node values will be of type NODETYPE, and whose root
     will be added to the item ROOTTYPE. NODETYPE must be a
     forward-declared structure, and the DLLIST_ITEM macro below must
     be included in its definition to create the fields needed for
     splaying.
  */
  
  /* create the components to be added to the structure of type NODETYPE
     which are needed for the doubly-linked list; a line of the form
     STRUCT_DLLIST_NODE_ITEM(struct my_node)
     must be included in the definition of struct my_node,
     which must have a forward declaration
     
     e.g.

     struct my_node;

     typedef struct my_node {
     int key;
     ...
     STRUCT_DLLIST_NODE_ITEM(mine, struct my_node);
     ...
     } t_my_node;

     typedef struct my_list {
     ...
     STRUCT_DLLIST_ROOT_ITEM(mine, struct my_node);
     ...
     } t_my_list;

     t_my_list list;

     DECLARE_DLLIST_FUNCTIONS(mine, t_my_node, int, key);
     DEFINE_DLLIST_FUNCTIONS(mine,  t_my_node, int, key);

     // Then linked list functions are called as 

     t_my_node *n, node;

     DLLIST_INIT(mine, &list);
     DLLIST_PUSH(mine, &list, node)(mine, &list, node); 
     n = DLLIST_POP(mine, &list);
     n = DLLIST_HEAD(mine, &list);
     n = DLLIST_TAIL(mine, &list);
     DLLIST_RAISE(mine, &list, &node);
     DLLIST_REMOVE(mine, &list, &node);

     "mine" is the literal namespace prefix used in function definitions.
  */

#define STRUCT_DLLIST_NODE_ITEM(NSPC, NODETYPE) 	\
  /* Pointers to predecessor and successord.  */ 	\
  NODETYPE *NSPC##_pred; 				\
  NODETYPE *NSPC##_succ; 

#define STRUCT_DLLIST_ROOT_ITEM(NSPC, NODETYPE) 	\
  /* Pointers to the root node of the splay tree.  */ 	\
  NODETYPE *NSPC##_head;                          	\
  NODETYPE *NSPC##_tail;                          

#define DLLIST_HEAD(NSPC, dlist)         (dlist)->NSPC##_head
#define DLLIST_TAIL(NSPC, dlist)         (dlist)->NSPC##_tail
#define DLLIST_INIT(NSPC, dlist)         DLLIST_HEAD(NSPC, dlist) = DLLIST_TAIL(NSPC, dlist) = NULL;
#define DLLIST_PRED(NSPC, node)          (node)->NSPC##_pred
#define DLLIST_SUCC(NSPC, node)          (node)->NSPC##_succ

  /* add a node to the front of the list */
#define DLLIST_PUSH(NSPC, dlist, node)   { if (((node)->NSPC##_succ = DLLIST_HEAD(NSPC, dlist))) 	\
                                           (node)->NSPC##_succ->NSPC##_pred = node; 			\
                                         (node)->NSPC##_pred = NULL; 					\
                                         DLLIST_HEAD(NSPC, dlist) = node;				\
                                         if(!DLLIST_TAIL(NSPC, dlist)) 					\
                                           DLLIST_TAIL(NSPC, dlist) = node; }

  /* remove and return the first node in the list */
#define DLLIST_POP(NSPC, dlist)          ({ NODETYPE *_tmp_ = DLLIST_HEAD(NSPC, dlist); 		\
                                          DLLIST_HEAD(NSPC, dlist) = _tmp_->NSPC##_succ; 		\
                                          _tmp_->NSPC##_succ = NULL; 					\
                                          _tmp_; 							\
                                         })

/* remove a node from the list */
#define DLLIST_REMOVE(NSPC, dlist, node) { if ((node)->NSPC##_pred)  					\
                                           (node)->NSPC##_pred->NSPC##_succ = (node)->NSPC##_succ; 	\
                                         else 								\
                                           DLLIST_HEAD(NSPC, dlist) = (node)->NSPC##_succ; 		\
                                         if ((node)->NSPC##_succ)  					\
                                           (node)->NSPC##_succ->NSPC##_pred = (node)->NSPC##_pred; 	\
                                         else 								\
                                           DLLIST_TAIL(NSPC, dlist) = (node)->NSPC##_pred; 		\
                                         (node)->NSPC##_succ = (node)->NSPC##_pred = NULL; }

  /* bring a node in the list to the head */
#define DLLIST_RAISE(NSPC, dlist, node) if (node != DLLIST_HEAD(NSPC, dlist)) { 			\
                                          if ((node)->NSPC##_pred)  					\
                                            (node)->NSPC##_pred->NSPC##_succ = (node)->NSPC##_succ; 	\
                                          if ((node)->NSPC##_succ)  					\
                                            (node)->NSPC##_succ->NSPC##_pred = (node)->NSPC##_pred; 	\
                                          else 								\
                                            DLLIST_TAIL(NSPC, dlist) = (node)->NSPC##_pred; 		\
                                          (node)->NSPC##_succ = DLLIST_HEAD(NSPC, dlist);    		\
                                          (node)->NSPC##_succ->NSPC##_pred = node; 			\
                                          (node)->NSPC##_pred = NULL; 					\
                                          DLLIST_HEAD(NSPC, dlist) = node; 				\
                                        }

                                       
#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _DLLIST_H */
