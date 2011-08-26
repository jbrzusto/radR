/* testsplay.c - test the tree-building / maintenance code in splay-tree.h */


#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>


#include "splay-tree.h"

struct my_node;

typedef struct my_node {
  int key;
  char stuff[32];
  STRUCT_SPLAY_NODE_ITEM(mine, struct my_node);
} t_my_node;

typedef struct  {
  int something;
  STRUCT_SPLAY_ROOT_ITEM(mine, struct my_node);
  double x;
} t_my_tree;

t_my_tree my_tree;

int
mine_splay_compare_keys (int key1, int key2) {
  return key1 - key2;
}

DECLARE_SPLAY_FUNCTIONS(mine, t_my_node, int, key);
DEFINE_SPLAY_FUNCTIONS(mine,  t_my_node, int, key);

void traverse_tree_helper(t_my_node *n, int indent) {
  if (!n)
    return;
  printf("%*d\n", indent, n->key);
  traverse_tree_helper(n->mine_child[0], indent+3);
  traverse_tree_helper(n->mine_child[1], indent+3);
}

void traverse_tree(t_my_tree *sp) {
  traverse_tree_helper(sp->mine_root, 2);
}

void traverse_tree_helper_inorder(t_my_node *n, int indent) {
  if (!n)
    return;
  traverse_tree_helper_inorder(n->mine_child[0], indent+3);
  printf("%*d\n", indent, n->key);
  traverse_tree_helper_inorder(n->mine_child[1], indent+3);
}

void traverse_tree_inorder(t_my_tree *sp) {
  traverse_tree_helper_inorder(sp->mine_root, 2);
}

int
main (int argc, char *argv[]) {
  int i, j;
  int m = atoi(argv[1]);
  int n = atoi(argv[2]);
  struct timeval tv;
  int * in_tree = (int *) calloc (m, sizeof(int));
  t_my_node * nodes = (t_my_node*) calloc(m, sizeof(t_my_node));
  t_my_node *np;

  gettimeofday(&tv, ((void *)0));
  srandom((unsigned int) tv.tv_usec);

  SPLAY_INIT(mine, &my_tree);

  for (i = 0; i < m; ++i) {
    nodes[i].key = i;
    sprintf(nodes[i].stuff, "key=%d", i);
    SPLAY_INSERT(mine, &my_tree, &nodes[i]);
    if (SPLAY_ROOT(mine, &my_tree) != &nodes[i])
      printf("Error: after insert, root is not inserted node with key == %d\n", i);
    in_tree[i] = 1;
  }
  /*
  traverse_tree_inorder(&my_tree);
  traverse_tree(&my_tree);
  */

  for (i = 0; i < n; ++i) {
    /* do a find */
    j = random() % (2 * m);
    if (random() % 2) {
      np = SPLAY_FIND(mine, &my_tree, j);
      if (j < m && in_tree[j] && np != &nodes[j]) {
	printf("Error: failed to get entry with key == %d\n", j);
      } else if ((j >= m || !in_tree[j]) && np) {
	printf("Error: invalidly found non-existent or removed entry with key == %d\n", j);
      }
      if (j < m && in_tree[j] && SPLAY_ROOT(mine, &my_tree) != &nodes[j])
	printf("Error: root node == %d != last node accessed\n", j);
      /* do a remove */
    } else {
      np = SPLAY_REMOVE(mine, &my_tree, j);
      if (j < m && in_tree[j] && np != &nodes[j]) {
	printf("Error: failed to delete non-deleted entry with key == %d\n", j);
      }
      if ((j >= m || !in_tree[j]) && np) {
	printf("Error: invalidly found non-existent or removed entry with key == %d\n", j);
      }
      if (j <= m)
	in_tree[j] = 0;
    }
  }
  traverse_tree_inorder(&my_tree);
  traverse_tree(&my_tree);
  exit(0);
}
