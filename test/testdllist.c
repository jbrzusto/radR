/* testdllist.c - test the doubly-linked list building / maintenance code in dllist.h */


#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <time.h>


#include "dllist.h"

struct my_node;

typedef struct my_node {
  int key;
  char stuff[32];
  STRUCT_DLLIST_NODE_ITEM(mine, struct my_node);
} t_my_node;

typedef struct  {
  int something;
  STRUCT_DLLIST_ROOT_ITEM(mine, struct my_node);
  double x;
} t_my_dllist;

t_my_dllist my_dllist;

void traverse_dllist(t_my_dllist *dllist) {
  t_my_node *n = DLLIST_HEAD(mine, dllist);
  while (n) {
    printf("%d\n", n->key);
    n = DLLIST_SUCC(mine, n);
  }
}

int
main (int argc, char *argv[]) {
  int i, j;
  int m = atoi(argv[1]);
  int n = atoi(argv[2]);
  struct timeval tv;
  int * in_list = (int *) calloc (m, sizeof(int));
  t_my_node * nodes = (t_my_node*) calloc(m, sizeof(t_my_node));
  t_my_node *np;

  gettimeofday(&tv, ((void *)0));
  srandom((unsigned int) tv.tv_usec);

  DLLIST_INIT(mine, &my_dllist);

  for (i = 0; i < m; ++i) {
    nodes[i].key = i;
    sprintf(nodes[i].stuff, "key=%d", i);
    DLLIST_PUSH(mine, &my_dllist, &nodes[i]);
    if (DLLIST_HEAD(mine, &my_dllist) != &nodes[i])
      printf("Error: after push, head is not pushed node with key == %d\n", i);
    in_list[i] = 1;
  }

  printf("After building.\n");
  traverse_dllist(&my_dllist);
  for (i = 0; i < n; ++i) {
    /* do a raise */
    j = random() % m;
    if (random() % 2 < 1) {
      if (j < m && in_list[j]) {
	DLLIST_RAISE(mine, &my_dllist, &nodes[j]);
	if (DLLIST_HEAD(mine, &my_dllist) != &nodes[j])
	  printf("Error: after raise, head is not raised node with key == %d\n", j);
      }
      /* do a remove */
    } else {
      if (j < m && in_list[j]) {
	DLLIST_REMOVE(mine, &my_dllist, &nodes[j]);
	for (np = DLLIST_HEAD(mine, &my_dllist); np; np = DLLIST_SUCC(mine, np))
	  if (np == &nodes[j])
	    printf("Error: removed node with key == %d still in dllist\n", j);
	in_list[j] = 0;
      }
    }
  }
  printf("After raises/removals.\n");
  traverse_dllist(&my_dllist);
  exit(0);
}
