/* 
   An implementation of a write-back cache using a splay tree for
   insert/find/delete and a doubly-linked list for the LRU replacement
   policy.  The cached items are assumed to be managed by the user;
   this file simply adds structures and functions for managing them as
   a cache.  The user may provide callbacks for dealing with cache
   misses and cache eviction.

   Copyright 2007 by John Brzustowski.  Licence GPL.

*/

#include "splay-tree.h"
#include "dllist.h"

#ifndef _CACHE_H
#define _CACHE_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

  /* create typedefs for a cache in the namespace given by NSPC, whose key values
     will be of type KEYTYPE, and whose item values will be of type ENTRYTYPE, and
     whose root will be added to the item HEADTYPE
     ENTRYTYPE must be a forward-declared structure, and the SPLAY_ENTRY macro below
     must be included in its definition to create the fields needed for splaying.
  */
  
  /* create the components to be added to the structure of type ENTRYTYPE
     which are needed for the splay tree; a line of the form
     STRUCT_CACHE_ENTRY_ITEM(struct my_item)
     must be included in the definition of struct my_item,
     which must have a forward declaration
     
     e.g.

     struct my_entry;

     typedef struct my_entry {
     int key;
     ...
     STRUCT_CACHE_ENTRY_ITEM(mine, struct my_entry);
     ...
     } t_my_entry;

     typedef struct my_tree {
     ...
     STRUCT_CACHE_ROOT_ITEM(mine, struct my_entry);
     ...
     } t_my_cache;

     t_my_cache cache;

     DECLARE_CACHE_FUNCTIONS(mine, t_my_cache, t_my_entry, int, key, cache_miss_fun, cache_eviction_fun);
     DEFINE_CACHE_FUNCTIONS(mine,  t_my_cache, t_my_entry, int, key, cache_miss_fun, cache_eviction_fun);

     // Then splay functions are called as 

     t_my_entry *n, *entry;

     CACHE_INIT(mine, &cache, size);      // initialize the cache structure
     CACHE_PUT(mine, &cache, entry);      // insert / update an entry into / in the cache
     n = CACHE_GET(mine, &cache, key);    // fetch the entry with specified key from the cache
     CACHE_FLUSH(mine, &cache);           // rewrite all dirty cache entries to backing storage
     CACHE_RESIZE(mine, &cache, newsize); // change the maximum size limit of the cache;
                                          // if newsize is smaller than the number of entries currently
                                          // in the cache, excess entries are discarded, in order from least to most
                                          // recently used, triggering cache_eviction_fun callbacks as appropriate

     "mine" is the literal namespace prefix used in function definitions.

     You must define the following two callbacks:

     void read_back(HEADTYPE *cache, KEYTYPE key, ENTRYTYPE *entry)
     - read the entry with the given key from the backing store.
       Returns a pointer to the entry, if one with that key exists, or NULL if not.

     void write_back(HEADTYPE *cache, ENTRYTYPE *entry)
     - write the entry to the backing store.

     ENTRYTYPE *get_new_entry(HEADTYPE *cache)
     - called when a new entry is to be added to a non-full
       cache; returns a never-used cache entry

     void copy_to_entry(HEADTYPE *cache, ENTRYTYPE *entry, void * data)
     - called when data must be copied to a cache entry 
  */

#define STRUCT_CACHE_ENTRY_ITEM(NSPC, NODETYPE) 		\
  int NSPC##_dirty; 						\
  /* items for the splay and dlist structure  */ 		\
  STRUCT_SPLAY_NODE_ITEM(NSPC, NODETYPE); 			\
  STRUCT_DLLIST_NODE_ITEM(NSPC, NODETYPE);

#define STRUCT_CACHE_ROOT_ITEM(NSPC, NODETYPE) 			\
  /* items for the splay and dlist roots, and cache params */ 	\
  int NSPC##_max_cache_size; 					\
  int NSPC##_cache_size; 					\
  STRUCT_SPLAY_ROOT_ITEM(NSPC, NODETYPE); 			\
  STRUCT_DLLIST_ROOT_ITEM(NSPC, NODETYPE);                      \
  NODETYPE *NSPC##_cache_free_list;
 
#define CACHE_INIT(NSPC, cache, size)  		NSPC##_cache_init(cache, size)
#define CACHE_PUT(NSPC, cache, key, data)       NSPC##_cache_put(cache, key, data)
#define CACHE_GET(NSPC, cache, key)    		NSPC##_cache_get(cache, key)
#define CACHE_REMOVE(NSPC, cache, key)    	NSPC##_cache_remove(cache, key)
#define CACHE_FLUSH(NSPC, cache)       		NSPC##_cache_flush(cache)
#define CACHE_RESIZE(NSPC, cache, newsize) 	NSPC##_cache_resize(cache, newsize)
#define CACHE_SIZE(NSPC, cache)                 (cache)->NSPC##_cache_size
#define CACHE_MAX_SIZE(NSPC, cache)             (cache)->NSPC##_max_cache_size

#define DECLARE_CACHE_FUNCTIONS(NSPC, HEADTYPE, ENTRYTYPE, KEYTYPE, KEYFIELD) 								 \
extern void NSPC##_cache_init(HEADTYPE *cache, int size); 										 \
extern void NSPC##_cache_put(HEADTYPE *cache, KEYTYPE key, void * data); 								 \
extern ENTRYTYPE *NSPC##_cache_get(HEADTYPE *cache, KEYTYPE key); 									 \
extern ENTRYTYPE *NSPC##_cache_remove(HEADTYPE *cache, KEYTYPE key); 									 \
extern void NSPC##_cache_flush(HEADTYPE *cache);  											 \
DECLARE_SPLAY_FUNCTIONS(NSPC, ENTRYTYPE, KEYTYPE, KEYFIELD);

 
#define DEFINE_CACHE_FUNCTIONS(NSPC, HEADTYPE, ENTRYTYPE, KEYTYPE, KEYFIELD, F_GET_NEW_ENTRY, F_COPY_TO_ENTRY, F_READ_BACK, F_WRITE_BACK)\
int																	 \
NSPC##_splay_compare_keys(KEYTYPE key1, KEYTYPE key2) { 										 \
  return (int) (key1 - key2); 														 \
} 																	 \
 																	 \
DEFINE_SPLAY_FUNCTIONS(NSPC, ENTRYTYPE, KEYTYPE, KEYFIELD); 										 \
																	 \
void 																	 \
NSPC##_cache_init(HEADTYPE *cache, int size) {												 \
  SPLAY_INIT(NSPC, cache);														 \
  DLLIST_INIT(NSPC, cache);														 \
  cache->NSPC##_max_cache_size = size;												 	 \
  cache->NSPC##_cache_size = 0;													 	 \
  cache->NSPC##_cache_free_list = NULL;                                                                                                  \
}																	 \
																	 \
ENTRYTYPE *																 \
NSPC##_get_empty_entry(HEADTYPE *cache) {												 \
  ENTRYTYPE *ce; 															 \
  /* get an empty cache entry, possibly by evicting an existing one */									 \
  if (cache->NSPC##_cache_size < cache->NSPC##_max_cache_size) {								 	 \
    /* cache has room for more new entries */												 \
    if ((ce=cache->NSPC##_cache_free_list)) {                                                                                            \
      cache->NSPC##_cache_free_list = DLLIST_SUCC(NSPC, ce);                                                                             \
    } else {                                                                                                                             \
      ce = F_GET_NEW_ENTRY(cache);													 \
    }                                                                                                                                    \
    ++cache->NSPC##_cache_size;													 	 \
    DLLIST_PUSH(NSPC, cache, ce); 													 \
  } else {																 \
    /* evict the least recently used entry */												 \
    ce = DLLIST_TAIL(NSPC, cache);													 \
    if (ce->NSPC##_dirty)														 \
      F_WRITE_BACK(cache, ce);														 \
    SPLAY_REMOVE(NSPC, cache, ce->KEYFIELD);												 \
    DLLIST_RAISE(NSPC, cache, ce);													 \
  }																	 \
  return ce;																 \
}																	 \
																	 \
void 																	 \
NSPC##_cache_put(HEADTYPE *cache, KEYTYPE key, void *data) {										 \
  ENTRYTYPE *ce; 															 \
  ce = SPLAY_FIND(NSPC, cache, key);													 \
  if (!ce) {																 \
    ce = NSPC##_get_empty_entry(cache);												 	 \
    ce->key = key;                                                                                                                       \
    SPLAY_INSERT(NSPC, cache, ce);                                                                                                       \
  } else { 																 \
    DLLIST_RAISE(NSPC, cache, ce);													 \
  } 																	 \
  F_COPY_TO_ENTRY(cache, ce, data);													 \
  ce->NSPC##_dirty = TRUE;														 \
}																	 \
		  															 \
ENTRYTYPE *															         \
NSPC##_cache_remove(HEADTYPE *cache, KEYTYPE key) {											 \
  /* find the cache entry with the specified key and remove it from the                                                                  \
     cache if it exists.  The cache entry will be moved to the free list.                                                                \
 */																	 \
  ENTRYTYPE *ce;															 \
  ce = SPLAY_REMOVE(NSPC, cache, key);													 \
  if (ce) {																 \
    DLLIST_REMOVE(NSPC, cache, ce);                                                                                                      \
    DLLIST_SUCC(NSPC, ce) = cache->NSPC##_cache_free_list;                                                                               \
    cache->NSPC##_cache_free_list = ce;													 \
    -- cache->NSPC##_cache_size;                                                                                                         \
  }                                                                                                                                      \
  return ce;																 \
}																	 \
																	 \
ENTRYTYPE *																 \
NSPC##_cache_get(HEADTYPE *cache, KEYTYPE key) {											 \
  /* get the cache entry with the specified key; if no such entry is									 \
     in the cache, call the F_READ_BACK function to find										 \
     it in the backing store.  In either case, if the entry is										 \
     found, raise it to the front of the dlist.												 \
 */																	 \
  ENTRYTYPE *ce;															 \
  ce = SPLAY_FIND(NSPC, cache, key);													 \
  if (!ce) {																 \
    ce = NSPC##_get_empty_entry(cache);													 \
    F_READ_BACK(cache, key, ce);													 \
    ce->key = key;                                                                                                                       \
    ce->NSPC##_dirty = FALSE;													 	 \
    SPLAY_INSERT(NSPC, cache, ce);                                                                                                       \
  } else {																 \
    DLLIST_RAISE(NSPC, cache, ce);													 \
  }                                                                                                                                      \
  return ce;																 \
}																	 \
																	 \
void																	 \
NSPC##_cache_flush(HEADTYPE *cache) {													 \
  /* for each cache entry, if it is dirty, write it to backing										 \
     store and set its dirty flag to FALSE; This is done in										 \
     order from least to most recently used entry. */											 \
  ENTRYTYPE *ce;															 \
  for (ce = DLLIST_TAIL(NSPC, cache); ce; ce = DLLIST_PRED(NSPC, ce)) {									 \
    if (ce->NSPC##_dirty) {														 \
      F_WRITE_BACK(cache, ce);														 \
      ce->NSPC##_dirty = FALSE;													 	 \
    }																	 \
  }																	 \
}


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _CACHE_H */
