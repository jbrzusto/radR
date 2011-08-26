/* svn $Id$
  
  definitions for a simple threading scenario:  a persistent thread that
  is occasionally awakened to do a bit of work and then put itself to sleep.
  The thread is detached so it can be killed without joining.

  TAG is a unique identifier for the sleepable thread.

  An array of sleepable threads can be declared using a tag with an
  index expression e.g.  and in that case, the particular thread must
  be indicated whenever the other macros are used, e.g.:

       THREAD_DECLARE(getter[4])
       ...
       for (i=0; i < 4; ++i) 
           THREAD_CREATE(getter[i], my_getter_fun, my_getter_parms[i]);
       ...

       THREAD_WAKE(getter[2]);
       ...
       THREAD_SLEEP(getter[my_parms[threadno]]);

  When using an array of sleepable threads, each thread sleeps or not
  independently (i.e. uses its own mutex and condition variable).
  Thus, if multiple threads use the same function, that code must have
  some way of knowing which thread it is currently being executed by
  when it uses THREAD_SLEEP; presumably, a field in the
  parms to the thread function contains this information.

  For data synchronization, there is a mutex associated with the pointer
  passed to the thread function.  It is recommended to lock this mutex
  when reading or writing the pointer's destination structure.

  No errors are reported, although some obvious ones are guarded against.

*/

#ifndef _THREADUTIL_H
#define _THREADUTIL_H

#ifdef THREADUTIL_DEBUG
#define THREAD_PF(...) printf(__VA_ARGS__); fflush(stdout)
#else
#define THREAD_PF(...)
#endif

// declare a mutex for locking access to a particular
// data item, presumably a field in a structure

#define THREAD_DECLARE_DATA(TAG) 						\
  static pthread_mutex_t THREAD_data_mut_ ## TAG = PTHREAD_MUTEX_INITIALIZER

// lock a data item

#define THREAD_LOCK_DATA(TAG) { 						\
THREAD_PF("THREAD_LOCK_DATA: trying to lock data " # TAG "\n"); 		\
  pthread_mutex_lock(& THREAD_data_mut_ ## TAG ); 				\
THREAD_PF("THREAD_LOCK_DATA: locked data " # TAG "\n"); 			\
}

// unlock a data item
#define THREAD_UNLOCK_DATA(TAG) { 						\
  pthread_mutex_trylock(& THREAD_data_mut_ ## TAG ); 				\
THREAD_PF("THREAD_UNLOCK_DATA: trying to unlock data " # TAG  "\n"); 		\
  pthread_mutex_unlock(& THREAD_data_mut_ ## TAG ); 				\
THREAD_PF("THREAD_UNLOCK_DATA: unlocked data " # TAG  "\n"); 			\
}

// is a field is locked?
#define THREAD_IS_DATA_LOCKED(TAG) ({ 						\
  int __rv__; 									\
  if (EBUSY != pthread_mutex_trylock(& THREAD_data_mut_ ## TAG)) {		\
THREAD_PF("THREAD_IS_DATA_LOCKED: trying to release data " # TAG  "\n"); 	\
    pthread_mutex_unlock(& THREAD_data_mut_ ## TAG ); 				\
    __rv__ = 0; 								\
  } else {									\
    __rv__ = 1; 								\
  } 										\
  __rv__; 									\
})
 

#define THREAD_DECLARE(TAG)							\
  static pthread_t THREAD_pthread_ ## TAG;					\
  static int THREAD_created_ ## TAG;						\
  static int THREAD_started_ ## TAG;						\
  static pthread_mutex_t THREAD_mut_ ## TAG;					\
  static pthread_cond_t  THREAD_cv_ ## TAG;                               	\

#define THREAD_CREATE_PRIORITY(TAG, FUN, PARM, PRIORITY) {			\
  if (! THREAD_created_ ## TAG) {						\
    struct sched_param THREAD_sp;                                         	\
    pthread_attr_t THREAD_attr;                                           	\
    THREAD_sp.sched_priority = PRIORITY;            				\
    pthread_attr_init (&THREAD_attr);                                     	\
    pthread_attr_setschedparam (&THREAD_attr, &THREAD_sp);                  	\
    pthread_attr_setdetachstate (&THREAD_attr, PTHREAD_CREATE_JOINABLE); 	\
    pthread_mutex_init (& THREAD_mut_ ## TAG, NULL);				\
    pthread_cond_init ( & THREAD_cv_ ## TAG, NULL );                      	\
    THREAD_started_ ## TAG = 0;							\
    if (!pthread_create (&THREAD_pthread_ ## TAG, NULL, FUN, PARM )) 		\
      THREAD_created_ ## TAG = 1;						\
  } 										\
}

#define THREAD_CREATE(TAG, FUN, PARM) 						\
THREAD_CREATE_PRIORITY(TAG, FUN, PARM, THREAD_PRIORITY_NORMAL)

#define THREAD_CREATE_HP(TAG, FUN, PARM) 					\
THREAD_CREATE_PRIORITY(TAG, FUN, PARM, THREAD_PRIORITY_TIME_CRITICAL)

#define THREAD_WAKE(TAG) {							\
THREAD_PF("wake_sleepable_thread:  trying to lock mutex " # TAG "\n"); 		\
    if (THREAD_started_ ## TAG) {						\
      pthread_mutex_lock (& THREAD_mut_ ## TAG);				\
THREAD_PF("wake_sleepable_thread:  locked mutex " # TAG "\n"); 			\
      pthread_cond_signal (& THREAD_cv_ ## TAG );				\
THREAD_PF("wake_sleepable_thread: signalled condition " # TAG "\n"); 		\
      pthread_mutex_unlock (& THREAD_mut_ ## TAG);				\
THREAD_PF("wake_sleepable_thread: unlocked mutex " # TAG "\n"); 		\
    }                                                                   	\
  }

#define THREAD_KILL(TAG, WAIT) {						\
  if (THREAD_created_ ## TAG) {					        	\
    THREAD_WAKE(TAG);                                                 		\
THREAD_PF("kill_sleepable_thread: unlocking mutexes " # TAG "\n"); 		\
    pthread_mutex_unlock (& THREAD_mut_ ## TAG);				\
THREAD_PF("kill_sleepable_thread: doing cancel " # TAG "\n"); 			\
    pthread_cancel (THREAD_pthread_ ## TAG);					\
THREAD_PF("kill_sleepable_thread: doing cond_destroy " # TAG "\n");		\
    pthread_cond_destroy (& THREAD_cv_ ## TAG);                                 \
THREAD_PF("kill_sleepable_thread: doing mutex_destroy " # TAG "\n");		\
    pthread_mutex_destroy (& THREAD_mut_ ## TAG);                               \
THREAD_PF("kill_sleepable_thread: finished cancel " # TAG "\n"); 		\
    THREAD_created_ ## TAG = 0;							\
    THREAD_started_ ## TAG = 0;							\
  }                                                                 		\
  if (WAIT) { 									\
THREAD_PF("kill_sleepable_thread: waiting on thread cancellation " # TAG "\n"); \
    pthread_join(THREAD_pthread_ ## TAG, NULL); 				\
 } 										\
}


#define THREAD_IS_CREATED(TAG) (THREAD_created_ ## TAG)
#define THREAD_IS_RUNNING(TAG) (THREAD_started_ ## TAG)

/* WARNING: use the following only from inside the sleepable thread
   functions */

#define THREAD_STARTING(TAG) { 							\
  THREAD_started_ ## TAG = 1; 							\
  pthread_setcanceltype (PTHREAD_CANCEL_ASYNCHRONOUS, NULL); 			\
  pthread_setcancelstate (PTHREAD_CANCEL_ENABLE, NULL);                         \
THREAD_PF("sleepable_thread_started " # TAG "\n");                              \
}

#define THREAD_SLEEP(TAG) {							\
THREAD_PF("sleep_sleepable_thread: trying to lock mutex " # TAG "\n"); 		\
    pthread_mutex_lock (& THREAD_mut_ ## TAG);					\
THREAD_PF("sleep_sleepable_thread: locked mutex " # TAG "\n"); 			\
    pthread_cond_wait (&THREAD_cv_ ## TAG, & THREAD_mut_ ## TAG);		\
THREAD_PF("sleep_sleepable_thread: returned from wait " # TAG "\n"); 		\
    pthread_mutex_unlock (& THREAD_mut_ ## TAG);				\
THREAD_PF("sleep_sleepable_thread: unlocked mutex " # TAG "\n"); 		\
  }

#define THREAD_SLEEP_TIMED(TAG, TIMESPEC) {					\
THREAD_PF("timed_sleep_sleepable_thread: trying to lock mutex " # TAG "\n"); 	\
    pthread_mutex_lock (& THREAD_mut_ ## TAG);					\
THREAD_PF("timed_sleep_sleepable_thread: locked mutex " # TAG "\n"); 		\
 pthread_cond_timedwait (&THREAD_cv_ ## TAG, & THREAD_mut_ ## TAG, TIMESPEC);	\
THREAD_PF("timed_sleep_sleepable_thread: returned from wait " # TAG "\n"); 	\
    pthread_mutex_unlock (& THREAD_mut_ ## TAG);				\
THREAD_PF("timed_sleep_sleepable_thread: unlocked mutex " # TAG "\n"); 		\
  }

#define THREAD_CLEANUP(TAG) {                                         		\
  pthread_mutex_unlock (& THREAD_mut_ ## TAG);                                  \
THREAD_PF("sleepable_thread_cleanup: unlocked mutex " # TAG "\n"); 		\
  pthread_mutex_unlock (& THREAD_data_mut_ ## TAG);                             \
THREAD_PF("sleepable_thread_cleanup: unlocked data mutex " # TAG "\n"); 	\
  }										  

#define THREAD_DIE(TAG)								\
  THREAD_started_ ## TAG = THREAD_created_ ## TAG = 0;                          \
THREAD_PF("die_sleepable_thread: doing cond_destroy " # TAG "\n");		\
    pthread_cond_destroy (& THREAD_cv_ ## TAG);                                 \
THREAD_PF("die_sleepable_thread: doing mutex_destroy " # TAG "\n");		\
    pthread_mutex_destroy (& THREAD_mut_ ## TAG);                               \
THREAD_PF("die_sleepable_thread: doing data mutex_destroy " # TAG "\n");	\
    pthread_mutex_destroy (& THREAD_data_mut_ ## TAG);                          \
  pthread_exit (NULL)

/* if the following is used, the end of the function
   must be preceded by a call to to THREAD_DROP_CLEANUP(X)
   where X is non-zero if the cleanup is to be executed.
*/

#define THREAD_ADD_CLEANUP(FUN, X)		 				\
  pthread_cleanup_push ((FUN), (X))

#define THREAD_DROP_CLEANUP(X)							\
  pthread_cleanup_pop ((X))

#endif /* _THREADUTIL_H */
