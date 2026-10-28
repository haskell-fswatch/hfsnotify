#ifndef CBITS_C_FSEVENTS_H
#define CBITS_C_FSEVENTS_H 1

#include <CoreServices/CoreServices.h>
#include <pthread.h>

typedef struct {
  FSEventStreamRef eventStream;
  CFRunLoopRef runLoop;
  int writefd;
  pthread_mutex_t mut;
} watch;


int createWatch ( char** folders
                , int n
                , UInt32 createFlags
                , UInt64 since
                , double latency
                , int* fd
                , void** wp
                );

void destroyWatch(watch* w);

UInt64 getCurrentEventId(void);

void osVersion( SInt32 *majorVersion
              , SInt32 *minorVersion
	      , SInt32 *bugFixVersion);

#endif

