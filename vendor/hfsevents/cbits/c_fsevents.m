#include <CoreServices/CoreServices.h>
#include <pthread.h>
#include <unistd.h>

#include "c_fsevents.h"

void writeEvent(int fd, UInt64 eventId, UInt64 eventFlags, char* path) {
  UInt64 buf[3];
  buf[0] = eventId;
  buf[1] = eventFlags;
  buf[2] = (UInt64)strlen(path);
  write(fd, buf, 3*sizeof(UInt64));
  write(fd, path, strlen(path));
}

void watchCallback(ConstFSEventStreamRef streamRef, void *clientCallBackInfo,
                   size_t n, void *eventPaths, const FSEventStreamEventFlags eventFlags[],
                   const FSEventStreamEventId eventIds[]) {
  int i;
  watch *w = clientCallBackInfo;
  char **paths = eventPaths;
  for (i=0; i<n; i++) {
    writeEvent(w->writefd, eventIds[i], eventFlags[i], paths[i]);
  }
}

void *watchRunLoop(void *vw) {
  watch* w = (watch*) vw;
  CFRunLoopRef rl = CFRunLoopGetCurrent();
  CFRetain(rl);
  w->runLoop = rl;
  FSEventStreamScheduleWithRunLoop(w->eventStream, rl, kCFRunLoopDefaultMode);
  FSEventStreamStart(w->eventStream);
  pthread_mutex_unlock(&w->mut);
  CFRunLoopRun();
  pthread_exit(NULL);
}

#define MAX_WATCH_PATHS 4096

int createWatch( char** folders
               , int n
               , UInt32 createFlags
               , UInt64 since
               , double latency
               , int* fd
               , void** wp
               ) {
  int i;
  int rv;
  int pfds[2];
  if(n>MAX_WATCH_PATHS) return -1;
  if(pipe(pfds)) return -1;
  if(!since) since = kFSEventStreamEventIdSinceNow;
  CFStringRef *cffolders = malloc(n * sizeof(CFStringRef));
  watch *w;
  w  = malloc(sizeof(watch));
  FSEventStreamContext ctx;
  ctx.version = 0;
  ctx.info = (void*)w;
  ctx.retain = NULL;
  ctx.release = NULL;
  ctx.copyDescription = NULL;
  for(i=0;i<n;i++) {
    cffolders[i] = CFStringCreateWithCString(NULL, folders[i], kCFStringEncodingUTF8);
  }
  CFArrayRef paths = CFArrayCreate(NULL, (const void **)cffolders, n, NULL);
  FSEventStreamRef es = FSEventStreamCreate(NULL, &watchCallback, &ctx, paths, since, latency, createFlags);
  pthread_t t;
  if(es != NULL) { /* fixme is this the correct way to check for failure? */
    w->writefd = pfds[1];
    w->eventStream = es;
    w->runLoop = NULL;
    pthread_mutex_init(&w->mut, NULL);
    pthread_mutex_lock(&w->mut);
    pthread_create(&t, NULL, &watchRunLoop, (void*)w);

    // Wait for watchRunLoop to release the mutex.
    // This way, we know it has finished calling FSEventStreamStart before we return.
    // If not, we'd have a race condition where filesystem events from just after the watch
    // creation could be missed.
    pthread_mutex_lock(&w->mut);
    pthread_mutex_unlock(&w->mut);

    *fd = pfds[0];
    *wp = w;
    rv = 0;
  } else {
    close(pfds[0]);
    close(pfds[1]);
    free(w);
    rv = -1;
  }
  for(i=0;i<n;i++) CFRelease(cffolders[i]);
  free(cffolders);
  CFRelease(paths);
  return rv;
}

void destroyWatch(watch* w) {
  pthread_mutex_lock(&w->mut);
  FSEventStreamStop(w->eventStream);
  FSEventStreamInvalidate(w->eventStream);
  CFRunLoopStop(w->runLoop);
  CFRelease(w->runLoop);
  FSEventStreamRelease(w->eventStream);
  close(w->writefd);
  pthread_mutex_unlock(&w->mut);
  pthread_mutex_destroy(&w->mut);
  free(w);
}

UInt64 getCurrentEventId() {
  return (UInt64)FSEventsGetCurrentEventId();
}

void osVersion(SInt32 *majorVersion, SInt32 *minorVersion, SInt32 *bugFixVersion) {
   Gestalt(gestaltSystemVersionMajor, majorVersion);
   Gestalt(gestaltSystemVersionMinor, minorVersion);
   Gestalt(gestaltSystemVersionBugFix, bugFixVersion);
}
