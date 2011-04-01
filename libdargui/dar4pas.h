#ifndef DAR4PAS_H
#define DAR4PAS_H

#ifdef __cplusplus
#include <dar/libdar.hpp>
#define EXPORTCALL __attribute__((stdcall))
#else
typedef int bool;
#define EXPORTCALL
#endif

#ifdef __cplusplus

extern "C"
{
#endif

typedef unsigned int U_Int;

typedef struct {
   bool ea;
   bool largefile;
   bool nodump;
   bool special_alloc;
   U_Int bits;
   bool thread_safe;
   bool libz;
   bool libbz2;
   bool libcrypto;
   bool new_blowfish;
} dar_features;

typedef struct {
   char  *name;
   char *directory;
   bool encrypted;
   char *password;
   void (*listcallback)(const char*); 
   bool (*questioncallback)(const char*);
} dar_archive;


extern unsigned int EXPORTCALL get_dar_version( U_Int *major,  U_Int *minor,  U_Int *sub);
extern void EXPORTCALL get_dar_features( dar_features *feat );

extern int EXPORTCALL test_call(char *txt, void (*fpcproc)(char * ));
//extern int EXPORTCALL libraryfunction(char *txt);

extern void EXPORTCALL list_archive(dar_archive *archiveinfo);

extern int EXPORTCALL libraryfunction(int numb);

#ifdef __cplusplus
}
#endif
#endif/*DAR4PAS_H*/
 
