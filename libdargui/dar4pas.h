#ifndef DAR4PAS_H
#define DAR4PAS_H

#ifdef __cplusplus
#include <dar/libdar.hpp>
#define EXPORTCALL __attribute__((stdcall))
typedef libdar::archive *archiveHandle;
#else
typedef int bool;
typedef struct dar_archive_t *archiveHandle;
#define EXPORTCALL
#endif

#define DEFAULT_CRYPTO_SIZE 10240

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
   char *name;
   char *directory;
   bool encrypted;
   char *password;
   void (*listcallback)(const char*); 
   bool (*questioncallback)(const char*);
   char *(*passwordcallback)(const char*);
} dar_archive;


extern unsigned int EXPORTCALL get_dar_version( U_Int *major,  U_Int *minor,  U_Int *sub);
extern void EXPORTCALL get_dar_features( dar_features *feat );

extern archiveHandle EXPORTCALL open_archive(dar_archive *archiveinfo);
extern unsigned short int EXPORTCALL list_archive(dar_archive *archiveinfo);

#ifdef __cplusplus
}
#endif
#endif/*DAR4PAS_H*/
 
