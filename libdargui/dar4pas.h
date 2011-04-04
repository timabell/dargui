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

enum listformat
	{
	    normal,   //< the tar-like listing (should be the default to use)
	    tree,     //< the original dar's tree listing (for those that like forest)
	    xml       //< the xml catalogue output
	};


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
} dar_features_t;

typedef struct {
   char *flag;
   char *perm;
   char *uid;
   char *gid;
   char *size;
   char *date;
   char *filename;
   bool is_dir;
   bool has_children;
   void *context;
} listing_output_t;

typedef struct {
   archiveHandle arch;
   char *name;
   char *directory;
   bool encrypted;
   char *password;
   int  fmt;
   void (*listcallback)(const char*); 
   bool (*questioncallback)(const char*);
   char *(*passwordcallback)(const char*);
   void (*listlevelcallback)(listing_output_t*); 
   bool verbosity;
} dar_archive_t;


extern unsigned int EXPORTCALL get_dar_version( U_Int *major,  U_Int *minor,  U_Int *sub);
extern void EXPORTCALL get_dar_features( dar_features_t *feat );

extern unsigned short int EXPORTCALL dg_open_archive(dar_archive_t *archiveinfo);
extern unsigned short int EXPORTCALL dg_list_archive(dar_archive_t *archiveinfo);
extern unsigned short int EXPORTCALL dg_close_archive(dar_archive_t *archiveinfo);
extern unsigned short int EXPORTCALL dg_get_children_of(dar_archive_t *archiveinfo, char *directory);

#ifdef __cplusplus
}
#endif
#endif/*DAR4PAS_H*/
 
