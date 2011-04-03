#include "dar4pas.h"
#include <dar/archive.hpp>
#include <stdio.h>
#include <string.h>
using namespace std;
using namespace libdar;

extern "C"{

unsigned int EXPORTCALL get_dar_version(U_Int *major, U_Int *minor, U_Int *sub){
 U_I maj, med, min;
   try
   {
       get_version(maj, med, min);

      if(maj != libdar::LIBDAR_COMPILE_TIME_MAJOR ||
         med < libdar::LIBDAR_COMPILE_TIME_MEDIUM)
        throw libdar::Erange("initialization",
        "we are linking against a wrong libdar");
   }
   catch(libdar::Egeneric & e)
   {
      std::cout << e.get_message() << std::endl;
   }
   *major = maj;
   *minor = med;
   *sub = min;
return maj;
};

void EXPORTCALL get_dar_features(dar_features *feat){
  bool ea;
  bool largefile;
  bool nodump;
  bool special_alloc;
  U_I bits;
  bool thread_safe;
  bool libz;
  bool libbz2;
  bool libcrypto;
  bool new_blowfish;
 
  
  get_compile_time_features(ea,  largefile,  nodump,  special_alloc,  bits,
					   thread_safe,  libz,  libbz2,  libcrypto,  new_blowfish);
  feat->ea = ea;
  feat->largefile = largefile;
  feat->nodump = nodump;
  feat->special_alloc = special_alloc;
  feat->bits = bits;
  feat->thread_safe = thread_safe;
  feat->libz = libz;
  feat->libbz2 = libbz2;
  feat->libcrypto = libcrypto;
  feat->new_blowfish = new_blowfish;
};

void warning(const string &x, void *context)
{
    //printf("[%d]%s\n", (U_I)context, x.c_str());
    printf("%s", x.c_str());
}

void warning2(const string &x, void *context)
{
  void (*cb)(const char*); 
  
   cb = ((dar_archive*)context)->listcallback;
   cb(x.c_str());

}

bool question(const string & x, void *context)
{
    bool rep = false;
	    char r;

	    printf("[%d]%s\n", (U_I)context, x.c_str());
	    scanf("%c", &r);
	    rep = r == 'y';

    return rep;
}

bool question2(const string & x, void *context)
{
    bool (*cb)(const char*);

   cb = ((dar_archive*)context)->questioncallback;
   return cb(x.c_str());
}

string getstring(const string &x, bool echo, void *context)
{
    //throw SRC_BUG;
printf("called getstring");
return "getstring result";
}

string getstring2(const string &x, bool echo, void *context)
{
    //throw SRC_BUG;
printf("called getstring2\n");
return "test";
}

static user_interaction_callback listing_ui = user_interaction_callback(warning, question, getstring, (void *)1000);

void listing(const std::string & flag,
	     const std::string & perm,
	     const std::string & uid,
	     const std::string & gid,
	     const std::string & size,
	     const std::string & date,
	     const std::string & filename,
	     bool is_dir,
	     bool has_children,
	     void *context)
{
   listing_ui.printf("[[%d]][%S][%S][%S][%S][%S][%S][%S][%s][%s]\n", (U_I)context, &flag, &perm, &uid, &gid, &size, &date, &filename, is_dir ? "dir" : "not_dir", has_children ? "has children" : "no children");
}

archiveHandle EXPORTCALL open_archive(dar_archive *archiveinfo){
static user_interaction_callback ui = user_interaction_callback(warning, question2, getstring2, archiveinfo);

  U_32 blocksize = 0;
  crypto_algo encryption = crypto_none;
  string passphrase = "";
  
  if (archiveinfo->password != NULL) { passphrase = archiveinfo->password; }

    U_16 code;
    string msg;

    
static archiveHandle arch = open_archive_noexcept(ui,
        archiveinfo->directory,  // location of the archive
        archiveinfo->name, // slice name
        "dar",   // dar's archive extensions
        encryption,
        passphrase,
        blocksize, // these three previous are for encryptions
        "",    // not used as we didn't gave "-" as
        "",    // slice name
        "",    // no command executed for now
        false,  // verbose output
	code,
	msg);

if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception opening archive: %S\n", &msg);
	return NULL;
    }
  return arch;
}

unsigned short int EXPORTCALL list_archive(dar_archive *archiveinfo){

  static user_interaction_callback ui = user_interaction_callback(warning2, question2, getstring2, archiveinfo);
  
  U_32 blocksize = 0;
  crypto_algo encryption = crypto_none;
  string passphrase = "";
  
  if (archiveinfo->password != NULL) { passphrase = archiveinfo->password; }
  
  if (archiveinfo->encrypted) { 
             blocksize = DEFAULT_CRYPTO_SIZE;
	     encryption = crypto_blowfish;
	 }

    U_16 code;
    string msg;
    
 archiveHandle arch = open_archive_noexcept(ui,
        archiveinfo->directory,  // location of the archive
        archiveinfo->name, // slice name
        "dar",   // dar's archive extensions
        encryption,
        "",
        blocksize , // these three previous are for encryptions
        "",    // not used as we didn't gave "-" as
        "",    // slice name
        "",    // no command executed for now
        false,  // verbose output
	code,
	msg);
 
if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("%S\n", &msg);
	return code;
    }

op_listing_noexcept(ui,
				    arch,
				    true,
				    arch->tree,
				    bool_mask(true),
				    false,
				    code,
				    msg);
				    

				    
if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception listing: %S\n", &msg);
	return code;
    }
close_archive_noexcept(arch, code, msg);
    if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception closing: %S\n", &msg);
	return code;
    }
    
return LIBDAR_NOEXCEPT;
}

}

