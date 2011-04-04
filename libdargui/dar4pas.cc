#include "dar4pas.h"
#include <dar/archive.hpp>
#include <stdio.h>
#include <string.h>
using namespace std;
using namespace libdar;

extern "C"{

  
// ==============================================================================================
// get_dar_version
// ==============================================================================================
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

// ==============================================================================================
// get_dar_features
// ==============================================================================================
void EXPORTCALL get_dar_features(dar_features_t *feat){
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

// ==============================================================================================
// CALLBACKS
// ==============================================================================================
void message_cb(const string &x, void *context)   // listings and other information
{
  void (*cb)(const char*); 
  
   cb = ((dar_archive_t*)context)->listcallback;
   if (cb == NULL) {
		    printf("Error: message callback is NULL\n");
		    return;
		   }
   cb(x.c_str());

}

// ==============================================================================================
bool question_cb(const string & x, void *context) // requests true/false responses
{
    bool (*cb)(const char*);

   cb = ((dar_archive_t*)context)->questioncallback;
   if (cb == NULL) {
		    printf("Error: question callback is NULL\n");
		    return false;
		   }
   return cb(x.c_str());
}

// ==============================================================================================
string getstring_cb(const string &x, bool echo, void *context) // requests passwords
{
    char *(*cb)(const char*);
  
   cb = ((dar_archive_t*)context)->passwordcallback;
   if (cb == NULL) {
		    printf("Error: getstring callback is NULL\n");
		    return "";
		   }
   string pw(cb(((dar_archive_t*)context)->name)); 
   return pw;
}

// ==============================================================================================
void listing(const std::string & flag,  // used by dg_get_children_of to send listing
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
  U_16 code;
  string msg;
  void (*cb)(listing_output_t*); 
  
  cb = ((dar_archive_t*)context)->listlevelcallback;
  if (cb == NULL) {
		    printf("Error: listlevel callback is NULL\n");
		    return;
		   }
 
  listing_output_t *output = new listing_output_t;
      output->flag = libdar_str2charptr_noexcept(flag, code, msg);
      output->perm = libdar_str2charptr_noexcept(perm, code, msg);
      output->uid =  libdar_str2charptr_noexcept(uid, code, msg);
      output->gid =  libdar_str2charptr_noexcept(gid, code, msg);
      output->size = libdar_str2charptr_noexcept(size, code, msg);
      output->date = libdar_str2charptr_noexcept(date, code, msg);
      output->filename = libdar_str2charptr_noexcept(filename, code, msg);
      output->is_dir = is_dir;
      output->has_children = has_children;
      output->context = context;
  if(code != LIBDAR_NOEXCEPT)
    {
	printf("exception when listing\n");
	return;
    }
  
  cb(output);
  
  delete output;
  
}
 
 


// ==============================================================================================
// dg_open_archive
// ==============================================================================================
unsigned short int EXPORTCALL dg_open_archive(dar_archive_t *archiveinfo){

static user_interaction_callback ui = user_interaction_callback(message_cb, question_cb, getstring_cb, archiveinfo);
  U_16 code;
  string msg;
  U_32 blocksize = 0;
  crypto_algo encryption = crypto_none;
  string passphrase = "";
  
  if (archiveinfo->name == NULL) { return LIBDAR_ELIBCALL; }
  if (archiveinfo->directory == NULL) { return LIBDAR_ELIBCALL; }
  
  if (archiveinfo->password != NULL) { passphrase = archiveinfo->password; }

  if (archiveinfo->encrypted) { 
             blocksize = DEFAULT_CRYPTO_SIZE;
	     encryption = crypto_blowfish;
	 }
 
  archive *arch = open_archive_noexcept(ui,
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
	return code;
    }
  archiveinfo->arch = arch;
  return LIBDAR_NOEXCEPT;
}

// ==============================================================================================
// dg_list_archive
// ==============================================================================================
unsigned short int EXPORTCALL dg_list_archive(dar_archive_t *archiveinfo){

  static user_interaction_callback ui = user_interaction_callback(message_cb, question_cb, getstring_cb, archiveinfo);
  
  U_16 code;
  string msg;
  
  if (archiveinfo->arch == NULL) { return LIBDAR_ELIBCALL; }

  op_listing_noexcept(ui,
		        archiveinfo->arch,
		        archiveinfo->verbosity,
			(libdar::archive::listformat)archiveinfo->fmt,
			bool_mask(true),
			false,
			code,
			msg);
				    
  if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception listing: %S\n", &msg);
	return code;
    }
    
  return LIBDAR_NOEXCEPT;
}

// ==============================================================================================
// dg_close_archive
// ==============================================================================================
unsigned short int EXPORTCALL dg_close_archive(dar_archive_t *archiveinfo) {
  
  static user_interaction_callback ui = user_interaction_callback(message_cb, question_cb, getstring_cb, archiveinfo);
  
  U_16 code;
  string msg;
  
  if (archiveinfo->arch == NULL) { return LIBDAR_ELIBCALL; }

  close_archive_noexcept(archiveinfo->arch, code, msg);
    if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception closing: %S\n", &msg);
	return code;
    }
  archiveinfo->arch = NULL;  
  return LIBDAR_NOEXCEPT;
}


// ==============================================================================================
// dg_get_children_of
// ==============================================================================================
unsigned short int EXPORTCALL dg_get_children_of(dar_archive_t *archiveinfo, char *directory) {

  static user_interaction_callback ui = user_interaction_callback(message_cb, question_cb, getstring_cb, archiveinfo);
  ui.set_listing_callback(&listing);

  U_16 code;
  string msg;
  string dir;
  if (directory == NULL) { return LIBDAR_ELIBCALL; }
  else { dir = directory; }
  
  if (archiveinfo->arch == NULL) { return LIBDAR_ELIBCALL; }

  get_children_of_noexcept(ui,
			 archiveinfo->arch,
			 dir,
			 code,
			 msg);
  if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception listing: %S\n", &msg);
	return code;
    }
    
return LIBDAR_NOEXCEPT;
 }

}

