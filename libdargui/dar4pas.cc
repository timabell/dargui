#include "dar4pas.h"
//#include "user_interaction.hpp"
#include <dar/archive.hpp>
using namespace std;
using namespace libdar;

extern "C"{

int EXPORTCALL test_call(char *txt, void (*fpcproc)(char * )){
 printf("test_callback (dar4pas.cc) %s\n---------------\n", txt);
 fpcproc(txt);
 printf("-- %s --\n","Completed test_callback");
 return 5;
}

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
    printf("[%d]%s\n", (U_I)context, x.c_str());
   // printf("%s", x.c_str());
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
printf("called getstring");
return "getstring result";
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

void EXPORTCALL list_archive(dar_archive *archiveinfo){
  
static user_interaction_callback ui = user_interaction_callback(warning2, question2, getstring2, archiveinfo);

  //libdar::user_interaction_callback dialog;
     U_16 code;
    string msg;
    

archive *arch = open_archive_noexcept(ui,
        archiveinfo->directory,  // where is the archive
        archiveinfo->name, // slice name
        "dar",   // dar's archive extensions
        crypto_none,
        "",
        0, // these three previous are for encryptions
        "",    // not used as we didn't gave "-" as
        "",    // slice name
        "",    // no command executed for now
        true,
	code,
	msg); // no verbose output

    if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception opening archive: %S\n", &msg);
	return;
    }

arch->op_listing(ui,
             true, //  verbose output
             arch->tree, // using the tar-like format
             bool_mask(true),
                    // all filenames are listed
             false);// do not filter unsaved files}

close_archive_noexcept(arch, code, msg);
    if(code != LIBDAR_NOEXCEPT)
    {
	ui.printf("exception closing: %S\n", &msg);
	return;
    }
}

extern int EXPORTCALL libraryfunction(int numb){
 return numb*numb; 
}

}
