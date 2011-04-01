echo g++ -fPIC -g -Wall -c dar4pas.cc -o darpas_cc.o
g++ -fPIC -g -Wall -c dar4pas.cc -o darpas_cc.o
echo -----

echo gcc -fPIC -g -Wall -x c -c dar4pas.h -o dar4pas_c.o
gcc -fPIC -g -Wall -x c -c dar4pas.h -o dar4pas_c.o
echo -----

echo gcc -shared -Wl,-soname,libdargui.so.1 -o libdargui.so.1.0.1 dar4pas_c.o darpas_cc.o /usr/lib/libdar.so -lc 
gcc -shared -Wl,-soname,libdargui.so.1 -o libdargui.so.1.0.1 dar4pas_c.o darpas_cc.o /usr/lib/libdar.so -lc 
echo -----

echo done!; echo; echo

