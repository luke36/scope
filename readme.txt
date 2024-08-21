Requires a C++ compiler supporting C++17 or higher.

Build and test (for example) by:
  $ cd minisat
  $ make -j4
  $ cd ..
  $ g++ -I minisat -L minisat/build/release/lib -O2 -o scope scope.cpp -lminisat
  $ ./scope let.lisp
