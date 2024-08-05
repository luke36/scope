Requires a C++ compiler supporting C++17 or higher.

Build and test (for example) by (I didn't make it to use the compiled
archive/dynamic library):
  $ cd minisat
  $ make -j4
  $ cd ..
  $ g++ -I minisat -O2 -o scope scope.cpp \
    minisat/build/release/minisat/core/Solver.o \
    minisat/build/release/minisat/simp/SimpSolver.o \
    minisat/build/release/minisat/utils/Options.o \
    minisat/build/release/minisat/utils/System.o
  $ ./scope < let.lisp 2>/dev/null
