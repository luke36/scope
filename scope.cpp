#include <cstdint>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

using std::string;
using std::vector;
using std::shared_ptr;

#define fail(x) (throw std::runtime_error(x))

// -------- language --------

typedef uint64_t Id;
typedef uint PortId;

struct Function;
typedef shared_ptr<Function> FunctionP;
typedef Function *FunctionWP;

struct Sort {
  Id id;
  string name;
  vector<FunctionWP> funcs;
  PortId n_import;
  PortId n_export;
  Sort(Id id, string &&name, PortId n_import, PortId n_export):
    id(id), name(std::move(name)), n_import(n_import), n_export(n_export) {}
};

typedef shared_ptr<Sort> SortP;
typedef Sort *SortWP;

struct Port;
typedef vector<Port> PortSet;

struct Parameter {
  Id id;
  string name;
  SortWP sort;
  FunctionWP mom;
  vector<PortSet> binds;
  Parameter(Id id, string &&name, SortWP sort):
    id(id), name(std::move(name)), sort(sort), mom(nullptr) {}
};

typedef Parameter *ParameterP;

struct Port {
  ParameterP param;
  PortId port;
  Port(ParameterP param, PortId x): param(param), port(x) {}
  void show(std::ostream &os) {
    os << "(" << param->name << " " << port << ")";
  }
  bool operator<(const Port &other) const {
    return param < other.param || (param == other.param && port < other.port);
  }
  bool operator==(const Port &other) const {
    return param == other.param && port == other.port;
  }
};

struct Function {
  Id id;
  string name;
  SortWP sort;
  vector<Parameter> params;
  uint8_t scoped;
  vector<PortSet> imports;
  vector<PortSet> exports;
  Function(Id id_, string &&name_, SortWP sort_, vector<Parameter> &&params_):
    id(id_), name(std::move(name_)), sort(sort_), params(std::move(params_)),
    scoped(0) {
    sort->funcs.emplace_back(this);
    for (auto &p : params) {
      p.mom = this;
    }
  }
  ParameterP findParameter(const string &name) {
    for (auto &pi : params) {
      if (name == pi.name) {
        return &pi;
      }
    }
    fail("findParameter: no such parameter");
  }
};

struct Expr;
struct Application;
struct Hole;
typedef shared_ptr<Expr> ExprP;
typedef Expr *ExprWP;
typedef Application *ApplicationP;
typedef Hole *HoleP;

struct Expr {
  ApplicationP mom;
  ParameterP self_param;
  size_t depth;
  Expr(): mom(nullptr), self_param(nullptr) {}
  virtual void show(std::ostream &os) const = 0;
};

struct Var: public Expr {
  string name;
  Var(string &&name): Expr(), name(std::move(name)) {}
  virtual void show(std::ostream &os) const override {
    os << name;
  }
};

typedef Var *VarP;

struct Hole: public Expr {
  Id id;
  Hole(Id id): Expr(), id(id) {}
  virtual void show(std::ostream &os) const override {
    os << id;
  }
};

struct DefToUse: public Expr {
  Id hole;
  DefToUse(Id hole): Expr(), hole(hole) {}
  virtual void show(std::ostream &os) const override {
    os << "(->use " << hole << ")";
  }
};

typedef DefToUse *DefToUseP;

struct Application: public Expr {
  FunctionWP func;
  vector<ExprP> args;
  Application(FunctionWP func_, vector<ExprP> &&args_):
    Expr(),func(func_), args(std::move(args_)) {
    for (size_t i = 0; i < args.size(); i++) {
      args[i]->mom = this;
      args[i]->self_param = &func->params[i];
    }
  }
  virtual void show(std::ostream &os) const override {
    os << '(' << func->name;
    for (auto e : args) {
      os << ' ';
      e->show(os);
    }
    os << ')';
  }
};

inline ExprP mkVar(string &&name) {
  Var *p = new Var(std::move(name));
  return shared_ptr<Expr>(p);
}

inline ExprP mkHole(Id id) {
  Hole *p = new Hole(id);
  return shared_ptr<Expr>(p);
}

inline ExprP mkFunc(FunctionWP func, vector<ExprP> &&args) {
  Application *p = new Application(func, std::move(args));
  return shared_ptr<Expr>(p);
}

inline ExprP mkDefToUse(Id hole) {
  DefToUse *p = new DefToUse(hole);
  return shared_ptr<Expr>(p);
}

void typecheck(ExprP e, const std::map<Id, HoleP> &holes) {
  if (auto var = dynamic_cast<VarP>(e.get()); var != nullptr) {
    auto sort = e->self_param->sort;
    if (sort->name != "VarDef" && sort->name != "VarUse") {
      fail("typecheck: sort error");
    }
  } else if (auto d2u = dynamic_cast<DefToUseP>(e.get()); d2u != nullptr) {
    if (auto it = holes.find(d2u->hole); it != holes.end()) {
      if (it->second->self_param->sort->name != "VarDef") {
        fail("typecheck: not a definitional hole");
      }
    } else {
      fail("typecheck: no such hole");
    }
  } else if (auto ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
    if (ap->self_param != nullptr &&
        ap->func->sort != ap->self_param->sort) {
      fail("typecheck: sort error");
    }
    for (auto e0 : ap->args) {
      typecheck(e0, holes);
    }
  }
}

struct Macro {
  ExprP from;
  ExprP to;
  std::map<Id, HoleP> from_holes;
  std::map<Id, HoleP> to_holes;

  void locateHoles(ExprP e, std::map<Id, HoleP> &m) {
    if (ApplicationP ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
      for (auto e0 : ap->args) {
        locateHoles(e0, m);
      }
    } else if (HoleP hp = dynamic_cast<HoleP>(e.get()); hp != nullptr) {
      m[hp->id] = hp;
    }
  }

  Macro(ExprP from, ExprP to): from(from), to(to) {
    locateHoles(from, from_holes);
    locateHoles(to, to_holes);
  }
};

void checkMacro(const Macro &m) {
  typecheck(m.from, std::map<Id, HoleP>());
  typecheck(m.to, m.to_holes);
  ApplicationP ap;
  if (ap = dynamic_cast<ApplicationP>(m.from.get()); ap == nullptr) {
    fail("checkMacro: bad LHS");
  }
  for (auto &p : m.to_holes) {
    if (auto it = m.from_holes.find(p.first); it != m.from_holes.end()) {
      if (p.second->self_param == nullptr) {
        if (it->second->self_param->sort != ap->func->sort) {
          fail("checkMacro: sort error");
        }
      } else if (p.second->self_param->sort != it->second->self_param->sort) {
        fail("checkMacro: sort error");
      }
    } else {
      fail("checkMacro: invented hole");
    }
  }
}

// -------- global environment --------

struct TextPort {
  string param;
  PortId port;
  TextPort(string &&param, PortId port):
    param(std::move(param)), port(port) {}
};

typedef vector<TextPort> TextPortSet;

struct Environment {
  Id next_id;
  // two predefined sorts:
  // VarDef: 1 import & 1 export
  // VarUse: 1 import & 0 export
  std::map<string, SortP> sorts;
  std::map<string, FunctionP> funcs;
  vector<Macro> pending_macro;

  std::optional<SortWP> findSort(const string &s) {
    auto x = sorts.find(s);
    if (x == sorts.end()) {
      return {};
    } else {
      return x->second.get();
    }
  }

  std::optional<FunctionWP> findFunction(const string &s) {
    auto x = funcs.find(s);
    if (x == funcs.end()) {
      return {};
    } else {
      return x->second.get();
    }
  }

  SortWP findSortException(const string &s) {
    auto &&x = findSort(s);
    if (x) {
      return x.value();
    } else {
      fail("findSortException: unknown sort");
    }
  }

  FunctionWP findFunctionException(const string &s) {
    auto &&x = findFunction(s);
    if (x) {
      return x.value();
    } else {
      fail("findFunctionException: unknown function");
    }
  }

  Id fresh() {
    return next_id++;
  }

  bool addFunction(string &&name, const string &sort,
                   vector<std::pair<string, string>> &&param) {
    vector<Parameter> params;
    for (size_t i = 0; i < param.size(); i++) {
      params.emplace_back(fresh(), std::move(param[i].first),
                          findSortException(param[i].second));
    }
    string name1 = name;
    auto p = funcs.emplace(name1,
                           shared_ptr<Function>
                           (new Function(fresh(), std::move(name),
                                         findSortException(sort),
                                         std::move(params))));
    return p.second;
  }

  bool addSort(string &&name, PortId n_import, PortId n_export) {
    string name1 = name;
    auto p = sorts.emplace(name1,
                           shared_ptr<Sort>
                           (new Sort(fresh(), std::move(name),
                                     n_import, n_export)));
    return p.second;
  }

  void addScope(const string &name,
                const vector<TextPortSet> &imports,
                const vector<TextPortSet> &exports,
                const vector<std::pair<TextPort, TextPort>> &binds) {
    auto f = findFunctionException(name);
    if (f->scoped) {
      fail("addScope: already specified scope");
    }
    // clear from last exception
    f->imports = vector<PortSet>(f->sort->n_import);
    f->exports = vector<PortSet>(f->sort->n_export);
    for (auto &p : f->params) {
      p.binds = vector<PortSet>(p.sort->n_import);
    }

    for (size_t i = 0; i < imports.size(); i++) {
      if (i >= f->sort->n_import) {
        fail("addScope: invalid import");
      }
      for (auto &p : imports[i]) {
        auto param = f->findParameter(p.param);
        if (p.port >= param->sort->n_import) {
          fail("addScope: invalid import");
        }
        f->imports[i].emplace_back(param, p.port);
      }
    }
    for (size_t i = 0; i < exports.size(); i++) {
      if (i >= f->sort->n_export) {
        fail("addScope: invalid export");
      }
      for (auto &p : exports[i]) {
        auto param = f->findParameter(p.param);
        if (p.port >= param->sort->n_export) {
          fail("addScope: invalid export");
        }
        f->exports[i].emplace_back(param, p.port);
      }
    }
    for (auto &p : binds) {
      auto from = f->findParameter(p.first.param);
      auto to = f->findParameter(p.second.param);
      if (p.first.port >= from->sort->n_import ||
          p.second.port >= to->sort->n_export) {
        fail("addScope: invalid bind");
      }
      from->binds[p.first.port].emplace_back(to, p.second.port);
    }
    f->scoped = 1;
  }

  void dump(std::ostream &os) {
    for (auto &s : sorts) {
      os << "(defsort "  << s.second->name
         << " :import " << s.second->n_import
         << " :export " << s.second->n_export << ")" << std::endl;
    }
    for (auto &f : funcs) {
      os << "(defun " << f.second->name;
      for (auto &p : f.second->params) {
        os << " (: " << p.name << " " << p.sort->name << ")";
      }
      os << " :sort " << f.second->sort->name << ")" << std::endl;
    }
    for (auto &f : funcs) {
      if (f.second->scoped) {
        os << "(defscope " << f.second->name;
        if (!f.second->imports.empty()) {
          os << " :import";
        }
        for (auto &s : f.second->imports) {
          os << " (";
          bool first = true;
          for (auto &port : s) {
            if (!first) {
              os << " ";
            }
            first = false;
            port.show(os);
          }
          os << ")";
        }
        if (!f.second->exports.empty()) {
          os << " :export";
        }
        for (auto &s : f.second->exports) {
          os << " (";
          bool first = true;
          for (auto &port : s) {
            if (!first) {
              os << " ";
            }
            first = false;
            port.show(os);
          }
          os << ")";
        }
        bool first = true;
        for (auto &p : f.second->params) {
          for (size_t i = 0; i < p.binds.size(); i++) {
            for (auto &port : p.binds[i]) {
              if (first) {
                os << " :bind";
                first = false;
              }
              os << " (";
              Port(&p, i).show(os);
              os << " ";
              port.show(os);
              os << ")";
            }
          }
        }
        os << ")" << std::endl;
      }
    }
    for (auto &m : pending_macro) {
      os << "(defmacro ";
      m.from->show(os);
      os << " ";
      m.to->show(os);
      os << ")" << std::endl;
    }
  }

  Environment() {
    addSort("VarUse", 1, 0);
    addSort("VarDef", 1, 1);
  }
} env;

// -------- reader --------

char getcharSkip(std::istream &is) {
  char c;
  do {
    c = is.get();
    if (!is.good()) {
      fail("getcharSkip: eof");
    }
  } while (isspace(c));
  return c;
}

string readString(std::istream &is) {
  char c;
  string s;
  c = getcharSkip(is);
  if (c == '(' || c == ')') {
    fail("readString: parenthesis in wrong position");
  } else {
    do {
      s += c;
      c = is.get();
    } while (is.good() && c != '(' && c != ')' && !isspace(c));
    if (is.good()) {
      is.putback(c);
    }
  }
  return s;
}

void readExactChar(std::istream &is, char c) {
  if (getcharSkip(is) != c) {
    fail("readExactChar: bad syntax");
  }
}

ExprP readExpr(std::istream &is) {
  auto c = getcharSkip(is);

  switch (c) {
  case '(': {
    string &&func = readString(is);
    if (func == "->use") {
      string &&s = readString(is);
      size_t pos;
      try {
        Id id = std::stoul(s, &pos);
        if (pos == s.size()) {
          readExactChar(is, ')');
          return mkDefToUse(id);
        } else {
          fail("readExpr: expecting hole");
        }
      } catch (...) {
        fail("readExpr: expecting hole");
      }
    } else {
      vector<ExprP> args;
      while ((c = getcharSkip(is)) != ')') {
        is.putback(c);
        args.emplace_back(readExpr(is));
      }
      return mkFunc(env.findFunctionException(func), std::move(args));
    }
    break;
  }
  default: {
    is.putback(c);
    string &&s = readString(is);
    size_t pos;
    try {
      Id id = std::stoul(s, &pos);
      if (pos == s.size()) {
        return mkHole(id);
      } else {
        return mkVar(std::move(s));
      }
    } catch (...) {
      return mkVar(std::move(s));
    }
    break;
  }
  }
}

uint readUInt(std::istream &is) {
  string &&x = readString(is);
  size_t pos;
  try {
    long n = std::stoul(x, &pos);
    if (pos == x.size() && n >= 0) {
      return static_cast<uint>(n);
    } else {
      fail("readUInt: not a positive number");
    }
  } catch (...) {
    fail("readUInt: not a positive number");
  }
}

// -------- REPL --------

// (defsort Expr :import 1 :export 0)
// (defun lambda (: x VarDef) (: body Expr) :sort Expr)
// (defscope :function lambda :import ((x 0) (body 0)) :bind ((body 0) (x 0)))
// (defmacro (let (more-binds 1 2 3) 4) (app (lambda 1 (let 3 4)) 2))
// (infer)
// (resolve (let (more-binds x (var x) end-binds) (var x)))
// (dump)
// (regexp lambda let)
// (exit)

void readExactString(std::istream &is, const string &&s) {
  if (readString(is) != s) {
    fail("readExactString: bad syntax");
  }
}

void readDefsort(std::istream &is) {
  string &&name = readString(is);
  PortId n_import = 0;
  PortId n_export = 0;
  char c;
  while ((c = getcharSkip(is)) != ')') {
    is.putback(c);
    string &&which = readString(is);
    if (which == ":import") {
      n_import = readUInt(is);
    } else if (which == ":export") {
      n_export = readUInt(is);
    } else {
      fail("readDefsort: bad syntax");
    }
  }
  // for uniform treatment in repl()
  is.putback(c);
  if (!env.addSort(std::move(name), n_import, n_export)) {
    fail("readDefsort: redefine sort");
  }
}

void readDefun(std::istream &is) {
  string &&name = readString(is);
  char c;
  vector<std::pair<string, string>> param;
  while ((c = getcharSkip(is)) == '(') {
    readExactChar(is, ':');
    string &&name = readString(is);
    string &&sort = readString(is);
    param.emplace_back(std::move(name), std::move(sort));
    readExactChar(is, ')');
  }
  is.putback(c);
  readExactString(is, ":sort");
  string &&sort = readString(is);
  if (!env.addFunction(std::move(name), sort, std::move(param))) {
    fail("readDefun: redefine function");
  }
}

TextPort readPort(std::istream &is) {
  readExactChar(is, '(');
  string &&name = readString(is);
  PortId port = readUInt(is);
  readExactChar(is, ')');
  return TextPort(std::move(name), port);
}

void readDefscope(std::istream &is) {
  vector<TextPortSet> imports;
  vector<TextPortSet> exports;
  vector<std::pair<TextPort, TextPort>> binds;
  string name = readString(is);
  char c;
  while ((c = getcharSkip(is)) != ')') {
    is.putback(c);
    string &&which = readString(is);
    if (which == ":import") {
      while ((c = getcharSkip(is)) == '(') {
        imports.emplace_back();
        while ((c = getcharSkip(is)) == '(') {
          is.putback(c);
          imports.back().emplace_back(readPort(is));
        }
        is.putback(c);
        readExactChar(is, ')');
      }
      is.putback(c);
    } else if (which == ":export") {
      while ((c = getcharSkip(is)) == '(') {
        exports.emplace_back();
        while ((c = getcharSkip(is)) == '(') {
          is.putback(c);
          exports.back().emplace_back(readPort(is));
        }
        is.putback(c);
        readExactChar(is, ')');
      }
      is.putback(c);
    } else if (which == ":bind") {
      while ((c = getcharSkip(is)) == '(') {
        TextPort &&from = readPort(is);
        TextPort &&to = readPort(is);
        binds.emplace_back(std::move(from), std::move(to));
        readExactChar(is, ')');
      }
      is.putback(c);
    } else {
      fail("readDefscope: bad syntax");
    }
  }
  is.putback(c);
  env.addScope(name, imports, exports, binds);
}

void readDefmacro(std::istream &is) {
  auto from = readExpr(is);
  auto to = readExpr(is);
  auto m = Macro(from, to);
  checkMacro(m);
  env.pending_macro.emplace_back(std::move(m));
}

void doInfer();

void readInfer(std::istream &_) {
  doInfer();
}

void readResolve(std::istream &is, std::function<void (ExprP)> k) {
  auto e = readExpr(is);
  k(e);
}

struct RegExp {
  virtual void show(std::ostream &os) const = 0;
  virtual bool equal(const RegExp &other) const = 0;
};

typedef shared_ptr<RegExp> RegExpP;

std::pair<RegExpP, RegExpP> characterize(vector<FunctionWP> &fs);

void readRegexp(std::istream &is) {
  vector<FunctionWP> fs;
  char c;
  while ((c = getcharSkip(is)) != ')') {
    is.putback(c);
    string &&func = readString(is);
    fs.emplace_back(env.findFunctionException(func));
  }
  auto &&ud = characterize(fs);
  ud.first->show(std::cout);
  std::cout << std::endl;
  ud.second->show(std::cout);
  std::cout << std::endl;
  is.putback(c);
}

bool repl(std::istream &is, bool quiet) {
  while (true) {
    if (!quiet) {
      std::cout << "> ";
    }
    try {
      char c = getcharSkip(is);
      if (c == ';') {
        while (is.good() && is.get() != '\n')
          ;
        if (!is.good()) {
          if (!quiet) {
            std::cout << "exit.";
          }
          return true;
        }
        continue;
      } else if (c != '(') {
        fail("repl: bad syntax");
      }
      string &&form = readString(is);
      if (form == "defsort") {
        readDefsort(is);
      } else if (form == "defun") {
        readDefun(is);
      } else if (form == "defmacro") {
        readDefmacro(is);
      } else if (form == "resolve") {
        readResolve(is, [](ExprP _){});
      } else if (form == "defscope") {
        readDefscope(is);
      } else if (form == "infer") {
        readInfer(is);
      } else if (form == "dump") {
        env.dump(std::cout);
      } else if (form == "regexp") {
        readRegexp(is);
      } else if (form == "exit") {
        return false;
      } else {
        fail("repl: bad form");
      }
      readExactChar(is, ')');
    } catch (std::runtime_error &err) {
      if (is.good()) {
        std::cerr << "\e[1;31merror:\e[0m " << err.what() << std::endl;
      }
      while (is.good() && is.get() != '\n')
        ;
      if (!is.good()) {
        if (!quiet) {
          std::cout << "exit.";
        }
        return true;
      }
    }
  }
}

// -------- infer --------

enum class Polar { Import, Export };

struct PortLoc {
  ExprWP e;
  Polar polar;
  PortId port;
  bool operator<(const PortLoc &other) const {
    return e < other.e ||
           (e == other.e &&
            (port < other.port ||
             (port == other.port &&
              polar < other.polar)));
  }
  bool operator==(const PortLoc &other) const {
    return e == other.e && polar == other.polar && port == other.port;
  }
  PortLoc(ExprWP e, Polar polar, PortId port):
    e(e), polar(polar), port(port) {}
};

ApplicationP lca(ExprWP e1, ExprWP e2) {
  size_t d1 = e1->depth;
  size_t d2 = e2->depth;
  if (d1 > d2) {
    auto t1 = e1;
    e1 = e2;
    e2 = t1;
    auto t2 = d1;
    d1 = d2;
    d2 = t2;
  }
  for (; d2 > d1; d2--) {
    e2 = e2->mom;
  }
  while (e1 != e2) {
    e1 = e1->mom;
    e2 = e2->mom;
  }
  return dynamic_cast<ApplicationP>(e1);
}

struct PortPath {
  PortLoc from;
  PortLoc to;
  ApplicationP lca;
  bool operator<(const PortPath &other) const {
    return from < other.from || (from == other.from && to < other.to);
  }
  PortPath(const PortLoc &from, const PortLoc &to, ApplicationP lca):
    from(from), to(to), lca(lca) {}
  PortPath(const PortLoc &from, const PortLoc &to):
    from(from), to(to) {
    lca = ::lca(from.e, to.e);
  }
};

enum class RuleType { Import, Export, Bind };

struct Rule {
  RuleType type;
  // could be more space-efficient
  FunctionWP func;
  PortId port;
  Port from;
  Port to;
  bool operator<(const Rule &other) const {
    return type < other.type ||
           (type == other.type &&
            (func < other.func ||
             (func == other.func &&
              (port < other.port ||
               (port == other.port &&
                (from < other.from ||
                 (from == other.from &&
                  to < other.to)))))));
  }
  Rule(RuleType type, FunctionWP func, PortId port, Port &&from, Port &&to):
    type(type), func(func), port(port), from(std::move(from)), to(std::move(to)) {}
};

Rule Import(Port &&from, FunctionWP func, PortId to) {
  auto from1 = from;
  return Rule(RuleType::Import, func, to, std::move(from1), std::move(from));
}

Rule Export(FunctionWP func, PortId from, Port &&to) {
  auto to1 = to;
  return Rule(RuleType::Export, func, from, std::move(to1), std::move(to));
}

Rule Bind(Port &&from, Port &&to) {
  return Rule(RuleType::Bind, from.param->mom, 0, std::move(from), std::move(to));
}

#include "minisat/core/Solver.h"

using Minisat::mkLit;

std::map<Rule, Minisat::Var> rule_var;
std::map<PortPath, Minisat::Var> path_var;
Minisat::Solver *solver;

bool hasPort(const PortSet &s, const Port &p) {
  for (auto &q : s) {
    if (p == q) {
      return true;
    }
  }
  return false;
}

Minisat::Var ruleToVar(const Rule &r) {
  auto it = rule_var.find(r);
  if (it != rule_var.end()) {
    return it->second;
  } else {
    auto var = solver->newVar();
    if (r.func->scoped) {
      switch (r.type) {
      case RuleType::Import:
        if (hasPort(r.func->imports[r.port], r.from)) {
          solver->addClause(mkLit(var, false));
        } else {
          solver->addClause(mkLit(var, true));
        }
        break;
      case RuleType::Export:
        if (hasPort(r.func->exports[r.port], r.to)) {
          solver->addClause(mkLit(var, false));
        } else {
          solver->addClause(mkLit(var, true));
        }
        break;
      case RuleType::Bind:
        if (hasPort(r.from.param->binds[r.from.port], r.to)) {
          solver->addClause(mkLit(var, false));
        } else {
          solver->addClause(mkLit(var, true));
        }
        break;
      }
    }
    rule_var[r] = var;
    return var;
  }
}

Port portLocToPort(const PortLoc &pl) {
  return Port(pl.e->self_param, pl.port);
}

// p iff q11 /\ q12 \/ q21 /\ q22 \/ ...
void addIff(Minisat::Var p,
            const vector<std::pair<Minisat::Var, Minisat::Var>> &q) {
  for (auto &q1q2 : q) {
    // a /\ b \/ c /\ d -> e
    // a /\ b -> e, c /\ d -> e
    // ~a \/ ~b \/ e
    solver->addClause(mkLit(p, false),
                      mkLit(q1q2.first, true),
                      mkLit(q1q2.second, true));
  }
  // e -> a /\ b \/ c /\ d
  // ~e \/ a/\b \/ c/\d
  Minisat::vec<Minisat::Lit> defs;
  for (auto &q1q2 : q) {
    // x <-> a/\b
    // x -> a/\b | ~x \/ a/\b | ~x\/a /\ ~x\/b
    // a/\b -> x
    // ~a \/ ~b \/ x
    auto q1 = q1q2.first, q2 = q1q2.second;
    auto x = solver->newVar();
    solver->addClause(mkLit(x, true), mkLit(q1, false));
    solver->addClause(mkLit(x, true), mkLit(q2, false));
    solver->addClause(mkLit(x, false), mkLit(q1, true), mkLit(q2, true));
    defs.push(mkLit(x, false));
  }
  defs.push(mkLit(p, true));
  solver->addClause(defs);
}

void addIff(Minisat::Var p, Minisat::Var q) {
  // a <-> b
  // ~a\/b /\ ~b\/a
  solver->addClause(mkLit(p, false), mkLit(q, true));
  solver->addClause(mkLit(q, false), mkLit(p, true));
}

void addIff(Minisat::Var p, const vector<Minisat::Lit> &ls) {
  Minisat::Lit q;
  if (ls.empty()) {
    solver->addClause(mkLit(p, false));
    return;
  } else {
    q = ls[0];
    for (size_t i = 1; i < ls.size(); i++) {
      Minisat::Var x = solver->newVar();
      solver->addClause(mkLit(x, true), q);
      solver->addClause(mkLit(x, true), ls[i]);
      solver->addClause(mkLit(x, false), ~q, ~ls[i]);
      q = mkLit(x, false);
    }
    solver->addClause(mkLit(p, false), ~q);
    solver->addClause(mkLit(p, true), q);
  }
}

Minisat::Var analyzePath(const PortPath &p) {
  Minisat::Var var;
  auto it = path_var.find(p);
  if (it != path_var.end()) {
    return it->second;
  } else {
    var = path_var[p] = solver->newVar();
  }
  const PortLoc &from = p.from;
  const PortLoc &to = p.to;
  if (from.polar == Polar::Import && to.polar == Polar::Import) {
    if (from.e->mom == to.e) {
      auto func = from.e->mom->func;
      addIff(var, ruleToVar(Import(portLocToPort(from), func, to.port)));
      return var;
    } else {
      goto from_up;
    }
  } else if (from.polar == Polar::Export && to.polar == Polar::Export) {
    if (to.e->mom == from.e) {
      auto func = to.e->mom->func;
      addIff(var, ruleToVar(Export(func, from.port, portLocToPort(to))));
      return var;
    } else {
      goto to_up;
    }
  } else if (from.polar == Polar::Import && to.polar == Polar::Export) {
    if (to.e->mom == p.lca && from.e->mom == p.lca) {
      addIff(var, ruleToVar(Bind(portLocToPort(from), portLocToPort(to))));
      return var;
    } else if (from.e->mom == p.lca) {
      goto to_up;
    } else {
      goto from_up;
    }
  } else {
    fail("analyzePath: bad path");
  }

 from_up: {
    ApplicationP higher = from.e->mom;
    FunctionWP func = higher->func;
    vector<std::pair<Minisat::Var, Minisat::Var>> q;
    for (PortId i = 0; i < func->sort->n_import; i++) {
      auto var1 = analyzePath(PortPath(PortLoc(higher, Polar::Import, i),
                                       to, p.lca));
      auto var2 = ruleToVar(Import(portLocToPort(from), func, i));
      q.emplace_back(var1, var2);
    }
    addIff(var, q);
    return var;
  }
 to_up: {
    ApplicationP higher = to.e->mom;
    FunctionWP func = higher->func;
    vector<std::pair<Minisat::Var, Minisat::Var>> q;
    for (PortId i = 0; i < func->sort->n_export; i++) {
      auto var1 = analyzePath(PortPath(from,
                                       PortLoc(higher, Polar::Export, i), p.lca));
      auto var2 = ruleToVar(Export(func, i, portLocToPort(to)));
      q.emplace_back(var1, var2);
    }
    addIff(var, q);
    return var;
  }
}

void computeDepth(ExprP e, size_t d) {
  e->depth = d;
  if (ApplicationP ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
    for (auto e0 : ap->args) {
      computeDepth(e0, d + 1);
    }
  }
}

void findFreshDef(ExprP e, std::map<string, ExprP> &fdef) {
  if (ApplicationP ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
    for (auto e0 : ap->args) {
      findFreshDef(e0, fdef);
    }
  } else if (VarP var = dynamic_cast<VarP>(e.get());
             var != nullptr && var->self_param->sort->name == "VarDef") {
    if (fdef.find(var->name) != fdef.end()) {
      fail("findFreshDef: fresh identifiers with same name");
    }
    fdef[var->name] = e;
  }
}

void findFreshUse(ExprP e, std::map<string, ExprP> &fdef) {
  if (ApplicationP ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
    for (auto e0 : ap->args) {
      findFreshUse(e0, fdef);
    }
  } else if (VarP var = dynamic_cast<VarP>(e.get());
             var != nullptr && var->self_param->sort->name == "VarUse") {
    if (auto it = fdef.find(var->name); it != fdef.end()) {
      auto from1 = PortLoc(e.get(), Polar::Import, 0);
      auto to1 = PortLoc(it->second.get(), Polar::Export, 0);
      auto p = analyzePath(PortPath(from1, to1));
      solver->addClause(mkLit(p, false));
    } else {
      fail("findFreshUse: unbound identifier in rhs");
    }
  }
}

void findDefToUse(ExprP e, vector<DefToUseP> &d2u) {
  if (ApplicationP ap = dynamic_cast<ApplicationP>(e.get()); ap != nullptr) {
    for (auto e0 : ap->args) {
      findDefToUse(e0, d2u);
    }
  } else if (DefToUseP du = dynamic_cast<DefToUseP>(e.get()); du != nullptr) {
    d2u.emplace_back(du);
  }
}

void generateConstraint(const Macro &macro) {
  auto &hs1 = macro.from_holes,
       &hs2 = macro.to_holes;
  computeDepth(macro.from, 0);
  computeDepth(macro.to, 0);
  ApplicationP ap1;
  ApplicationP ap2;
  if (ap1 = dynamic_cast<ApplicationP>(macro.from.get()); ap1 == nullptr) {
    fail("generateConstraint: bad macro");
  }
  auto func = ap1->func;
  // discarded should not bind anything
  for (auto &h1 : hs1) {
    for (auto &g1 : hs1) {
      if (h1.first != g1.first &&
          hs2.find(g1.first) == hs2.end() &&
          hs2.find(h1.first) != hs2.end()) {
        for (PortId i = 0; i < h1.second->self_param->sort->n_import; i++) {
          for (PortId j = 0; j < g1.second->self_param->sort->n_export; j++) {
            auto from1 = PortLoc(h1.second, Polar::Import, i);
            auto to1 = PortLoc(g1.second, Polar::Export, j);
            auto p = analyzePath(PortPath(from1, to1));
            solver->addClause(mkLit(p, true));
          }
        }
      }
    }
  }
  if (ap2 = dynamic_cast<ApplicationP>(macro.to.get()); ap2 != nullptr) {
    std::map<Id, HoleP>::const_iterator h2, g2;
    // import
    for (auto &h1 : hs1) {
      if ((h2 = hs2.find(h1.first)) != hs2.end()) {
        for (PortId i = 0; i < h1.second->self_param->sort->n_import; i++) {
          for (PortId j = 0; j < func->sort->n_import; j++) {
            auto from1 = PortLoc(h1.second, Polar::Import, i);
            auto to1 = PortLoc(ap1, Polar::Import, j);
            auto p = analyzePath(PortPath(from1, to1, ap1));
            auto from2 = PortLoc(h2->second, Polar::Import, i);
            auto to2 = PortLoc(ap2, Polar::Import, j);
            auto q = analyzePath(PortPath(from2, to2, ap2));
            addIff(p, q);
          }
        }
      }
    }
    // export
    for (auto &h1 : hs1) {
      if ((h2 = hs2.find(h1.first)) != hs2.end()) {
        for (PortId i = 0; i < h1.second->self_param->sort->n_export; i++) {
          for (PortId j = 0; j < func->sort->n_export; j++) {
            auto from1 = PortLoc(ap1, Polar::Export, j);
            auto to1 = PortLoc(h1.second, Polar::Export, i);
            auto p = analyzePath(PortPath(from1, to1, ap1));
            auto from2 = PortLoc(ap2, Polar::Export, j);
            auto to2 = PortLoc(h2->second, Polar::Export, i);
            auto q = analyzePath(PortPath(from2, to2, ap2));
            addIff(p, q);
          }
        }
      }
    }
    // bind
    for (auto &h1 : hs1) {
      for (auto &g1 : hs1) {
        if (h1.first != g1.first &&
            (h2 = hs2.find(h1.first)) != hs2.end() &&
            (g2 = hs2.find(g1.first)) != hs2.end()) {
          for (PortId i = 0; i < h1.second->self_param->sort->n_import; i++) {
            for (PortId j = 0; j < g1.second->self_param->sort->n_export; j++) {
              auto from1 = PortLoc(h1.second, Polar::Import, i);
              auto to1 = PortLoc(g1.second, Polar::Export, j);
              auto p = analyzePath(PortPath(from1, to1));
              auto from2 = PortLoc(h2->second, Polar::Import, i);
              auto to2 = PortLoc(g2->second, Polar::Export, j);
              auto q = analyzePath(PortPath(from2, to2));
              addIff(p, q);
            }
          }
        }
      }
    }
    // fresh in rhs should be uniquely bound
    std::map<string, ExprP> fdef;
    findFreshDef(macro.to, fdef);
    findFreshUse(macro.to, fdef);
    // ->use should be uniquely bound
    vector<DefToUseP> d2us;
    findDefToUse(macro.to, d2us);
    for (auto d2u : d2us) {
      for (auto &h : hs2) {
        if (h.first == d2u->hole) {
          auto from1 = PortLoc(d2u, Polar::Import, 0);
          auto to1 = PortLoc(h.second, Polar::Export, 0);
          auto p = analyzePath(PortPath(from1, to1));
          solver->addClause(mkLit(p, false));
        } else {
          // // another (possible) definition e and wanted definition d
          // // either (1) e do not access d and
          // vector<Minisat::Lit> c;
          // Minisat::Var p = solver->newVar();
          // for (size_t i = 0; i < h.second->self_param->sort->n_import; i++) {
          //   auto from1 = PortLoc(h.second, Polar::Import, i);
          //   auto to1 = PortLoc(hs2.at(d2u->hole), Polar::Export, 0);
          //   auto q = analyzePath(PortPath(from1, to1));
          //   c.emplace_back(mkLit(q, true));
          // }
          // addIff(p, c);
          // for (size_t i = 0; i < h.second->self_param->sort->n_export; i++) {
          //   // in LHS, something access both
          //   vector<std::pair<Minisat::Var, Minisat::Var>> c;
          //   auto tod = PortLoc(hs1.at(d2u->hole), Polar::Export, 0);
          //   auto toe = PortLoc(hs1.at(h.first), Polar::Export, i);
          //   auto lca = ::lca(tod.e, toe.e);
          //   for (auto ap = lca; ap != nullptr; ap = ap->mom) {
          //     for (auto e0 : ap->args) {
          //       if (::lca(e0.get(), tod.e) != e0.get() &&
          //           ::lca(e0.get(), toe.e) != e0.get()) {
          //         for (size_t i = 0; i < e0->self_param->sort->n_import; i++) {
          //           auto from = PortLoc(e0.get(), Polar::Import, i);
          //           c.emplace_back(analyzePath(PortPath(from, tod)),
          //                          analyzePath(PortPath(from, toe)));
          //         }
          //       }
          //     }
          //   }
          //   auto s = solver->newVar();
          //   addIff(s, c);
          //   // or (2) do not access e
          //   auto from1 = PortLoc(d2u, Polar::Import, 0);
          //   auto to1 = PortLoc(hs2.at(h.first), Polar::Export, i);
          //   auto q = analyzePath(PortPath(from1, to1));
          //   // or (3) d access e
          //   auto from2 = PortLoc(hs2.at(d2u->hole), Polar::Import, 0);
          //   auto to2 = to1;
          //   auto r = analyzePath(PortPath(from2, to2));
          //   // (p /\ s) \/ ~q \/ r
          //   // (p \/ ~q \/ r) /\ (s \/ ~q \/ r)
          //   solver->addClause(mkLit(p, false), mkLit(q, true), mkLit(r, false));
          //   solver->addClause(mkLit(s, false), mkLit(q, true), mkLit(r, false));
          // }
        }
      }
    }
  } else if (HoleP hp2 = dynamic_cast<HoleP>(macro.to.get()); hp2 != nullptr) {
    for (PortId i = 0; i < func->sort->n_import; i++) {
      for (PortId j = 0; j < func->sort->n_import; j++) {
        PortLoc from(hs1.at(hp2->id), Polar::Import, i);
        PortLoc to(macro.from.get(), Polar::Import, j);
        auto var = analyzePath(PortPath(from, to));
        solver->addClause(mkLit(var, false));
      }
    }
    for (PortId i = 0; i < func->sort->n_export; i++) {
      for (PortId j = 0; j < func->sort->n_export; j++) {
        PortLoc from(macro.from.get(), Polar::Export, j);
        PortLoc to(hs1.at(hp2->id), Polar::Export, i);
        auto var = analyzePath(PortPath(from, to));
        solver->addClause(mkLit(var, false));
      }
    }
  }
}

void readbackScope() {
  for (auto &p : rule_var) {
    auto &r = p.first;
    auto var = p.second;
    auto f = r.func;
    if (!f->scoped) {
      f->imports = vector<PortSet>(f->sort->n_import);
      f->exports = vector<PortSet>(f->sort->n_export);
      for (auto &p : f->params) {
        p.binds = vector<PortSet>(p.sort->n_import);
      }
      f->scoped = 2;
    }
    if (f->scoped == 2 && !Minisat::toInt(solver->modelValue(var))) {
      switch (r.type) {
      case RuleType::Import:
        f->imports[r.port].emplace_back(r.from);
        break;
      case RuleType::Export:
        f->exports[r.port].emplace_back(r.to);
        break;
      case RuleType::Bind:
        r.from.param->binds[r.from.port].emplace_back(r.to);
        break;
      }
    }
  }
  for (auto &p : env.funcs) {
    if (p.second->scoped == 2) {
      p.second->scoped = 1;
    }
  }
}

void doInfer() {
  Minisat::Solver solver;
  ::solver = &solver;
  rule_var.clear();
  path_var.clear();
  for (auto &m : env.pending_macro) {
    generateConstraint(m);
  }
  solver.solve();
  // for (auto &p : rule_var) {
  //   auto &r = p.first;
  //   auto var = p.second;
  //   std::cout << r.func->name;
  //   switch (r.type) {
  //   case RuleType::Import:
  //     std::cout << " import " << r.from.param->name;
  //     break;
  //   case RuleType::Export:
  //     std::cout << " export " << r.to.param->name;
  //     break;
  //   case RuleType::Bind:
  //     std::cout << " bind " << r.from.param->name << ' ' << r.to.param->name;
  //     break;
  //   }
  //   std::cout << " :" << !Minisat::toInt(solver.modelValue(var));
  //   std::cout << std::endl;
  // }
  // for (auto &pr : path_var) {
  //   auto &p = pr.first;
  //   auto var = pr.second;
  //   p.from.e->show(std::cout);
  //   std::cout << " ";
  //   p.to.e->show(std::cout);
  //   std::cout << " :" << !Minisat::toInt(solver.modelValue(var));
  //   std::cout << std::endl;
  // }
  if (solver.okay()) {
    readbackScope();
  } else {
    fail("doInfer: inference failed");
  }
  env.pending_macro.clear();
}

// -------- regexp --------

struct Empty : public RegExp {
  Empty() {}
  virtual void show(std::ostream &os) const override {
    os << "nothing";
  };
  virtual bool equal(const RegExp &other) const override {
    return dynamic_cast<const Empty *>(&other) != nullptr;
  }
};

typedef Empty *EmptyP;

struct Eps : public RegExp {
  Eps() {}
  virtual void show(std::ostream &os) const override {
    os << "empty";
  };
  virtual bool equal(const RegExp &other) const override {
    return dynamic_cast<const Eps *>(&other) != nullptr;
  }
};

typedef Eps *EpsP;

typedef bool direction;
#define UP true
#define DOWN false

struct Char : public RegExp {
  ParameterP param;
  direction dir;
  Char(ParameterP param, direction dir): param(param), dir(dir) {}

  virtual void show(std::ostream &os) const override {
    os << param->mom->name << "." << param->name;
    os << (dir == UP ? "↑" : "↓");
  };
  virtual bool equal(const RegExp &other) const override {
    if (auto ch = dynamic_cast<const Char *>(&other); ch != nullptr) {
      return param == ch->param && dir == ch->dir;
    } else {
      return false;
    }
  }
};

typedef Char *CharP;

struct Concat : public RegExp {
  vector<RegExpP> rs;
  Concat(vector<RegExpP> &&rs): rs(std::move(rs)) {}

  virtual void show(std::ostream &os) const override {
    os << "(concat";
    for (auto r : rs) {
      os << " ";
      r->show(os);
    }
    os << ")";
  };
  virtual bool equal(const RegExp &other) const override {
    if (auto cc = dynamic_cast<const Concat *>(&other); cc != nullptr) {
      if (rs.size() != cc->rs.size()) {
        return false;
      } else {
        for (size_t i = 0; i < rs.size(); i++) {
          if (!rs[i]->equal(*cc->rs[i])) {
            return false;
          }
        }
        return true;
      }
    } else {
      return false;
    }
  }
};

typedef Concat *ConcatP;

struct Or : public RegExp {
  vector<RegExpP> rs;
  Or(vector<RegExpP> &&rs): rs(std::move(rs)) {}

  virtual void show(std::ostream &os) const override {
    os << "(or";
    for (auto r : rs) {
      os << " ";
      r->show(os);
    }
    os << ")";
  };
  virtual bool equal(const RegExp &other) const override {
    if (auto o = dynamic_cast<const Or *>(&other); o != nullptr) {
      if (rs.size() != o->rs.size()) {
        return false;
      } else {
        for (size_t i = 0; i < rs.size(); i++) {
          if (!rs[i]->equal(*o->rs[i])) {
            return false;
          }
        }
        return true;
      }
    } else {
      return false;
    }
  }
};

typedef Or *OrP;

struct Many : public RegExp {
  RegExpP r;
  Many(RegExpP r): r(r) {}

  virtual void show(std::ostream &os) const override {
    os << "(many ";
    r->show(os);
    os << ")";
  };
  virtual bool equal(const RegExp &other) const override {
    if (auto m = dynamic_cast<const Many *>(&other); m != nullptr) {
      return r->equal(*m->r);
    } else {
      return false;
    }
  }
};

inline RegExpP mkEps() {
  Eps *p = new Eps();
  return shared_ptr<RegExp>(p);
}

inline RegExpP mkEmpty() {
  Empty *p = new Empty();
  return shared_ptr<RegExp>(p);
}

inline RegExpP mkChar(ParameterP param, direction dir) {
  Char *p = new Char(param, dir);
  return shared_ptr<RegExp>(p);
}

inline RegExpP mkConcat(vector<RegExpP> &&rs) {
  Concat *p = new Concat(std::move(rs));
  return shared_ptr<RegExp>(p);
}

inline RegExpP mkOr(vector<RegExpP> &&rs) {
  Or *p = new Or(std::move(rs));
  return shared_ptr<RegExp>(p);
}

inline RegExpP mkMany(RegExpP r) {
  Many *p = new Many(r);
  return shared_ptr<RegExp>(p);
}

// smart constructors
// todo: avoid deep copy
RegExpP sOr(RegExpP r1, RegExpP r2) {
  if (EmptyP em1 = dynamic_cast<EmptyP>(r1.get()); em1 != nullptr) {
    return r2;
  }
  if (EmptyP em2 = dynamic_cast<EmptyP>(r2.get()); em2 != nullptr) {
    return r1;
  }
  if (r1 == r2 || r1->equal(*r2)) {
    return r1;
  }
  if (OrP or1 = dynamic_cast<OrP>(r1.get()),
          or2 = dynamic_cast<OrP>(r2.get());
      or1 != nullptr && or2 != nullptr) {
    vector<RegExpP> rs = or1->rs;
    for (auto r : or2->rs) {
      rs.emplace_back(r);
    }
    return mkOr(std::move(rs));
  } else if (or1 != nullptr) {
    vector<RegExpP> rs = or1->rs;
    rs.emplace_back(r2);
    return mkOr(std::move(rs));
  } else if (or2 != nullptr) {
    vector<RegExpP> rs = or2->rs;
    rs.insert(rs.begin(), r1);
    return mkOr(std::move(rs));
  } else {
    return mkOr({r1, r2});
  }
}

RegExpP sConcat(RegExpP left, RegExpP right) {
  if (EmptyP eml = dynamic_cast<EmptyP>(left.get()); eml != nullptr) {
    return left;
  }
  if (EmptyP emr = dynamic_cast<EmptyP>(right.get()); emr != nullptr) {
    return right;
  }
  if (EpsP epl = dynamic_cast<EpsP>(left.get()); epl != nullptr) {
    return right;
  }
  if (EpsP epr = dynamic_cast<EpsP>(right.get()); epr != nullptr) {
    return left;
  }
  if (ConcatP conl = dynamic_cast<ConcatP>(left.get()),
              conr = dynamic_cast<ConcatP>(right.get());
      conl != nullptr && conr != nullptr) {
    vector<RegExpP> rs = conl->rs;
    for (auto r : conr->rs) {
      rs.emplace_back(r);
    }
    return mkConcat(std::move(rs));
  } else if (conl != nullptr) {
    vector<RegExpP> rs = conl->rs;
    rs.emplace_back(right);
    return mkConcat(std::move(rs));
  } else if (conr != nullptr) {
    vector<RegExpP> rs = conr->rs;
    rs.insert(rs.begin(), left);
    return mkConcat(std::move(rs));
  } else {
    return mkConcat({left, right});
  }
}

struct RHS {
  RegExpP c;
  std::map<Id, RegExpP> xs;
  RHS(RegExpP c, std::map<Id, RegExpP> &&xs):
    c(c), xs(std::move(xs)) {}
};

struct SortPort {
  SortWP sort;
  PortId port;
  Polar polar;
  SortPort(SortWP sort, PortId port, Polar polar):
    sort(sort), port(port), polar(polar) {}
  bool operator<(const SortPort &other) const {
    return sort < other.sort ||
           (sort == other.sort &&
            (port < other.port ||
             (port == other.port &&
              polar < other.polar)));
  }
};

std::map<SortPort, Id> numbering;
vector<RHS> automaton;

Id sortPortToId(SortWP sort, PortId port, Polar polar) {
  auto sp = SortPort(sort, port, polar);
  if (auto it = numbering.find(sp); it != numbering.end()) {
    return it->second;
  } else {
    if (sort->name == "VarDef" && polar == Polar::Export) {
      automaton.emplace_back(mkEps(), std::map<Id, RegExpP>());
    } else {
      automaton.emplace_back(mkEmpty(), std::map<Id, RegExpP>());
    }
    return numbering[sp] = automaton.size() - 1;
  }
}

void addTransition(Id from, Id to, RegExpP ch) {
  auto &xs = automaton[from].xs;
  if (xs.find(to) == xs.end()) {
    xs[to] = ch;
  } else {
    xs[to] = sOr(xs[to], ch);
  }
}

Id addState() {
  automaton.emplace_back(mkEmpty(), std::map<Id, RegExpP>());
  return automaton.size() - 1;
}

void buildAutomate(const vector<FunctionWP> &fs, bool use) {
  numbering.clear();
  automaton.clear();
  sortPortToId(env.findSort(use ? "VarUse" : "VarDef").value(), 0, Polar::Import);
  for (auto f : fs) {
    for (size_t i = 0; i < f->imports.size(); i++) {
      for (auto &p : f->imports[i]) {
        addTransition(sortPortToId(p.param->sort, p.port, Polar::Import),
                      sortPortToId(f->sort, i, Polar::Import),
                      mkChar(p.param, UP));
      }
    }
    for (size_t i = 0; i < f->exports.size(); i++) {
      for (auto &p : f->exports[i]) {
        addTransition(sortPortToId(f->sort, i, Polar::Export),
                      sortPortToId(p.param->sort, p.port, Polar::Export),
                      mkChar(p.param, DOWN));
      }
    }
    for (auto &pa : f->params) {
      for (size_t i = 0; i < pa.binds.size(); i++) {
        bool first = true;
        Id tmp;
        for (auto &p : pa.binds[i]) {
          if (first) {
            tmp = addState();
            first = false;
          }
          addTransition(sortPortToId(pa.sort, i, Polar::Import), tmp,
                        mkChar(&pa, UP));
          addTransition(tmp, sortPortToId(p.param->sort, p.port, Polar::Export),
                        mkChar(p.param, DOWN));
        }
      }
    }
  }
}

// Brzozowski's algorithm

// [r/x]s
void subst(Id x, const RHS &r, RHS &s) {
  if (auto it = s.xs.find(x); it != s.xs.end()) {
    auto a = it->second;
    s.xs.erase(x);
    s.c = sOr(s.c, sConcat(a, r.c));
    for (auto &p : r.xs) {
      if (auto it = s.xs.find(p.first); it != s.xs.end()) {
        it->second = sOr(it->second, sConcat(a, p.second));
      } else {
        s.xs[p.first] = sConcat(a, p.second);
      }
    }
  }
}

RegExpP solve() {
  while (automaton.size() > 0) {
    auto x = automaton.size() - 1;
    auto &r = automaton.back();
    if (auto it = r.xs.find(x); it != r.xs.end()) {
      auto a = it->second;
      r.xs.erase(x);
      auto as = mkMany(a);
      r.c = sConcat(as, r.c);
      for (auto &b : r.xs) {
        b.second = sConcat(as, b.second);
      }
    }
    for (size_t i = 0; i < automaton.size() - 1; i++) {
      subst(x, automaton.back(), automaton[i]);
    }
    if (automaton.size() > 1) {
      automaton.pop_back();
    } else {
      break;
    }
  }
  return automaton[0].c;
}

std::pair<RegExpP, RegExpP> characterize(vector<FunctionWP> &fs) {
  buildAutomate(fs, true);
  auto ruse = solve();
  buildAutomate(fs, false);
  auto rdef = solve();
  return {ruse, rdef};
}

// -------- entry --------

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    std::ifstream fs(argv[i]);
    if (!repl(fs, true)) {
      fs.close();
      goto end;
    }
    fs.close();
  }
  repl(std::cin, false);
 end:
}

#undef fail
