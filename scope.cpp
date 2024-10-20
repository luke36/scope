#include <cstdint>
#include <fstream>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <tuple>
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

// builtin
SortWP VarDef;
SortWP VarUse;

struct Port;
typedef vector<Port> PortSet;

enum class MetaSort { List, Mono };

struct Parameter {
  Id id;
  string name;
  MetaSort msort;
  SortWP sort;
  FunctionWP mom;
  vector<PortSet> binds;
  vector<vector<PortId>> bind_left;
  vector<vector<PortId>> bind_right;
  Parameter(Id id, string &&name, MetaSort msort, SortWP sort):
    id(id), name(std::move(name)), msort(msort), sort(sort), mom(nullptr) {}
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
struct DefToUse;
struct Hole;
typedef shared_ptr<Expr> ExprP;
typedef Expr *ExprWP;
typedef Application *ApplicationP;
typedef Hole *HoleP;
typedef DefToUse *DefToUseP;
struct ListNF;
typedef shared_ptr<ListNF> ListNFP;
typedef ListNF *ListNFWP;

struct HoleContext {
  std::map<Id, HoleP> holes;
  HoleContext() = default;
};

struct Expr {
  ExprWP mom;
  size_t depth;
  ParameterP self_param;
  Expr(): mom(nullptr), self_param(nullptr) {}
  virtual void show(std::ostream &os) const = 0;
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) = 0;
  virtual void locateHoles(HoleContext &hc) const = 0;
  virtual void computeDepth(size_t d) = 0;
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const = 0;
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const = 0;
  virtual void findDefToUse(vector<DefToUseP> &d2u) const = 0;
};

struct Var: public Expr {
  string name;
  SortWP sort;
  Var(string &&name): Expr(), name(std::move(name)) {}
  virtual void show(std::ostream &os) const override {
    os << name;
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &) override {
    if (sort->name != "VarDef" && sort->name != "VarUse") {
      fail("typeOf: sort error");
    }
    this->sort = sort;
  }
  virtual void locateHoles(HoleContext &) const override {}
  virtual void computeDepth(size_t d) override {
    depth = d;
  }
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const override {
    if (sort == VarDef) {
      if (fdef.find(name) != fdef.end()) {
        fail("findFreshDef: fresh identifiers with same name");
      }
      fdef[name] = const_cast<Var *>(this);
    }
  }
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const override {
    if (sort == VarUse) {
      if (auto it = fdef.find(name); it != fdef.end()) {
        // todo
      } else {
        fail("findFreshUse: unbound identifier in rhs");
      }
    }
  }
  virtual void findDefToUse(vector<DefToUseP> &) const override {}
};

typedef Var *VarP;

struct ListS: public Expr {
  ListS(): Expr() {}
};

struct Hole: public ListS { // miserably...
  Id id;
  SortWP sort;
  enum class Cat { Norm, List, Rev } cat;
  Hole(Id id, Cat cat): ListS(), id(id), sort(nullptr), cat(cat) {}
  virtual void show(std::ostream &os) const override {
    os << id;
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &) override {
    this->sort = sort;
  }
  virtual void locateHoles(HoleContext &hc) const override {
    if (auto it = hc.holes.find(id); it != hc.holes.end()) {
      fail("locateHoles: duplicated hole");
    }
    hc.holes[id] = const_cast<HoleP>(this);
  }
  virtual void computeDepth(size_t d) override {
    depth = d;
  }
  virtual void findFreshDef(std::map<string, ExprWP> &) const override {}
  virtual void findFreshUse(std::map<string, ExprWP> &) const override {}
  virtual void findDefToUse(vector<DefToUseP> &) const override {}
};

struct DefToUse: public Expr {
  Id hole;
  DefToUse(Id hole): Expr(), hole(hole) {}
  virtual void show(std::ostream &os) const override {
    os << "(->use " << hole << ")";
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    if (sort != VarUse) {
      fail("typeOf: sort error");
    }
    if (auto it = holes.find(hole); it != holes.end()) {
      it->second->typeOf(VarDef, holes);
    } else {
      fail("typeOf: no such hole");
    }
  }
  virtual void locateHoles(HoleContext &) const override {}
  virtual void computeDepth(size_t d) override {
    depth = d;
  }
  virtual void findFreshDef(std::map<string, ExprWP> &) const override {}
  virtual void findFreshUse(std::map<string, ExprWP> &) const override {}
  virtual void findDefToUse(vector<DefToUseP> &d2u) const override {
    d2u.emplace_back(const_cast<DefToUseP>(this));
  }
};

typedef shared_ptr<ListS> ListSP;
typedef ListS *ListSWP;

// inherite Expr just for convenience
struct ListNF: public Expr {
  vector<ListSP> lists;
  ListNF(vector<ListSP> &&lists_):
    Expr(), lists(std::move(lists_)) {
    for (auto s : lists) {
      s->mom = this;
    }
  }
  virtual void show(std::ostream &os) const override {
    os << "(append";
    for (auto l : lists) {
      os << " ";
      l->show(os);
    }
    os << ")";
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    for (auto l : lists) {
      l->typeOf(sort, holes);
    }
  }
  virtual void locateHoles(HoleContext &hc) const override {
    for (auto l : lists) {
      l->locateHoles(hc);
    }
  }
  virtual void computeDepth(size_t d) override {
    depth = d;
    for (auto l : lists) {
      l->computeDepth(d + 1);
    }
  }
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const override {
    for (auto l : lists) {
      l->findFreshDef(fdef);
    }
  }
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const override {
    for (auto l : lists) {
      l->findFreshUse(fdef);
    }
  }
  virtual void findDefToUse(vector<DefToUseP> &d2u) const override {
    for (auto l : lists) {
      l->findDefToUse(d2u);
    }
  }
};

struct ListSingle: public ListS {
  ExprP expr;
  ListSingle(ExprP expr): ListS(), expr(expr) {}
  virtual void show(std::ostream &os) const override {
    os << "(list ";
    expr->show(os);
    os << ")";
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    expr->typeOf(sort, holes);
  }
  virtual void locateHoles(HoleContext &hc) const override {
    expr->locateHoles(hc);
  }
  virtual void computeDepth(size_t d) override {
    depth = d;
    expr->computeDepth(d + 1);
  }
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const override {
    expr->findFreshDef(fdef);
  }
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const override {
    expr->findFreshUse(fdef);
  }
  virtual void findDefToUse(vector<DefToUseP> &d2u) const override {
    expr->findDefToUse(d2u);
  }
};

typedef ListSingle *ListSingleP;

struct ListRepeat: public ListS {
  ExprP expr;
  ListRepeat(ExprP expr): ListS(), expr(expr) {}
  virtual void show(std::ostream &os) const override {
    os << "(repeat "; // from Haskell
    expr->show(os);
    os << ")";
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    expr->typeOf(sort, holes);
  }
  virtual void locateHoles(HoleContext &) const override {}
  virtual void computeDepth(size_t d) override {
    depth = d;
  }
  virtual void findFreshDef(std::map<string, ExprWP> &) const override {}
  virtual void findFreshUse(std::map<string, ExprWP> &) const override {}
  virtual void findDefToUse(vector<DefToUseP> &) const override {}
};

struct ListMap: public ListS {
  FunctionWP func;
  vector<ListNFP> args;
  ListMap(FunctionWP func_, vector<ListNFP> &&args_):
    ListS(), func(func_), args(std::move(args_)) {
    for (auto nf : args) {
      nf->mom = this;
    }
  }
  virtual void show(std::ostream &os) const override {
    os << "(mapcar " << func->name;
    for (auto l : args) {
      os << " ";
      l->show(os);
    }
    os << ")";
  }
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    auto &params = func->params;
    if (args.size() != params.size()) {
      fail("typeOf: wrong argument number");
    }
    if (func->sort != sort) {
      fail("typeOf: sort error");
    }
    for (size_t i = 0; i < args.size(); i++) {
      args[i]->typeOf(params[i].sort, holes);
      args[i]->self_param = &params[i];
    }
  }
  virtual void locateHoles(HoleContext &hc) const override {
    for (auto l : args) {
      l->locateHoles(hc);
    }
  }
  virtual void computeDepth(size_t d) override {
    depth = d;
    for (auto l : args) {
      l->computeDepth(d + 1);
    }
  }
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const override {
    for (auto l : args) {
      l->findFreshDef(fdef);
    }
  }
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const override {
    for (auto l : args) {
      l->findFreshUse(fdef);
    }
  }
  virtual void findDefToUse(vector<DefToUseP> &d2u) const override {
    for (auto l : args) {
      l->findDefToUse(d2u);
    }
  }
};

typedef ListMap *ListMapP;

struct Application: public Expr {
  FunctionWP func;
  vector<ExprP> args;
  Application(FunctionWP func_, vector<ExprP> &&args_):
    Expr(),func(func_), args(std::move(args_)) {
    for (size_t i = 0; i < args.size(); i++) {
      args[i]->mom = this;
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
  virtual void typeOf(SortWP sort, const std::map<Id, HoleP> &holes) override {
    auto &params = func->params;
    if (args.size() != params.size()) {
      fail("typeOf: wrong argument number");
    }
    if (func->sort != sort) {
      fail("typeOf: sort error");
    }
    for (size_t i = 0; i < args.size(); i++) {
      if (ListNFWP l = dynamic_cast<ListNFWP>(args[i].get());
          (l != nullptr) != (params[i].msort == MetaSort::List)) {
        fail("typeOf: meta-sort error");
      }
      args[i]->typeOf(params[i].sort, holes);
      args[i]->self_param = &params[i];
    }
  }
  virtual void locateHoles(HoleContext &hc) const override {
    for (auto e0 : args) {
      e0->locateHoles(hc);
    }
  }
  virtual void computeDepth(size_t d) override {
    depth = d;
    for (auto e0 : args) {
      e0->computeDepth(d + 1);
    }
  }
  virtual void findFreshDef(std::map<string, ExprWP> &fdef) const override {
    for (auto e0 : args) {
      e0->findFreshDef(fdef);
    }
  }
  virtual void findFreshUse(std::map<string, ExprWP> &fdef) const override {
    for (auto e0 : args) {
      e0->findFreshUse(fdef);
    }
  }
  virtual void findDefToUse(vector<DefToUseP> &d2u) const override {
    for (auto e0 : args) {
      e0->findDefToUse(d2u);
    }
  }
};

inline ExprP mkVar(string &&name) {
  Var *p = new Var(std::move(name));
  return shared_ptr<Expr>(p);
}

inline ExprP mkHole(Id id, Hole::Cat cat) {
  Hole *p = new Hole(id, cat);
  return shared_ptr<Expr>(p);
}

inline ListSP mkListHole(Id id, Hole::Cat cat) {
  Hole *p = new Hole(id, cat);
  return shared_ptr<ListS>(p);
}

inline ExprP mkFunc(FunctionWP func, vector<ExprP> &&args) {
  Application *p = new Application(func, std::move(args));
  return shared_ptr<Expr>(p);
}

inline ExprP mkDefToUse(Id hole) {
  DefToUse *p = new DefToUse(hole);
  return shared_ptr<Expr>(p);
}

inline ExprP mkListNF(vector<ListSP> &&lists) {
  ListNFWP p = new ListNF(std::move(lists));
  return shared_ptr<Expr>(p);
}

inline ListSP mkListSingle(ExprP expr) {
  ListSWP p = new ListSingle(expr);
  return shared_ptr<ListS>(p);
}

inline ListSP mkListRepeat(ExprP expr) {
  ListSWP p = new ListRepeat(expr);
  return shared_ptr<ListS>(p);
}

inline ListSP mkListMap(FunctionWP func, vector<ListNFP> &&args) {
  ListSWP p = new ListMap(func, std::move(args));
  return shared_ptr<ListS>(p);
}

struct Macro {
  ExprP from;
  ExprP to;
  HoleContext from_hc;
  HoleContext to_hc;

  Macro(ExprP from, ExprP to): from(from), to(to) {
    from->locateHoles(from_hc);
    to->locateHoles(to_hc);
  }
};

void checkMacro(const Macro &m) {
  // we require LHS to be an compound; it eases type-checking, and I do not
  // think anyone would want a hole as LHS.
  ApplicationP ap;
  if (ap = dynamic_cast<ApplicationP>(m.from.get()); ap == nullptr) {
    fail("checkMacro: bad LHS");
  }
  m.from->typeOf(ap->func->sort, std::map<Id, HoleP>());
  m.to->typeOf(ap->func->sort, m.to_hc.holes);
  for (auto &p : m.to_hc.holes) {
    if (auto it = m.from_hc.holes.find(p.first); it != m.from_hc.holes.end()) {
      if (p.second->sort != it->second->sort) {
        fail("checkMacro: sort error");
      } else if ((p.second->cat == Hole::Cat::Norm) !=
                 (it->second->cat == Hole::Cat::Norm)) {
        fail("checkMacro: meta-sort error");
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
                   vector<std::tuple<string, MetaSort, string>> &&param) {
    vector<Parameter> params;
    for (size_t i = 0; i < param.size(); i++) {
      params.emplace_back(fresh(), std::move(std::get<0>(param[i])),
                          std::get<1>(param[i]),
                          findSortException(std::get<2>(param[i])));
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
                const vector<std::pair<TextPort, TextPort>> &binds,
                const vector<std::pair<TextPort, PortId>> &bind_left,
                const vector<std::pair<TextPort, PortId>> &bind_right) {
    auto f = findFunctionException(name);
    if (f->scoped) {
      fail("addScope: already specified scope");
    }
    // clear from last exception
    f->imports = vector<PortSet>(f->sort->n_import);
    f->exports = vector<PortSet>(f->sort->n_export);
    for (auto &p : f->params) {
      p.binds = vector<PortSet>(p.sort->n_import);
      if (p.msort == MetaSort::List) {
        p.bind_left = vector<vector<PortId>>(p.sort->n_import);
        p.bind_right = vector<vector<PortId>>(p.sort->n_import);
      }
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
    for (auto &p : bind_left) {
      auto param = f->findParameter(p.first.param);
      if (param->msort != MetaSort::List ||
          p.first.port >= param->sort->n_import ||
          p.second >= param->sort->n_export) {
        fail("addScope: invalid bind-left");
      }
      param->bind_left[p.first.port].emplace_back(p.second);
    }
    for (auto &p : bind_right) {
      auto param = f->findParameter(p.first.param);
      if (param->msort != MetaSort::List ||
          p.first.port >= param->sort->n_import ||
          p.second >= param->sort->n_export) {
        fail("addScope: invalid bind-right");
      }
      param->bind_right[p.first.port].emplace_back(p.second);
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
        first = true;
        for (auto &p : f.second->params) {
          for (size_t i = 0; i < p.bind_left.size(); i++) {
            for (auto port : p.bind_left[i]) {
              if (first) {
                os << " :bind-left";
                first = false;
              }
              os << " (" << p.name << " " << i << " " << port << ")";
            }
          }
        }
        first = true;
        for (auto &p : f.second->params) {
          for (size_t i = 0; i < p.bind_right.size(); i++) {
            for (auto port : p.bind_right[i]) {
              if (first) {
                os << " :bind-right";
                first = false;
              }
              os << " (" << p.name << " " << i << " " << port << ")";
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
    // ...
    VarUse = findSortException("VarUse");
    VarDef = findSortException("VarDef");
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

ListSP readListS(std::istream &is);

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
    } else if (func == "append") {
      vector<ListSP> lists;
      while ((c = getcharSkip(is)) != ')') {
        is.putback(c);
        lists.emplace_back(readListS(is));
      }
      return mkListNF(std::move(lists));
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
        return mkHole(id, Hole::Cat::Norm);
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

uint64_t readULong(std::istream &is) {
  string &&x = readString(is);
  size_t pos;
  try {
    long n = std::stoul(x, &pos);
    if (pos == x.size() && n >= 0) {
      return n;
    } else {
      fail("readULong: not a number");
    }
  } catch (...) {
    fail("readULong: not a number");
  }
}

ListNFP readListNF(std::istream &is) {
  readExactChar(is, '(');
  string &&prim = readString(is);
  if (prim != "append") {
    fail("readListNF: expecting 'append'");
  }
  vector<ListSP> lists;
  char c;
  while ((c = getcharSkip(is)) != ')') {
    is.putback(c);
    lists.emplace_back(readListS(is));
  }
  return std::make_shared<ListNF>(std::move(lists));
}

ListSP readListS(std::istream &is) {
  auto c = getcharSkip(is);

  switch (c) {
  case '(': {
    string &&prim = readString(is);
    if (prim == "reverse") {
      auto id = readULong(is);
      readExactChar(is, ')');
      return mkListHole(id, Hole::Cat::Rev);
    } else if (prim == "list") {
      auto expr = readExpr(is);
      readExactChar(is, ')');
      return mkListSingle(expr);
    } else if (prim == "repeat") {
      auto expr = readExpr(is);
      readExactChar(is, ')');
      return mkListRepeat(expr);
    } else if (prim == "map") {
      string &&func = readString(is);
      vector<ListNFP> args;
      while ((c = getcharSkip(is)) != ')') {
        is.putback(c);
        args.emplace_back(readListNF(is));
      }
      return mkListMap(env.findFunctionException(func), std::move(args));
    } else {
      fail("readListS: unknown list primitive");
    }
  }
  default:
    is.putback(c);
    return mkListHole(readULong(is), Hole::Cat::List);
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
  vector<std::tuple<string, MetaSort, string>> param;
  while ((c = getcharSkip(is)) == '(') {
    readExactChar(is, ':');
    string &&name = readString(is);
    string &&sort = readString(is);
    readExactChar(is, ')');
    char star = getcharSkip(is);
    MetaSort ms = MetaSort::Mono;
    if (star == '*') {
      ms = MetaSort::List;
    } else {
      is.putback(star);
    }
    param.emplace_back(std::move(name), ms, std::move(sort));
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
  vector<std::pair<TextPort, PortId>> bind_left;
  vector<std::pair<TextPort, PortId>> bind_right;
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
    } else if (which == ":bind-left") {
      while ((c = getcharSkip(is)) == '(') {
        string &&param = readString(is);
        PortId from = readUInt(is);
        PortId to = readUInt(is);
        bind_left.emplace_back(TextPort(std::move(param), from), to);
        readExactChar(is, ')');
      }
      is.putback(c);
    } else if (which == ":bind-right") {
      while ((c = getcharSkip(is)) == '(') {
        string &&param = readString(is);
        PortId from = readUInt(is);
        PortId to = readUInt(is);
        bind_right.emplace_back(TextPort(std::move(param), from), to);
        readExactChar(is, ')');
      }
      is.putback(c);
    } else {
      fail("readDefscope: bad syntax");
    }
  }
  is.putback(c);
  env.addScope(name, imports, exports, binds, bind_left, bind_right);
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

enum class Orient { Left, Right, Same };

struct LCA {
  Orient o;
  ApplicationP high;
  ListMapP low;
  LCA(Orient o, ApplicationP high, ListMapP low): o(o), high(high), low(low) {}
};

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

LCA lca(ExprWP e1, ExprWP e2) {
  if (e1 == e2) { // bind-left/right
    ApplicationP high;
    while (true) {
      e1 = e1->mom;
      if (auto high0 = dynamic_cast<ApplicationP>(e1); e1 != nullptr) {
        high = high0;
        break;
      }
    }
    return LCA(Orient::Same, high, nullptr);
  }
  size_t d1 = e1->depth;
  size_t d2 = e2->depth;
  for (; d1 > d2; d1--) {
    e1 = e1->mom;
  }
  for (; d2 > d1; d2--) {
    e2 = e2->mom;
  }
  ExprWP e1_o, e2_o;
  while (e1 != e2) {
    e1_o = e1;
    e2_o = e2;
    e1 = e1->mom;
    e2 = e2->mom;
  }
  if (auto ap = dynamic_cast<ApplicationP>(e1); ap != nullptr) {
    return LCA(Orient::Same, ap, nullptr);
  } else if (auto nf = dynamic_cast<ListNFWP>(e1); nf != nullptr) {
    Orient o;
    for (auto l : nf->lists) {
      if (l.get() == e1_o) {
        o = Orient::Left;
        break;
      } else if (l.get() == e2_o) {
        o = Orient::Right;
        break;
      }
    }
    for (ExprWP e0 = nf;
         (ap = dynamic_cast<ApplicationP>(e0)) == nullptr;
         e0 = e0->mom)
      ;
    return LCA(o, ap, nullptr);
  } else if (auto mp = dynamic_cast<ListMapP>(e1); mp != nullptr) {
    for (ExprWP e0 = mp;
         (ap = dynamic_cast<ApplicationP>(e0)) == nullptr;
         e0 = e0->mom)
      ;
    return LCA(Orient::Same, ap, mp);
  } else {
    fail("lca: this should not happend. Check it");
  }
}

struct PortPath {
  PortLoc from;
  PortLoc to;
  bool operator<(const PortPath &other) const {
    return from < other.from || (from == other.from && to < other.to);
  }
  PortPath(const PortLoc &from, const PortLoc &to): from(from), to(to) {}
};

enum class RuleType { Import, Export, Bind, BindLeft, BindRight };

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
  Rule(RuleType type, FunctionWP func, PortId port, Port from, Port to):
    type(type), func(func), port(port), from(std::move(from)), to(std::move(to)) {}
};

Rule Import(Port from, FunctionWP func, PortId to) {
  return Rule(RuleType::Import, func, to, from, from);
}

Rule Export(FunctionWP func, PortId from, Port to) {
  return Rule(RuleType::Export, func, from, to, to);
}

Rule Bind(Port from, Port to) {
  return Rule(RuleType::Bind, from.param->mom, 0, from, to);
}

Rule BindLeft(Port from, PortId to) {
  return Rule(RuleType::BindLeft, from.param->mom, 0,
              from, Port(from.param, to));
}

Rule BindRight(Port from, PortId to) {
  return Rule(RuleType::BindRight, from.param->mom, 0,
              from, Port(from.param, to));
}

#include "minisat/core/Solver.h"

using Minisat::mkLit;

std::map<Rule, Minisat::Var> rule_var;
std::map<PortPath, Minisat::Var> path_var;
Minisat::Solver *solver;

Minisat::Var True;

bool hasPort(const PortSet &s, const Port &p) {
  for (auto &q : s) {
    if (p == q) {
      return true;
    }
  }
  return false;
}

bool hasPortId(const vector<PortId> &s, PortId p) {
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
      case RuleType::BindLeft:
        if (hasPortId(r.from.param->bind_left[r.from.port], r.to.port)) {
          solver->addClause(mkLit(var, false));
        } else {
          solver->addClause(mkLit(var, true));
        }
      case RuleType::BindRight:
        if (hasPortId(r.from.param->bind_right[r.from.port], r.to.port)) {
          solver->addClause(mkLit(var, false));
        } else {
          solver->addClause(mkLit(var, true));
        }
      }
    }
    rule_var[r] = var;
    return var;
  }
}

// p only if q11 /\ q12 \/ q21 /\ q22 \/ ...
void addImply(Minisat::Var p,
              const vector<std::pair<Minisat::Var, Minisat::Var>> &q) {
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

void addImply(const vector<std::pair<Minisat::Var, Minisat::Var>> &q,
              Minisat::Var p) {
  for (auto &q1q2 : q) {
    // a /\ b \/ c /\ d -> e
    // a /\ b -> e, c /\ d -> e
    // ~a \/ ~b \/ e
    solver->addClause(mkLit(p, false),
                      mkLit(q1q2.first, true),
                      mkLit(q1q2.second, true));
  }
}

void addIff(Minisat::Var p,
            const vector<std::pair<Minisat::Var, Minisat::Var>> &q) {
  addImply(p, q);
  addImply(q, p);
}

void addIff(Minisat::Var p, Minisat::Var q) {
  // a <-> b
  // ~a\/b /\ ~b\/a
  solver->addClause(mkLit(p, false), mkLit(q, true));
  solver->addClause(mkLit(q, false), mkLit(p, true));
}

std::pair<ExprWP, ExprWP> nextMatter(ExprWP low, ExprWP high) {
  ExprWP i = low->mom, j = low;
  while (i != high) {
    if (auto ap = dynamic_cast<ApplicationP>(i); ap != nullptr) {
      return {j, ap};
    } else if (auto mp = dynamic_cast<ListMapP>(i); mp != nullptr) {
      return {j, mp};
    } else {
      j = i;
      i = i->mom;
    }
  }
  return {j, nullptr};
}

FunctionWP extractFunc(ExprWP e) {
  if (auto ap = dynamic_cast<ApplicationP>(e); ap != nullptr) {
    return ap->func;
  } else if (auto mp = dynamic_cast<ListMapP>(e); mp != nullptr) {
    return mp->func;
  } else {
    fail("extractFunc: can't extrace function");
  }
}

Minisat::Var analyzeSeg(const PortPath &p) {
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
    auto arg_higher = nextMatter(from.e, to.e);
    auto arg = arg_higher.first;
    auto higher = arg_higher.second;
    if (higher == nullptr) {
      auto func = extractFunc(to.e);
      addIff(var, ruleToVar(Import(Port(arg->self_param, from.port),
                                   func, to.port)));
      return var;
    } else {
      auto func = extractFunc(higher);
      vector<std::pair<Minisat::Var, Minisat::Var>> q;
      for (PortId i = 0; i < func->sort->n_import; i++) {
        auto var1 = analyzeSeg(PortPath(PortLoc(higher, Polar::Import, i), to));
        auto var2 = ruleToVar(Import(Port(arg->self_param, from.port),
                                     func, i));
        q.emplace_back(var1, var2);
      }
      addIff(var, q);
      return var;
    }
  } else if (from.polar == Polar::Export && to.polar == Polar::Export) {
    auto arg_higher = nextMatter(to.e, from.e);
    auto arg = arg_higher.first;
    auto higher = arg_higher.second;
    if (higher == nullptr) {
      auto func = extractFunc(from.e);
      addIff(var, ruleToVar(Export(func, from.port,
                                   Port(arg->self_param, to.port))));
      return var;
    } else {
      auto func = extractFunc(higher);
      vector<std::pair<Minisat::Var, Minisat::Var>> q;
      for (PortId i = 0; i < func->sort->n_export; i++) {
        auto var1 = analyzeSeg(PortPath(from, PortLoc(higher, Polar::Export, i)));
        auto var2 = ruleToVar(Export(func, i, Port(arg->self_param, to.port)));
        q.emplace_back(var1, var2);
      }
      addIff(var, q);
      return var;
    }
  } else {
    fail("analyzeSeg: bad path");
  }
}

Minisat::Var varImport(ExprWP hole, PortId p1, ExprWP root, PortId p2) {
  return analyzeSeg(PortPath(PortLoc(hole, Polar::Import, p1),
                             PortLoc(root, Polar::Import, p2)));
}

Minisat::Var varExport(ExprWP hole, PortId p1, ExprWP root, PortId p2) {
  return analyzeSeg(PortPath(PortLoc(root, Polar::Export, p2),
                             PortLoc(hole, Polar::Export, p1)));
}

Minisat::Var conj3(Minisat::Var x, Minisat::Var y, Minisat::Var z) {
  // v <-> x/\y/\z
  // v -> x/\y/\z  ~v\/x /\ ~v\/y /\ ~v\/z
  // ~x\/~y\/~z\/v
  auto v = solver->newVar();
  solver->addClause(mkLit(v, true), mkLit(x, false));
  solver->addClause(mkLit(v, true), mkLit(y, false));
  solver->addClause(mkLit(v, true), mkLit(z, false));
  Minisat::vec<Minisat::Lit> ls;
  ls.push(mkLit(x, true));
  ls.push(mkLit(y, true));
  ls.push(mkLit(z, true));
  ls.push(mkLit(v, false));
  solver->addClause(ls);
  return v;
}

void addIff
(Minisat::Var p,
 const vector<std::tuple<Minisat::Var, Minisat::Var, Minisat::Var>> &q) {
  for (auto &xyz : q) {
    auto x = std::get<0>(xyz), y = std::get<1>(xyz), z = std::get<2>(xyz);
    solver->addClause(mkLit(p, false),
                      mkLit(x, true), mkLit(y, true), mkLit(z, true));
  }
  Minisat::vec<Minisat::Lit> defs;
  for (auto &xyz : q) {
    auto x = std::get<0>(xyz), y = std::get<1>(xyz), z = std::get<2>(xyz);
    defs.push(mkLit(conj3(x, y, z), false));
  }
  defs.push(mkLit(p, true));
  solver->addClause(defs);
}

std::pair<ExprWP, ExprWP> lastMatter(ExprWP low, ExprWP high) {
  ExprWP i = low->mom, j = low;
  ExprWP reti = nullptr, retj = j;
  while (i != high) {
    if (auto ap = dynamic_cast<ApplicationP>(i); ap != nullptr) {
      reti = ap;
      retj = j;
    } else if (auto mp = dynamic_cast<ListMapP>(i); mp != nullptr) {
      reti = mp;
      retj = j;
    }
    j = i;
    i = i->mom;
  }
  return {retj, reti};
}

Minisat::Var varBindLeft(ExprWP hole, PortId p1, PortId p2) {
  auto &&lca = ::lca(hole, hole);
  ApplicationP ap = lca.high;
  auto top = lastMatter(hole, ap);
  if (top.second == nullptr) {
    return ruleToVar(BindLeft(Port(top.first->self_param, p1), p2));
  } else {
    auto _ = lastMatter(top.second, ap);
    auto param = _.first->self_param;
    vector<std::tuple<Minisat::Var, Minisat::Var, Minisat::Var>> vs;
    for (PortId i = 0; i < param->sort->n_import; i++) {
      for (PortId j = 0; j < param->sort->n_export; j++) {
        auto p = ruleToVar(BindLeft(Port(param, i), j));
        auto q = analyzeSeg(PortPath(PortLoc(hole, Polar::Import, p1),
                                     PortLoc(top.second, Polar::Import, i)));
        auto r = analyzeSeg(PortPath(PortLoc(top.second, Polar::Export, j),
                                     PortLoc(hole, Polar::Export, p2)));
        vs.emplace_back(p, q, r);
      }
    }
    auto v = solver->newVar();
    addIff(v, vs);
    return v;
  }
}

Minisat::Var varBindRight(ExprWP hole, PortId p1, PortId p2) {
  auto &&lca = ::lca(hole, hole);
  ApplicationP ap = lca.high;
  auto top = lastMatter(hole, ap);
  if (top.second == nullptr) {
    return ruleToVar(BindRight(Port(top.first->self_param, p1), p2));
  } else {
    auto _ = lastMatter(top.second, ap);
    auto param = _.first->self_param;
    vector<std::tuple<Minisat::Var, Minisat::Var, Minisat::Var>> vs;
    for (PortId i = 0; i < param->sort->n_import; i++) {
      for (PortId j = 0; j < param->sort->n_export; j++) {
        auto p = ruleToVar(BindRight(Port(param, i), j));
        auto q = analyzeSeg(PortPath(PortLoc(hole, Polar::Import, p1),
                                     PortLoc(top.second, Polar::Import, i)));
        auto r = analyzeSeg(PortPath(PortLoc(top.second, Polar::Export, j),
                                     PortLoc(hole, Polar::Export, p2)));
        vs.emplace_back(p, q, r);
      }
    }
    auto v = solver->newVar();
    addIff(v, vs);
    return v;
  }
}

Minisat::Var varBind(ExprWP hole1, PortId p1, ExprWP hole2, PortId p2) {
  auto &&lca = ::lca(hole1, hole2);
  ApplicationP ap = lca.high;
  if (lca.o != Orient::Same) {
    return (lca.o == Orient::Right ? varBindLeft : varBindRight)(hole1, p1, p2);
  } else if (lca.low == nullptr) {
    auto top1 = lastMatter(hole1, ap);
    auto top2 = lastMatter(hole2, ap);
    PortId l1, r1, l2, r2;
    ParameterP param1, param2;
    if (top1.second == nullptr) {
      l1 = r1 = p1;
      r1 += 1;
      param1 = top1.first->self_param;
    } else {
      auto _ = lastMatter(top1.second, ap);
      param1 = _.first->self_param;
      l1 = 0;
      r1 = param1->sort->n_import;
    }
    if (top2.second == nullptr) {
      l2 = r2 = p2;
      r2 += 1;
      param2 = top2.first->self_param;
    } else {
      auto _ = lastMatter(top2.second, ap);
      param2 = _.first->self_param;
      l2 = 0;
      r2 = param2->sort->n_export;
    }
    vector<std::tuple<Minisat::Var, Minisat::Var, Minisat::Var>> vs;
    for (PortId i = l1; i < r1; i++) {
      for (PortId j = l2; j < r2; j++) {
        auto p = ruleToVar(Bind(Port(param1, i), Port(param2, j)));
        auto q = top1.second == nullptr ? True :
          analyzeSeg(PortPath(PortLoc(hole1, Polar::Import, p1),
                              PortLoc(top1.second, Polar::Import, i)));
        auto r = top2.second == nullptr ? True :
          analyzeSeg(PortPath(PortLoc(top2.second, Polar::Export, j),
                              PortLoc(hole2, Polar::Export, p2)));
        vs.emplace_back(p, q, r);
      }
    }
    auto v = solver->newVar();
    addIff(v, vs);
    return v;
  } else {
    fail("unsupported now; shall be so in a short time");
  }
}

void generateConstraint(const Macro &macro) {
  auto &hs1 = macro.from_hc.holes,
       &hs2 = macro.to_hc.holes;
  macro.from->computeDepth(0);
  macro.to->computeDepth(0);
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
        for (PortId i = 0; i < h1.second->sort->n_import; i++) {
          for (PortId j = 0; j < g1.second->sort->n_export; j++) {
            auto p = varBind(h1.second, i, g1.second, j);
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
        for (PortId i = 0; i < h1.second->sort->n_import; i++) {
          for (PortId j = 0; j < func->sort->n_import; j++) {
            auto p = varImport(h1.second, i, ap1, j);
            auto q = varImport(h2->second, i, ap2, j);
            addIff(p, q);
          }
        }
      }
    }
    // export
    for (auto &h1 : hs1) {
      if ((h2 = hs2.find(h1.first)) != hs2.end()) {
        for (PortId i = 0; i < h1.second->sort->n_export; i++) {
          for (PortId j = 0; j < func->sort->n_export; j++) {
            auto p = varExport(h1.second, i, ap1, j);
            auto q = varExport(h2->second, i, ap2, j);
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
          for (PortId i = 0; i < h1.second->sort->n_import; i++) {
            for (PortId j = 0; j < g1.second->sort->n_export; j++) {
              auto p = varBind(h1.second, i, g1.second, j);
              auto q = varBind(h2->second, i, g2->second, j);
              addIff(p, q);
            }
          }
        }
      }
    }
    // bind-left/right
    for (auto &h1 : hs1) {
      if (h1.second->cat != Hole::Cat::Norm) {
        if ((h2 = hs2.find(h1.first)) != hs2.end()) {
          for (PortId i = 0; i < h1.second->sort->n_import; i++) {
            for (PortId j = 0; j < h1.second->sort->n_export; j++) {
              auto p = varBindLeft(h1.second, i, j);
              auto q = varBindLeft(h2->second, i, j);
              addIff(p, q);
              p = varBindRight(h1.second, i, j);
              q = varBindRight(h2->second, i, j);
              addIff(p, q);
            }
          }
        }
      }
    }
    // fresh in rhs should be uniquely bound
    std::map<string, ExprWP> fdef;
    macro.to->findFreshDef(fdef);
    macro.to->findFreshUse(fdef);
    // ->use should be uniquely bound
    vector<DefToUseP> d2us;
    macro.to->findDefToUse(d2us);
    for (auto d2u : d2us) {
      for (auto &h : hs2) {
        if (h.first == d2u->hole) {
          auto p = varBind(d2u, 0, h.second, 0);
          solver->addClause(mkLit(p, false));
        }
      }
    }
  } else if (HoleP hp2 = dynamic_cast<HoleP>(macro.to.get()); hp2 != nullptr) {
    for (PortId i = 0; i < func->sort->n_import; i++) {
      for (PortId j = 0; j < func->sort->n_import; j++) {
        auto var = varImport(hs1.at(hp2->id), i, macro.from.get(), j);
        solver->addClause(mkLit(var, false));
      }
    }
    for (PortId i = 0; i < func->sort->n_export; i++) {
      for (PortId j = 0; j < func->sort->n_export; j++) {
        auto var = varExport(hs1.at(hp2->id), i, macro.from.get(), j);
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
        p.bind_left = vector<vector<PortId>>(p.sort->n_import);
        p.bind_right = vector<vector<PortId>>(p.sort->n_import);
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
      case RuleType::BindLeft:
        r.from.param->bind_left[r.from.port].emplace_back(r.to.port);
        break;
      case RuleType::BindRight:
        r.from.param->bind_right[r.from.port].emplace_back(r.to.port);
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
  True = solver.newVar();
  solver.addClause(mkLit(True, false));
  rule_var.clear();
  path_var.clear();
  for (auto &m : env.pending_macro) {
    generateConstraint(m);
  }
  std::cerr << solver.nVars() << ' ' << solver.nClauses() << std::endl;
  solver.solve();
  if (solver.okay()) {
    readbackScope();
  } else {
    fail("doInfer: inference failed");
  }
  env.pending_macro.clear();
}

// -------- regexp --------

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
  if (OrP orl = dynamic_cast<OrP>(left.get());
      orl != nullptr && orl->rs.empty()) {
    return left;
  }
  if (OrP orr = dynamic_cast<OrP>(right.get());
      orr != nullptr && orr->rs.empty()) {
    return right;
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
      automaton.emplace_back(mkConcat({}), std::map<Id, RegExpP>());
    } else {
      automaton.emplace_back(mkOr({}), std::map<Id, RegExpP>());
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
  automaton.emplace_back(mkOr({}), std::map<Id, RegExpP>());
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

#include <chrono>

int main(int argc, char *argv[]) {
  auto begin = std::chrono::high_resolution_clock::now();
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
  auto end = std::chrono::high_resolution_clock::now();
  std::cerr << std::chrono::duration_cast<std::chrono::nanoseconds>(end - begin)
                   .count()
            << std::endl;
}

#undef fail
