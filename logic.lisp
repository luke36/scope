(defsort Stmt :import 2 :export 2)
(defsort Expr :import 1 :export 0)
(defsort Assn :import 1 :export 0)

(defun seq (: c1 Stmt) (: c2 Stmt) :sort Stmt)
(defun decl (: x VarDef) :sort Stmt)
(defun ghost (: x VarDef) :sort Stmt)
(defun expr (: e Expr) :sort Stmt)
(defun assn (: a Assn) :sort Stmt)

(defscope seq :import ((c1 0) (c2 0)) ((c1 1) (c2 1))
              :export ((c1 0) (c2 0)) ((c1 1) (c2 1))
              :bind ((c2 0) (c1 0)) ((c2 1) (c1 1)))
(defscope decl :import ((x 0)) ((x 0)) :export ((x 0)) ())
(defscope ghost :import () ((x 0)) :export () ((x 0)))
(defscope expr :import ((e 0)) ())
(defscope assn :import ((a 0)) ((a 0)))

(defun e-var (: x VarUse) :sort Expr)
(defun a-var (: x VarUse) :sort Assn)

(defscope e-var :import ((x 0)))
(defscope a-var :import ((x 0)))

(regexp seq decl ghost expr assn e-var a-var)
(exit)
