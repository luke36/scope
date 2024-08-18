(defsort Expr :import 2 :export 0)
(defsort Type :import 1 :export 0)

(defun var (: x VarUse) :sort Expr)
(defun app (: rator Expr) (: rand Expr) :sort Expr)
(defun lambda (: x VarDef) (: t Type) (: e Expr) :sort Expr)
(defun Lambda (: t VarDef) (: e Expr) :sort Expr)

(defscope var :import ((x 0)) ())
(defscope app :import ((rator 0) (rand 0)) ((rator 1) (rand 1)))
(defscope lambda :import ((x 0) (e 0)) ((t 0) (e 1)) :bind ((e 0) (x 0)))
(defscope Lambda :import ((e 0)) ((t 0) (e 1)) :bind ((e 1) (t 0)))

(defun tvar (: t VarUse) :sort Type)
(defun tarrow (: from Type) (: to Type) :sort Type)

(defscope tvar :import ((t 0)))
(defscope tarrow :import ((from 0) (to 0)))

(regexp var app lambda Lambda tvar tarrow)
(exit)
