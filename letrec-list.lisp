(defsort Expr :import 1 :export 0)
(defsort LetBind :import 1 :export 1)

(defun void :sort Expr)
(defscope void :import ())
(defun var (: x VarUse) :sort Expr)
(defscope var :import ((x 0)))
(defun begin (: e Expr)* :sort Expr)
(defscope begin :import ((e 0)))
(defun set! (: x VarUse) (: e Expr) :sort Expr)
(defscope set! :import ((x 0) (e 0)))
(defun let (: b LetBind)* (: e Expr) :sort Expr)
(defscope let :import ((b 0) (e 0)) :bind ((e 0) (b 0)))

(defun bind (: x VarDef) (: e Expr) :sort LetBind)
(defscope bind :import ((x 0) (e 0)) :export ((x 0)))

(defsort Stx :import 2 :export 1)
(defun rec-bind (: x VarDef) (: e Expr) :sort Stx)
(defun letrec (: b Stx)* (: e Expr) :sort Expr)

(defmacro
    (letrec (append (mapcar rec-bind (append 1) (append 2))) 3)
    (let (append (mapcar bind (append 1) (append (repeat (void)))))
      (begin (append (mapcar set! (append (->use 1)) (append 2)) (list 3)))))

(infer)
(dump)
(regexp void var begin set! let bind letrec rec-bind)
(exit)
