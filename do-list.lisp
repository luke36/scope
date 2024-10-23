(defsort Expr :import 1 :export 0)
(defsort Bind :import 2 :export 1)

(defun var (: x VarUse) :sort Expr)
(defscope var :import ((x 0)))
(defun lambda (: xs VarDef)* (: e Expr) :sort Expr)
(defscope lambda :import ((xs 0) (e 0)) :bind ((e 0) (xs 0)))
(defun app (: f Expr) (: es Expr)* :sort Expr)
(defscope app :import ((f 0) (es 0)))

(defun letrec (: bind Bind)* (: body Expr) :sort Expr)
(defscope letrec :import ((bind 1) (body 0)) :bind ((body 0) (bind 0)) :bind-left (bind 0 0) :bind-right (bind 0 0))
(defun bind (: x VarDef) (: e Expr) :sort Bind)
(defscope bind :import ((e 0)) ((x 0) (e 0)) :export ((x 0)) :bind ((e 0) (x 0)))

(defsort Stx :import 2 :export 1)
(defun do-bind (: x VarDef) (: init Expr) (: step Expr) :sort Stx)
(defun do (: binds Stx)* :sort Expr)
(defmacro
    (do (append (mapcar do-bind (append 1) (append 2) (append 3))))
    (letrec (append (list (bind loop (lambda (append 1)
                                       (app (var loop) (append 3))))))
      (app (var loop) (append 2))))

(infer)
(dump)
(regexp var lambda app letrec bind do-bind do)
(exit)
