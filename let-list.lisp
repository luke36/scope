(defsort Expr :import 1 :export 0)
(defun var (: x VarUse)
           :sort Expr)
(defun lambda (: x VarDef) (: body Expr)
              :sort Expr)
(defun app (: rator Expr) (: rand Expr)
           :sort Expr)
(defscope var
          :import ((x 0)))
(defscope lambda
          :import ((x 0) (body 0))
          :bind ((body 0) (x 0)))
(defscope app
          :import ((rator 0) (rand 0)))

(defsort Stx :import 1 :export 1)
(defun let* (: binds Stx)* (: body Expr) :sort Expr)
(defun bind (: x VarDef) (: e Expr) :sort Stx)
(defmacro (let* (append (list (bind 1 2)) 3) 4)
          (app (lambda 1 (let* (append 3) 4)) 2))
(defmacro (let* (append) 1) 1)

(infer)
(dump)
(regexp var lambda app let* bind)
(exit)
