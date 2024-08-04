(defsort Expr :import 1 :export 0)
(defun var (: x VarUse)
           :sort Expr)
(defun lambda (: x VarDef) (: body Expr)
              :sort Expr)
(defun app (: rator Expr) (: rand Expr)
           :sort Expr)
(defscope :function var
          :import ((x 0)))
(defscope :function lambda
          :import ((x 0) (body 0))
          :bind ((body 0) (x 0)))
(defscope :function app
          :import ((rator 0) (rand 0)))

(defsort Stx :import 1 :export 1)
(defun let (: binds Stx) (: body Expr)
           :sort Expr)
(defun more-binds (: x VarDef) (: e Expr) (: binds Stx)
                  :sort Stx)
(defun end-binds :sort Stx)
;; (defscope :function let :import ((binds 0) (body 0)) :bind ((body 0) (binds 0)))
(defmacro (let (more-binds 1 2 3) 4)
          (app (lambda 1 (let 3 4)) 2))
(defmacro (let (end-binds) 1) 1)

(infer)
(dump)
