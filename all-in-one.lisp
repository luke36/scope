(defsort Expr :import 1 :export 0)

;; base language

(defun void :sort Expr)
(defscope void :import ())
(defun var (: x VarUse) :sort Expr)
(defscope var :import ((x 0)))
(defun lambda (: xs VarDef)* (: e Expr) :sort Expr)
(defscope lambda :import ((xs 0) (e 0)) :bind ((e 0) (xs 0)))
(defun app (: f Expr) (: es Expr)* :sort Expr)
(defscope app :import ((f 0) (es 0)))
(defun set! (: x VarUse) (: e Expr) :sort Expr)
(defscope set! :import ((x 0) (e 0)))
(defun if (: c Expr) (: t Expr) (: f Expr) :sort Expr)
(defscope if :import ((c 0) (t 0) (f 0)))

;; base language end

(defsort Let*Bind :import 1 :export 1)
(defun let* (: binds Let*Bind)* (: body Expr) :sort Expr)
(defun let*-bind (: x VarDef) (: e Expr) :sort Let*Bind)
(defmacro (let* (append (list (let*-bind 1 2)) 3) 4)
          (app (lambda (append (list 1)) (let* (append 3) 4)) (append (list 2))))
(defmacro (let* (append) 1) 1)

(infer)

(defsort LetBind :import 1 :export 1)
(defun let (: binds LetBind)* (: body Expr) :sort Expr)
(defun let-bind (: x VarDef) (: e Expr) :sort LetBind)
(defmacro (let (append (mapcar let-bind (append 1) (append 2))) 3)
          (app (lambda (append 1) 3) (append 2)))

(infer)

(defun begin (: e Expr)* :sort Expr)
(defmacro (begin (append)) (void))
(defmacro (begin (append (list 1))) 1)
(defmacro (begin (append (list 1) 2))
          (let (append (list (let-bind _ 1))) (begin (append 2))))

(infer)

(defsort LetrecBind :import 2 :export 1)
(defun rec-bind (: x VarDef) (: e Expr) :sort LetrecBind)
(defun letrec (: b LetrecBind)* (: e Expr) :sort Expr)
(defmacro
    (letrec (append (mapcar rec-bind (append 1) (append 2))) 3)
    (let (append (mapcar let-bind (append 1) (append (repeat (void)))))
      (begin (append (mapcar set! (append (->use 1)) (append 2)) (list 3)))))

(infer)

(defsort DoBind :import 2 :export 1)
(defun do-bind (: x VarDef) (: init Expr) (: step Expr) :sort DoBind)
(defun do (: binds DoBind)* (: cond Expr) (: body Expr) :sort Expr)
(defmacro
    (do (append (mapcar do-bind (append 1) (append 2) (append 3))) 4 5)
    (letrec (append (list (rec-bind loop (lambda (append 1)
                                           (if 4
                                               (app (var loop) (append 3))
                                               5)))))
      (app (var loop) (append 2))))

(infer)

;; no match; it has little to do with lists

(dump)
(exit)
