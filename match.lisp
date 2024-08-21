(defsort Expr :import 1 :export 0)
(defun var (: x VarUse) :sort Expr)
(defscope var :import ((x 0)))
(defun app (: rator Expr) (: rand Expr) :sort Expr)
(defscope app :import ((rator 0) (rand 0)))
(defun car (: p Expr) :sort Expr)
(defscope car :import ((p 0)))
(defun cdr (: p Expr) :sort Expr)
(defscope cdr :import ((p 0)))
(defun if (: cond Expr) (: conseq Expr) :sort Expr)
(defscope if :import ((cond 0) (conseq 0)))

(defsort LetBind :import 1 :export 1)
(defun let (: b LetBind) (: e Expr) :sort Expr)
(defscope let :import ((b 0) (e 0)) :bind ((e 0) (b 0)))
(defun more-bind (: x VarDef) (: e Expr) (: b LetBind) :sort LetBind)
(defscope more-bind :import ((x 0) (e 0) (b 0)) :export ((x 0) (b 0)))
(defun end-bind :sort LetBind)
(defscope end-bind :import () :export ())

(defsort Stx :import 2 :export 2)
(defun var-pat (: x VarDef) :sort Stx)
(defun pair-pat (: p1 Stx) (: p2 Stx) :sort Stx)
(defun aut-pat (: f Expr) (: x VarDef) :sort Stx)
(defun match (: e Expr) (: p Stx) (: g Expr) (: r Expr) :sort Expr)

(defun vp-cons (: x1 VarUse) (: x2 VarUse) (: p Stx) (: vp Stx) :sort Stx)
(defun vp-nil :sort Stx)

(defun bind-cons (: x VarDef) (: e Expr) (: bs Stx) :sort Stx)
(defun bind-nil :sort Stx)

(defun build-let (: bs Stx) (: e Expr) :sort Expr)
(defmacro (build-let (bind-nil) 1) 1)
(defmacro (build-let (bind-cons 1 2 3) 4)
          (build-let 3 (let (more-bind 1 2 (end-bind)) 4)))

(defun aux (: vps Stx) (: bs Stx) (: cp LetBind) (: g Expr) (: aut LetBind) (: r Expr) :sort Expr)
(defmacro (match 1 2 3 4)
          (aux (vp-cons e e 2 (vp-nil)) (bind-cons e 1 (bind-nil)) (end-bind) 3 (end-bind) 4))
(defmacro (aux (vp-nil) 1 2 3 4 5)
          (build-let 1 (let 2 (if 3 (let 4 5)))))
(defmacro (aux (vp-cons 1 2 (var-pat 3) 4) 5 6 7 8 9)
          (aux 4 5 (more-bind 3 (var 1) 6) 7 8 9))
(defmacro (aux (vp-cons 1 2 (pair-pat 3 4) 5) 6 7 8 9 10)
          (aux (vp-cons a a 3 (vp-cons d d 4 5))
               (bind-cons a (car (var 1)) (bind-cons d (car (var 2)) 6))
               7 8 9 10))
(defmacro (aux (vp-cons 1 2 (aut-pat 3 4) 5) 6 7 8 9 10)
          (aux 5 6 (more-bind x (var 1) 7) 8 (more-bind 4 (app 3 (var x)) 9) 10))

(infer)
(dump)
(regexp var car cdr let more-bind end-bind var-pat pair-pat aut-pat match)
(exit)
