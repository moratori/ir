
(ql:quickload :yacc)
(ql:quickload :cl-lex)

(use-package :yacc)
(use-package :cl-lex)

(defmacro %deflexer% (name &rest clauses)
  `(define-string-lexer ,name 
        ,@(mapcar (lambda (clause)
                    (destructuring-bind (pattern kind) clause
                      `(,pattern (return (values ,kind $@))))) clauses)))

(defmacro defparser (name defterm &rest body)
  (let ((lexer (gensym))
        (parser (gensym)))
    `(progn 
       (%deflexer% ,lexer ,@defterm)
       (define-parser ,parser 
          (:terminals
            ,(mapcar #'second defterm))            
          ,@body)
       (defun ,name (target)
         (parse-with-lexer (,lexer target) ,parser)))))


(defpackage :IR-USER
  (:use :cl))
 

(defun upcase-intern (s)
  (intern (string-upcase s) :IR-USER))
  

(defun infix->prefix (a b c)
  `(,(upcase-intern b) ,a ,c))


(defparser program
  (("[0-9]+"      :number)         ;number
   ("_`\\|\\}"    :true)           ;true
   ("\\*<%!\\}"   :false)          ;false
   ("/\\}\\*\\|~" :declaration)    ;defun
   ("\\$\\*"      :condition)      ;if
   ("<~/"         :and)            ;and
   ("\"`"         :or)             ;or
   ("~\"_"        :not)            ;not
   ("~\\$%"       :nil)            ;nil
   ("a"           :lessthan)       ;<
   ("y"           :equal)          ;=
   ("\\)\"~!"     :cons)           ;cons
   ("\\)<`"       :car)            ;car
   ("\\)/`"       :cdr)            ;cdr
   ("b"           :plus)           ;+
   ("k"           :minus)          ;-
   ("f"           :mult)           ;*
   ("d"           :div)            ;/
   ("l"           :mod)            ;%
   ("h"           :sbrace)         ;{
   ("e"           :ebrace)         ;}
   ("g"           :sparen)         ;(
   ("c"           :eparen)         ;)
   ("j"           :sbracket)       ;[
   ("v"           :ebracket)       ;]
   ("q"           :comma)          ;,
   ("\\^[0-9]+"   :symbol))        ;^number 

  (:start-symbol program)
  (:precedence 
    ((:right :not)
     (:left  :mult)
     (:left  :div)
     (:left  :mod)
     (:left  :plus :minus)
     (:left  :equal)
     (:left  :lessthan)
     (:left  :and :or)
     (:right  :condition)))

  (program
    (expr 
      (lambda (x) `(progn ,x)))
    (program expr
     (lambda (x y)
       (append x (list y)))))

  (expr 
    (:declaration :symbol :sbracket varseq :ebracket :sbrace expr :ebrace
      (lambda (a b c d e f g h) 
        (declare (ignore a c e f h))
        `(progn (defun ,(upcase-intern b) ,d ,g) t)))
    (:condition expr expr expr
      (lambda (x y z w) 
        (declare (ignore x)) `(if ,y ,z ,w)))
    (:not expr
      (lambda (x y) 
        (declare (ignore x)) `(not ,y)))
    (expr :plus     expr 
          (lambda (x y z)
            (declare (ignore y)) `(+ ,x ,z)))
    (expr :minus    expr 
          (lambda (x y z)
            (declare (ignore y)) `(- ,x ,z)))
    (expr :mult     expr 
          (lambda (x y z)
            (declare (ignore y)) `(* ,x ,z)))
    (expr :div      expr
          (lambda (x y z)
            (declare (ignore y)) `(/ ,x ,z)))
    (expr :and      expr 
          (lambda (x y z)
            (declare (ignore y)) `(and ,x ,z)))
    (expr :or       expr
          (lambda (x y z)
            (declare (ignore y)) `(or ,x ,z)))
    (expr :lessthan expr
         (lambda (x y z)
            (declare (ignore y)) `(< ,x ,z)) )
    (expr :equal    expr
          (lambda (x y z)
            (declare (ignore y)) `(= ,x ,z)))
    (expr :mod      expr 
      (lambda (x y z) 
        (declare (ignore y)) `(mod ,x ,z)))
    (:cons :sbracket expr :comma expr :ebracket
      (lambda (x y z w v u) 
        (declare (ignore x y w u))
        `(cons ,z ,v)))
    (:car :sbracket expr :ebracket
      (lambda (x y z w)
        (declare (ignore x y w))
        `(car ,z)))
    (:cdr :sbracket expr :ebracket
      (lambda (x y z w)
        (declare (ignore x y w))
        `(cdr ,z)))
    (:symbol :sbracket exprseq :ebracket
      (lambda (x y z w) 
        (declare (ignore y w)) `(,(upcase-intern x) ,@z)))
    primary)

  (exprseq
    (expr #'list )
    (exprseq :comma expr      
      (lambda (a b c) 
        (declare (ignore b)) (append a (list c)))))

  (varseq
    (:symbol 
      (lambda (x) (list (upcase-intern x))))
    (varseq :comma :symbol
      (lambda (a b c) 
        (declare (ignore b))
        (append a (list (upcase-intern c))))))

  (primary 
    (:nil 
      (lambda (x) (declare (ignore x)) nil))
    (:symbol 
      (lambda (x) (upcase-intern x)))
    (:true 
      (lambda (x) (declare (ignore x)) t))
    (:false
      (lambda (y) (declare (ignore y)) nil))
    (:sparen expr :eparen 
      (lambda (a b c) (declare (ignore a c)) b))
    (:number #'parse-integer)))

(defun evaluation (source &optional (debug nil))
  (let ((val (eval 
               (if debug 
                 (print (program source)) 
                 (program source)))))
    (typecase val
      (number val)
      (boolean (if val "true" "false"))
      (t val))))

(defun prompt () (format t ">>> ") (force-output *standard-output*))

(defun repl ()
  (prompt)
  (loop for in = (read-line *standard-input* nil nil)
        while in
        do 
        (handler-case
          (let ((val (evaluation in)))
            (format *standard-output* "~A~%" val) 
            (prompt))
          (yacc-parse-error (c) (declare (ignore c))
            (format t "parse error occurred~%")
            (prompt))
          (error (c) (declare (ignore c))
            (format t "unexpected error occurred~%")
            (prompt))))
  (sb-ext:exit))


(defun main ()
  (in-package :IR-USER)
  (let ((argv sb-ext:*posix-argv*))
    (if (< (length argv) 2)
      (repl)
      (progn
        (with-open-file (in (second argv) :direction :input)
        (format *standard-output* "~%~A~%"
          (evaluation
            (loop 
              with result = ""
              finally (return result)
              for line = (read-line in nil nil)
              while line
              do (setf result (format nil "~A~%~A" result line)))))
        (sb-ext:exit))))))


(main)

