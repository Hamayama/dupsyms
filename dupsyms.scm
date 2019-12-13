;; -*- coding: utf-8 -*-
;;
;; dupsyms.scm
;; 2019-12-13 v1.01
;;
;; ＜内容＞
;;   Gauche で、import されたシンボルの重複チェックを行うための
;;   モジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/dupsyms
;;
(define-module dupsyms
  (use gauche.interactive)
  (export
    dupsyms))
(select-module dupsyms)

;; check if the imported duplicate symbols exist.
;;  return is '((sym mod) (sym mod) ...)
(define-syntax dupsyms
  (syntax-rules ()
    [(_) (%dupsyms (current-module))]))
(define (%dupsyms module)
  (define imported-list '())
  (with-input-from-string
      (with-output-to-string
        (^[] (with-module gauche.interactive (%apropos #/.*/ module #f))))
    (^[]
      (let loop ([sym (read)])
        (unless (eof-object? sym)
          (let ([mod (read)])
            (push! imported-list (cons sym mod))
            (loop (read)))))))
  (get-duplicates (reverse imported-list) equal? car))

;; get the list consisting only of duplicate elements.
;;  e.g. '(1 2 2 3 4 4 4 5 6) ==> '(2 2 4 4 4)
(define (get-duplicates list :optional (cmpfn equal?) (keyfn identity))
  (let loop ([list list] [ret '()] [hit-prev #f])
    (if (null? list)
      (reverse ret)
      (let ([hit (and (not (null? (cdr list)))
                      (cmpfn (keyfn (car list)) (keyfn (cadr list))))])
        (loop (cdr list)
              (if (or hit hit-prev) (cons (car list) ret) ret)
              hit)))))

