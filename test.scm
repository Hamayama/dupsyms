;;
;; Test dupsyms
;;

(add-load-path "." :relative)
(use gauche.test)

(test-start "dupsyms")
(use dupsyms)
(test-module 'dupsyms)

(define (filter-by-module result-list mod-list)
  (filter
   (^[item] (member (cadr item) mod-list))
   result-list))

(define-module mod-A
  (export aaa bbb f1)
  (define aaa 1000)
  (define bbb 2000)
  (define (f1 x) (+ x 10)))

(define-module mod-B
  (export aaa ccc f1)
  (define aaa 3000)
  (define ccc 4000)
  (define (f1 x y) (+ x y)))

(import mod-A)
(import mod-B)

(test-section "dupsyms")
(test* "dupsyms-1"
       '((aaa mod-A) (aaa mod-B) (f1 mod-A) (f1 mod-B))
       (filter-by-module (dupsyms) '(mod-A mod-B)))

;; summary
(format (current-error-port) "~%~a" ((with-module gauche.test format-summary)))

(test-end)

