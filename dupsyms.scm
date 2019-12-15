;; -*- coding: utf-8 -*-
;;
;; dupsyms.scm
;; 2019-12-15 v1.04
;;
;; ＜内容＞
;;   Gauche で、import されたシンボルの重複チェックを行うための
;;   モジュールです。
;;
;;   詳細については、以下のページを参照ください。
;;   https://github.com/Hamayama/dupsyms
;;
(define-module dupsyms
  (export
    dupsyms))
(select-module dupsyms)

;; check if the imported duplicate symbols exist.
;;  return is '((sym mod) (sym mod) ...)
;; (macro is required because current-module is determined
;;  at the compile time)
(define-syntax dupsyms
  (syntax-rules ()
    [(_) (%dupsyms (current-module))]))
(define (%dupsyms module)
  (get-duplicates (%apropos #/.*/ module #f) equal? car))


;; get the list consisting only of duplicate elements.
;;  e.g. '(1 2 2 3 4 4 4 5 6) ==> '(2 2 4 4 4)
;; (input list must be sorted before passing to this procedure)
(define (get-duplicates list :optional (cmpfn equal?) (keyfn identity))
  (let loop ([list list] [ret '()] [hit-prev #f])
    (if (null? list)
      (reverse ret)
      (let ([hit (and (not (null? (cdr list)))
                      (cmpfn (keyfn (car list)) (keyfn (cadr list))))])
        (loop (cdr list)
              (if (or hit hit-prev) (cons (car list) ret) ret)
              hit)))))


;; modified version of %apropos
;; ( the original is in lib/gauche/interactive.scm )
(define (%apropos item module stay-in-module)
  (let ([module (cond [(module? module) module]
                      [(symbol? module)
                       (or (find-module module)
                           (error "No such module: " module))]
                      [else (error "Bad object for module: " module)])]
        [matcher (cond [(symbol? item)
                        (let1 substr (symbol->string item)
                          (^[name] (string-scan name substr)))]
                       [(string? item)
                        ;; Note: future extension
                        (error "Bad object for item: " item)]
                       [(is-a? item <regexp>) (^[name] (rxmatch item name))]
                       [else
                        (error "Bad object for item: " item)])]
        [result '()]
        [searched '()])

    ;(define (search mod)
    (define (search mod :optional (exported-only #f))
      (define exp-syms (module-exports mod))
      (unless (memq mod searched)
        (set! searched (cons mod searched))
        (hash-table-for-each
         (module-table mod)
         (^[symbol value]
           ;(when (matcher (symbol->string symbol))
           (when (and (matcher (symbol->string symbol))
                      (or (not exported-only)
                          (and (boolean? exp-syms) exp-syms)
                          (memq symbol exp-syms)))
             (found mod symbol))))))

    (define (found module symbol)
      (set! result
            ;(cons (format #f "~30s (~a)~%" symbol (module-name module))
            (cons (list symbol (module-name module))
                  result)))

    ;; mimics the Scm_FindBinding
    (if stay-in-module
      (search module)
      ;(begin (for-each (^m (for-each search (module-precedence-list m)))
      ;                 (module-imports module))
      ;       (for-each search (module-precedence-list module))))
      (begin (search module)
             (for-each (cut search <> #t) (module-imports module))
             (for-each search (module-precedence-list module))))
    ;(for-each display (sort result))
    ;(values)
    (sort result string<? (^[item] (x->string (car item))))
    ))


