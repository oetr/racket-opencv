#|
opencv-lib.rkt: Finds the opencv library files
Copyright (C) Peter Samarin
|#

;; TODO: make this library define ffi-objects based on system type

(module opencv-lib racket
  ;; eval the require in order to provide a
  ;; "define-ffi-definer" and "ffi-lib"  
  (eval
   '(require ffi/unsafe
             ffi/unsafe/define))

  (define try-paths
    (case (system-type)
      ((macosx)
       (list "/usr/local/lib"
             "/opt/local/lib"
             "/sw/local/lib"))
      (else '())))


  (define opencv-lib "opencv")

  ;; find-all-libraries: path string -> ((symbol path) ...)
  ;; to find all libraries that match the provided library name
  ;; on the path
  ;; returns false if no matches found
  ;; returns a list with library name prepended "define-" as a symbol
  ;; and the corresponding path
  (define (find-all-libraries path library)
    ;; get the names of all files that belong to the library
    (define lib-files
      (if (directory-exists? path)
        (filter
         (lambda (a-file)
           (regexp-match
            (pregexp
             (string-append
              library
              "\\w*" (bytes->string/locale (system-type 'so-suffix))))
            a-file))
         (directory-list path))       
        #f))
    ;; remove extension and convert underscores
    (if lib-files
      (let ([lib-paths
             (map (lambda (a-p) (path->complete-path a-p path))
                  lib-files)])
        (map list
             (map (lambda (a-string)
                    (string->symbol
                     ;; append "define-" to the name of the library
                     (string-append
                      "define-"
                      (regexp-replace*
                       "_"
                       (car (regexp-match
                             (pregexp
                              (string-append library ".*"))
                             (path->string (path-replace-suffix a-string ""))))
                       "-"))))
                  lib-files)
             (map path->string lib-paths)))
      #f))

  (define-syntax (defchipmunk stx)
    (syntax-case stx ()
      [(defchipmunk name #:ptr type)
       #`(begin (provide name)
                (define name
                  (let ()
                    (define-chipmunk ptr _pointer
                      #:c-id #,(datum->syntax
                                #'name
                                (string->symbol
                                 (format "_~a" (syntax->datum #'name)))))
                    (function-ptr ptr type))))]
      [(defchipmunk name type)
       #'(begin (provide name)
                (define-chipmunk name type))]))


  ;; find the libs
  (define libs
    (ormap
     (lambda (a-path)
       (find-all-libraries a-path opencv-lib))
     try-paths))

  ;; define and provide all the library names as ffi definers
  (andmap (lambda (a-list)
            (printf "~a~n" (car a-list))
            (eval `(define-ffi-definer
                     ,(car a-list)
                     (ffi-lib ,(cadr a-list)))))
          libs))
