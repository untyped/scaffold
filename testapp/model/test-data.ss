#lang scheme

(require "../base.ss"
         "db.ss"
         "entities.ss")

; Helpers ----------------------------------------

; string thunk -> void
(define (with-message message thunk)
  (printf "~a ... " message)
  (with-handlers ([exn? (lambda (exn) (printf "[FAIL]~n"))])
    (thunk)
    (printf "[DONE]~n")))

; string thunk [thunk] -> any
(define (ask-question question yes-thunk [no-thunk void])
  (printf "~a [YN] " question)
  (if (regexp-match #rx"(?i:ye?s?)" (read-line))
      (yes-thunk)
      (no-thunk)))

; Program body -----------------------------------

(define (recreate-test-data)
  (call-with-connection
   (lambda ()
     (ask-question
      "Recreate tables?"
      (lambda ()
        (let ([entities (list post kitchen-sink)])
          (for-each drop-table (reverse entities))
          (for-each create-table entities))))
     (ask-question
      "Create sample data?"
      (lambda ()
        (with-message
         "Creating sample data"
         (lambda ()
           (for ([index (in-range 100)])
             (save! (make-post (format "Post ~a" index)
                               (string-append (format "This is post number ~a." index)
                                              #<<ENDPOST

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Sed sed nisi nec urna rutrum pharetra. Morbi lectus. Fusce ultricies odio ac metus. Aliquam fringilla mauris vel purus. Nullam ornare semper tellus. Quisque consequat luctus nibh. Cras malesuada suscipit orci. Cras vel quam id risus dictum adipiscing. Ut tortor. Fusce nisi enim, fermentum sed, mattis nec, posuere vitae, metus. In sit amet enim. In hac habitasse platea dictumst. Quisque nunc magna, mollis ac, pharetra sit amet, semper eget, erat. In pellentesque. Sed viverra ipsum fermentum risus. Duis nisi nulla, pellentesque id, vehicula et, vehicula ut, mi. Suspendisse ornare, turpis eu imperdiet placerat, justo eros adipiscing libero, nec fermentum pede metus at nulla.

Donec in eros eu nibh fringilla varius. Suspendisse hendrerit hendrerit velit. Maecenas malesuada enim sit amet odio. Cras ac magna. Phasellus enim. Donec dapibus dapibus lectus. Aliquam varius vehicula nisi. Suspendisse potenti. Proin felis. Nunc volutpat erat convallis neque. Duis sagittis.

Nulla viverra, ante a semper vulputate, dui velit dictum orci, sit amet tempus diam est vitae nisl. Mauris porta quam vel nisl. Quisque facilisis tempor mauris. Suspendisse at risus eu odio malesuada egestas. Phasellus bibendum augue et nunc. Nam at mauris sit amet dolor malesuada cursus. Mauris vitae nisl eu justo bibendum porta. Mauris a purus eget sapien vehicula auctor. Sed vitae lorem. Quisque eros. Vestibulum sollicitudin. Nullam eu erat. Vestibulum fringilla purus imperdiet urna. Suspendisse porttitor diam ut augue.

Nam aliquam diam ut nisi. Nulla leo risus, congue eget, rutrum vel, pellentesque at, mi. Vivamus ornare pede nec urna. Ut vitae sapien eget libero varius interdum. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Vivamus arcu. Sed magna massa, euismod sit amet, commodo eu, posuere nec, neque. Nulla ipsum. Vestibulum sed lacus. Phasellus mattis accumsan nunc. Quisque turpis lorem, mollis sed, dictum in, pretium eu, sem.

Vestibulum vitae diam. Aliquam vulputate tempus arcu. Cras consequat vestibulum magna. Pellentesque sem. Proin bibendum. Quisque sed lectus. Cras ut sapien. Nulla facilisis tempus ipsum. Ut vulputate mauris et enim. Integer a augue. Nullam nec odio. Curabitur tempor mi eu mauris. Sed et nulla. Mauris massa justo, pulvinar at, imperdiet ac, dictum vitae, nunc. Aliquam eget enim sed augue accumsan auctor. Praesent est. Nam quis quam. Nam pellentesque magna luctus leo. Cras mollis elementum est.

ENDPOST
                                              )))))))))))
