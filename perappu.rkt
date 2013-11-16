#lang racket

; utility functions
(define (getNth L n)
  (if (zero? n) (car L)
      (getNth (cdr L) (sub1 n))
      )
  )

(define (getOne L) (getNth L (random (length L))) )

; here come the nouns
(define common-nouns '(acorn cloud icon skeleton paparazza bottle basketball egg piranha))
(define proper-nouns '("Michael Bloomberg" "Barack Obama" "Mamma Po" "Agamemnon" "GLaDOS" "Giovanni" "Orestes" "Cyrus" "Miley Cyrus" "Explorer X" "The Hulk"))
(define pokemon-species-names '("Plusle" "Minun" "Persian" "Pikachu" "Shaymin" "Flab\u00e9b\u00e9" "Mime Jr." "Arceus"))

(define (common-noun) (list (getOne common-nouns)))
(define (proper-noun) (list (getOne proper-nouns)))
(define (pokemon-species) (list (getOne pokemon-species-names)))
(define (pokemon-noun) (append (proper-noun) '('s) (pokemon-species)))

; attack of the verbs
(define transitive-verbs '(eat climb defeat slurp help draw destroy))
(define intransitive-verbs '(eat run walk appear race))

(define (transitive-verb) (list (getOne transitive-verbs)))
(define (intransitive-verb) (list (getOne intransitive-verbs)))

; adjective invasion
(define adjectives '(golden quick competent slow excited unworldly super-effective fallacious))

(define (adj) (list (getOne adjectives)))

; combo time
(define (noun-phrase)
  (cond
    ((= 0 (random 7)) (proper-noun) )
    ((= 0 (random 6)) (append (proper-noun) '(and) (proper-noun)))
    ((= 0 (random 5)) (cons 'a (common-noun)) )
    ((= 0 (random 4)) (cons 'the (common-noun)) )
    ((= 0 (random 3)) (cons 'the (append (adj) (common-noun))) )
    ((= 0 (random 2)) (cons (random 127) (common-noun)) )
    (else (pokemon-noun))
    )
  )

(define (predicate)
  (cond
    ((= 0 (random 4)) (intransitive-verb))
    ((= 0 (random 3)) (append (transitive-verb) (noun-phrase)) )
    ((= 0 (random 2)) (append (intransitive-verb) (adj) '(ly)) )
    (else (append (adj) '(ly) (transitive-verb) (noun-phrase)))
    )
  )

(define (passive-predicate)
  (append '(was) (transitive-verb)
          (if (= 0 (random 4)) (cons 'by (noun-phrase)) '() )
          )
  )

; the grand finale
(define (sentence)
  (cond
    ((= 0 (random 4)) (append (noun-phrase) (predicate) '(&) (predicate)))
    ((= 0 (random 3)) (append (noun-phrase) (predicate)) )
    ((= 0 (random 2)) (append (noun-phrase) (passive-predicate)))
    (else (append (noun-phrase) '(doesn't) (predicate)))
    )
  )

; Main function
; Run `(main i)` to print i random sentences to stdout
(define (main i)
  (if (= i 0)
      (newline)
      (begin
        (display (sentence))
        (newline)
        (main (sub1 i))
        )
      )
  )
