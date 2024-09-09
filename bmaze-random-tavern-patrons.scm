(module bmaze-random-tavern-patrons ()

(import scheme
        (chicken base)
        (chicken port)
        (chicken process-context)
        (chicken random)
        (args)
        (bindings)
        (schemepunk show)
        (loop)
        (matchable)
        (srfi 1)
        )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rolling dice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (d0 n)
  ;; Zero-based dN roll, 0..(N-1)
  (pseudo-random-integer n))

(define (d n)
  ;; One-based dN roll, 1..N
  (+ 1 (d0 n)))

(define (make-dN N)
  (lambda ()
    (d N)))

(define d100 (make-dN 100))
(define d4 (make-dN 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Patrons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *patrons*
  ;; Barrowmaze Complete, p. 241.
  ;; Name Morning Afternoon Evening
  `(("Mazzahs the Magnificent" 1 1 1)
    ("Bollo the Barkeeper" 95 95 99)
    ("Taycee the Barmaid" 95 95 99)
    ("Urgritte the Barmaid" 95 95 99)
    ("Merida the Barmaid" 75 85 99)
    ("Karg Barrelgut the Smith" 10 10 65)
    ("Othar of St. Ygg" 1 1 5)
    ("Cella of St. Ygg" 1 1 5)
    ("Gamdar of St. Ygg" 10 10 10)
    ("Lord Kell Ironguard" 1 1 1)
    ("Krothos Ironguard" 10 12 15)
    ("Ollis Blackfell" 10 12 15)
    ("Valeron" 5 10 35)
    ("Guildmaster Osen" 10 10 65)
    ("Billworth Turgen" 5 10 50)
    ("Alaster the Idiot (outside)" 80 80 80)
    ("Perni Ticklebottom" 10 30 10)
    ("H.H.R. Huffnpuff" 10 10 35)
    ("Hendon the Miller" 5 5 20)
    ("Tamson, son of Hendon" 5 10 35)
    ("Yusef, second son of Hendon" 5 5 5)
    ("Alzo Danuth (50% in disguise)" 1 1 5 (disguise 50))
    ("Urnst Gunter (50% in disguise)" 1 1 5  (disguise 50))
    ("Villagers (farmers, etc) (2d4+1)" 30 60 90
     (number ,(lambda () (+ (d4) (d4) 1))))
    ("Renata the Robber (in disguise)" 5 5 5)
    ("The Norse Whisperers" 15 25 50)
    ("The Fearsome Fivesome" 15 25 50)
    ("The Bastards of Bogtown" 15 25 50)
    ("Bertrand’s Brigands" 15 25 50)
    ("The Outriders of Uleck" 15 25 50)
    ("Boon Companions" 15 25 50)
    ("Level 0 Men-at-Arms (1d4)" 15 25 60  (number ,d4))
    ("Level 1 Classed Henchmen (1)" 5 15 35)
    ("Porters/Torchbearers" 25 45 75)
    ))

(define *time-of-day* 'morning)
(define *number-of-times* 1)

(define +options+
  (list (args:make-option
            (h help) #:none "Display a help message and exit."
          (usage (current-output-port)))
        (args:make-option
            (m morning) #:none "It's morning."
          (set! *time-of-day* 'morning))
        (args:make-option
            (a afternoon) #:none "It's afternoon."
          (set! *time-of-day* 'afternoon))
        (args:make-option
            (e evening) #:none "It's evening."
          (set! *time-of-day* 'evening))
        (args:make-option
            (n number) #:required "Number of times to generate."
          (set! *number-of-times* (string->number arg)))))

(define (usage port)
  (with-output-to-port port
    (lambda ()
      (show #t "Adamant Entertainment Adventure Generators" nl)
      (show #t "Usage: "  (car (argv)) " [options...]" nl)
      (show #t (args:usage +options+) nl)
      (show #t "Report bugs to tkurtbond at gmail.com")
      (exit 1))))

(define (tod->string tod)
  (case tod
    ((morning)
     "morning")
    ((afternoon)
     "afternoon")
    ((evening)
     "evening")
    (else
     (assert #f (show #f "Impossible tod: "
                      tod)))))

(define (patron-present patron time-of-day)
  (bind (name morning afternoon evening . rnd)
      patron
    #;(show (current-error-port) "name: " name " morning: " (written morning)
          " afternoon: " (written afternoon) " evening: " (written evening) " rnd: " (written rnd) nl)
    (define chance (case time-of-day
                     ((morning) morning)
                     ((afternoon) afternoon)
                     ((evening) evening)))
    (define r (d100))
    (define present? (<= r chance))
    (define additional
      (match rnd
        ('() #f)
        ((('number f))
         ;;(show (current-error-port) (written (loop for i from 1 to 100 collect (f))) nl) 
         (show #f " (number appearing: " (numeric (f)) ")"))
        ((('disguise chance)) (if (<= (d100) chance) " (in disguise)" " (as themself)"))))
    ;; (show (current-error-port) "chance: " chance " r: " (written r) " present: " (written present?) " additional: " additional nl)
    (if present?
        (if additional
            (string-append name additional)
            name)
        #f)
    ))

(define (print-inhabitants)
  (let* ((l1 (reverse (filter identity
                              (loop for patron in *patrons*
                                    collect (patron-present patron *time-of-day*)))))
         (l2 (if (> (length l1) 1)
                 (cons (string-append "and " (car l1)) (cdr l1))
                 l1))
         (l3 (if (>= (length l2) 1)
                 (cons (string-append (car l2) ".") (cdr l2))
                 l2))
         (l3 (loop for i from 1
                   for patron in l3
                   if (> i 1) collect (string-append patron ",")
                   else collect patron)))
    ;;(show #t nl (written l1) nl nl (written l2) nl nl (written l3) nl nl)
    (show #t (wrapped/list (append (list "It's" (tod->string *time-of-day*)
                                         "and" "who" "is" "in" "the" "Brazen"
                                         "Strumpet?")
                                   (reverse l3))) nl)))

(define (main)
  (receive (options operands) (args:parse (command-line-arguments)
                                          +options+)
    (loop for i from 1 to *number-of-times*
          do (print-inhabitants)
          when (< i *number-of-times*) do (show #t nl))))

(cond-expand
  (compiling
   (main))
  (else)) 
)
