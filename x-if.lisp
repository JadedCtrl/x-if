;; This file is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; TODO: get x-if.lexicon working, so the lexicon can be modified on-the-fly
;;       by x-if.environment, when new objects are created.
;;       also add :synonyms and :adjectives to the GOD object class

;; —————————————————————————————————————
;; PACKAGES

(defpackage :x-if
  (:use :cl :arnesi)
  (:nicknames :xif))

(defpackage :x-if.lexicon
  (:use :cl :arnesi :earley-parser)
  (:nicknames :xif.l)
  (:export :action-indirect-required-p :action-direct-required-p
           :action-p :action-function
           *lexicon* :add-adjective :add-noun :add-proper-noun))

(defpackage :x-if.environment
  (:use :cl :arnesi :bknr.datastore)
  (:nicknames :xif.e)
  (:export :name→object :all-objects :id→object :get-player
           :adjectives :description :id :name :synonyms :adjectives
           :location→entity :all-entities
           :inventory :max-inventory :weight :hp :max-hp))

(defpackage :x-if.parsing
  (:use :cl :arnesi)
  (:nicknames :xif.p)
  (:export :parse
           :noun-phrases :det :prep :proper-noun :noun-name :det :prep
           :nominal-phrase :noun
           :verb-phrase :verb :direct-object :indirect
           :the-action :the-subject))

(defpackage :x-if.interpret
  (:use :cl :arnesi)
  (:nicknames :xif.i))

(defpackage :x-if.client
  (:use :cl :arnesi :xif.e :bknr.datastore)
  (:nicknames :xif.c)
  (:export :start
           :text :input-sentence :display-options :input-options :play-music
           :show-image :status :prompt))

(defpackage :x-if.client.terminal
  (:use :cl :arnesi :xif.e)
  (:nicknames :xif.c.t))




;; —————————————————————————————————————
;; X-IF.PARSING
;; SENTENCE PARSING

(in-package :x-if.parsing)

;; —————————————————————————————————————

;; This package will take a sentence (string) and turn it into an
;; easily-parseable tree. It also provides some functions to make parsing this
;; tree easy. It does not, however, actually “process” the sentence for the
;; game engine itself— that's :x-if.interpret's job.

;; It doesn't define the lexicon, either. That's :x-if.environment's job.

;; This uses Earley parse-trees from :cl-earley-parser— so, when a function
;; says "tree", it means a list with a identifying car and several sublists.
;; For example, here's a noun-phrase's tree:

;; ("NP" ("proper-noun" "Princess") (prep "beside"))

;; A "full tree" is the tree of an entire statement— not a sub-tree thereof.
;; The only functions that require the full statement tree is #'the-subject
;; and #'the-action, so don't sweat it.

;; —————————————————————————————————————

;; STRING → LIST_OF_TREES
(defun parse (sentence)
  "Parse a given string into a list of Earley trees."
  (mapcar #'car
  (mapcar #'parse-statement
          (mapcar #'clean-statement(split-statements sentence)))))

;; —————————————————————————————————————

;; STRING → TREE
(defun parse-statement (statement)
  "Parse a given statement into an Earley tree."
  (earley-parser:chart-listing->trees
    (earley-parser:earley-parse statement
      (earley-parser:load-bnf-grammar #p"example/grammar.txt")
      (earley-parser:load-lexicon #p"example/lexicon.txt"))))

;; —————————————————————————————————————

;; STRING → LIST_OF_STRINGS
(defun split-statements (sentence)
  "Split up a string into different statements, based on punctuation."
  (cl-strings:split sentence ","))

(defun clean-statement (statement) statement)

;; —————————————————————————————————————

;; TREE → LIST_OF_TREES_OF_NOUN_PHRASES
(defun noun-phrases (tree)
  "Return the noun-phrases within a given tree."
  (let ((phrase nil))
    (loop :while   (setf phrase (assoc "NP" (cdr tree) :test #'equal))
          :collect phrase
          :do      (setq tree (remove phrase tree)))))

;; TREE → TREE_OF_VERB_PHRASE
(defun verb-phrase (tree)
  "Return a tree's verb-phrase."
  (assoc "VP" (cdr tree) :test #'equal))

;; TREE_OF_VERB_PHRASE → TREE_OF_NOUN_PHRASE
(defun direct-object (verb-phrase)
  "Return a verb-phrase's direct object."
  (loop :for n-p :in (noun-phrases verb-phrase)
        :if (not (prep n-p))
        :return n-p))

;; TREE_OF_VERB_PHRASE → TREE_OF_NOUN_PHRASE
(defun indirect-object (verb-phrase)
  "Return a verb-phrase's indirect object."
  (loop :for n-p :in (noun-phrases verb-phrase)
        :if (prep n-p)
        :return n-p))

;; TREE_OF_NOUN_PHRASE → TREE_OF_NOMINAL_PHRASE
(defun nominal (noun-phrase)
  "Return the nominal of a noun— how it's referred to."
  (assoc "nominal" (cdr tree) :test #'equal))

;; —————————————————————————————————————

;; TREE_OF_VERB_PHRASE → CONS
(defun verb (tree)
  (assoc "verb" (cdr tree) :test #'equal))

;; TREE_OF_NOUN_PHRASE → CONS
(defun prep (noun-phrase)
  (assoc "prep" (cdr noun-phrase) :test #'equal))

;; TREE_OF_NOMINAL_PHRASE → STRING
(defun noun (nominal-phrase)
  "Return the name of a noun's nominal phrase."
  (cadr (assoc "noun" (cdr nominal-phrase) :test #'equal)))

;; TREE_OF_NOUN_PHRASE → STRING
(defun det (noun-phrase)
  "Return a noun-phrase's det attr— 'the', 'a', 'this', etc."
  (cadr (assoc "det" (cdr noun-phrase) :test #'equal)))

;; TREE_OF_NOUN_PHRASE → STRING
(defun proper-noun (noun-phrase)
  "Return the proper noun of a noun-phrase."
  (cadr (assoc "proper-noun" (cdr noun-phrase) :test #'equal)))

;; TREE_OF_NOUN_PHRASE → STRING
(defun noun-name (noun-phrase)
  "Return the name of a noun, whether it's a proper noun or not."
  (if (nominal noun-phrase)
    (noun (nominal noun-phrase))
    (proper-noun noun-phrase)))

;; —————————————————————————————————————

;; FULL_TREE → TREE_OF_VERB_PHRASE
(defun the-action (full-tree)
  "Return THE main action of a sentence— takes the root of a statement's  tree."
  (verb-phrase full-tree))

;; FULL_TREE → TREE_OF_NOUN_PHRASE
(defun the-subject (full-tree)
  "Return THE subject of a sentence— AKA, the first noun-phrase. Assumes
  subject-verb-etc order."
  (car (noun-phrases full-tree)))




;; —————————————————————————————————————
;; X-IF.LEXICON
;; MANAGEMENT OF EARLEY-PARSER'S LEXICON

(in-package :x-if.lexicon)

;; —————————————————————————————————————

(defvar *lexicon* "")
(defvar *actions* (make-hash-table :test #'equal))

(defmacro add-string-to-var (var string)
  `(setq ,var (concatenate 'string ,var ,string)))

(defun add-word (name class)
  (add-string-to-var *lexicon* (format nil "~A :class \<~A\>~%" name class)))

(defmethod add-noun ((name string))
  (add-word name "noun"))

(defmethod add-noun ((object xif.e::god))
  (mapcar #'add-noun (xif.e:synonyms object))
  (add-noun (xif.e:name object)))

(defmethod add-adjective ((adj string))
  (add-word adj "adjective"))

(defmethod add-adjective ((adjectives xif.e::god))
  (mapcar #'add-adjective (xif.e:adjectives object)))

(defmethod add-verb ((verb string))
  (add-word verb "verb"))

(defun add-action (


;; —————————————————————————————————————
;; X-IF.INTERPRET
;; SENTENCE INTERPRETATION

(in-package :x-if.interpret)

;; —————————————————————————————————————

;; This package will take a parsed sentence (earley tree) and return proper
;; actions to be taken based on the sentence.

;; #'interpret will return either a list— of a function-name, followed by
;; x-if.environment objects with identifying symbols, or an error string with
;; symbol signifying the error as a second return-value.

;; The actionable list (function-name, followed by arguments to said function)
;; will look like this, for example:
;;  '(#'kill :direct <XIF.E:MAN> :indirect <XIF.E:KNIFE> :subject <XIF.E:PLAYER>)
;; if you give it the sentence
;;   "Kill the man with the knife."
;; … assuming that the knife is a valid object (in inventory or room) and the
;; man is is valid as well (in room or inventory).

;; If, for example, either the knife or man are invalid, an appropriate string
;; will be returned instead, to be printed as an error.
;; For instance: "you can't do that" or "that object isn't here", in cases
;; where the word is in lexicon but not currently applicable.
;; It will also return an appropriate error symbol, as a second value—
;; I.E., 'INVALID-ACTION, etc.

;; If a word *isn't* in lexicon, then #'x.if.parsing:parse will return an
;; error-string instead of a parsed Earley tree— which means that #'interpret
;; will recieve that error string from the engine. In this case, #'interpret
;; will return the string as if it were its own error-string, but with an
;; error-symbol of 'PARSE-DIE.

;; Anyway, ultimately the engine will actually execute the actionable list
;; generated by #'interpret.

;; —————————————————————————————————————

;; TREE_OF_STATEMENT → LIST || (STRING SYMBOL)
(defmethod interpret ((statement-tree list))
  "Actually interpret a parsed statement-tree; returns a list with the
  applicable function-name for the action, and the objects for the direct and
  indirect objects, as well as the subject."
  (let* ((subject  (or (the-subject statement-tree) (xif.e:get-player)))
         (action   (the-action statement-tree))
         (verb     (verb action))
         (indirect (xif.e:name→object (indirect-object action)))
         (direct   (xif.e:name→object (direct-object action))))
    (cond ((not (xif.l:action-p verb))
           "That… that's just not a thing people do.")
          ((and (not indirect) (xif.l:action-indirect-required-p verb))
           (values "With what?" 'NO-INDIRECT))
          ((and (not direct)   (xif.l:action-direct-required-p verb))
           (values "To what?" 'NO-INDIRECT))
          (T
           (list (xif.l:action-function verb)
                 :subject subject :indirect indirect :direct direct)))))

           
           

;; —————————————————————————————————————
;; X-IF.ENVIRONMENT

(in-package :x-if.environment)

;; —————————————————————————————————————

(define-persistent-class god ()
  ((id :read
      :initarg :id :reader id
      :index-type bknr.datastore::unique-index
      :index-initargs (:test #'equal)
      :index-reader id→object
      :index-values all-objects)
   (name :read
      :initarg :name :reader name
      :index-type bknr.datastore::hash-index
      :index-initargs (:test #'equal)
      :index-reader name→object)
   (proper-name :read
      :initarg :proper-name :reader proper-name
      :index-type bknr.datastore::hash-index
      :index-initargs (:test #'equal)
      :index-reader proper-name→object
      :initform nil)
   (adjectives :read
      :initarg :adjectives :reader adjectives
      :initform nil)
   (synonyms :read
      :initarg :synonyms :reader synonyms
      :initform nil)
   (description :read
      :initarg :desc :reader description
      :initform nil)))

(define-persistent-class entity (god)
  ((location :read
      :initarg :location :reader location
      :index-type bknr.datastore::hash-index
      :index-initargs (:test #'equal)
      :index-reader location→entity
      :index-values all-entities)
   (inventory :read
      :initarg  :inventory :reader inventory
      :initform nil)
   (max-inventory :read
      :initarg :max-inventory :reader max-inventory
      :initform 0)
   (weight :read
      :initarg :weight :reader weight
      :initform 0)
   (hp :read
      :initarg :hp :reader hp
      :initform nil)
   (max-hp :read
       :initarg :max-hp :reader max-hp
       :initform nil)))

(define-persistent-class npc (entity) ((normie :read :initform T :index-values all-npcs)))

(define-persistent-class player (npc)
  ((mlg :read
        :initform T :index-values get-player)))

(define-persistent-class location (entity)
  ((extreme-makeover-home-edition :read
                                  :initform T :index-values all-locations)))

(define-persistent-class fareblo ()
  ((function-name :read
      :initarg :function :reader function-name
      :index-type bknr.datastore::hash-index
      :index-reader function→action)
   (verbs :read
       :initarg :verbs :reader verbs)
   (


(defun get-player ()
  (id→object 100))




;; —————————————————————————————————————
;; X-IF.CLIENT

(in-package :x-if.client)

;; —————————————————————————————————————

;; This is the general client package— everything else uses it.

;; —————————————————————————————————————

(defun status ()
  (format nil "~A~%" (slot-value (get-player) 'xif.e::location)))

(defun prompt () ">> ")

(defun game-loop ()
  (let ((m 1))
    (display-status)
    (setq m (input-sentence))
    (text "You said: ~A~%" m)
    (text "~A~%" (xif.e:all-objects))
    (text "I LOVE YOU~%")
    (sleep 2)
    (game-loop)))

(defun start (game)
  (make-instance 'mp-store :directory #p"~/.local/share/x-if/"
                 :subsystems (list (make-instance 'store-object-subsystem)))
  (if (not (all-objects))
    (populate-world))
  (game-loop))

(defun populate-world ()
  (make-instance 'xif.e::location :name "Lobby" :id 0
                 :description "It's rather ugly, really.")
  (make-instance 'xif.e::npc :name "Barry"      :id 101
                 :description "He looks suspicious, no?"
                 :location (id→object 0))
  (make-instance 'xif.e::player :name "Maria"   :id 100
                 :description "A rather hideous lass."
                 :location (id→object 0)))




;; —————————————————————————————————————
;; X-IF.CLIENT.TERMINAL

(in-package :x-if.client.terminal)

;; —————————————————————————————————————

;; This package is a client package— it uses only basic terminal input/output,
;; so ought to be the most portable possible. No image or music support.

;; —————————————————————————————————————

(defun xif.c::text (string &rest format-args)
  (apply #'format (nconc (list t string) format-args)))

(defun xif.c::input-sentence ()
  (read-line))
