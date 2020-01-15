;; This file is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License
;; as published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; TODO: Get :x-if.interpret to make ACTION objects.

;; —————————————————————————————————————
;; PACKAGES

(defpackage :x-if
  (:use :cl)
  (:nicknames :xif))

(defpackage :x-if.misc
  (:use :cl :anaphora)
  (:nicknames :xif.m)
  (:export :line-cdr :line-car
           :line-length :line-position
           :in-string-p
           :remove-line
           :position-equal))

(defpackage :x-if.lexicon
  (:use :cl :earley-parser)
  (:nicknames :xif.l)
  (:export :action-indirect-required-p :action-direct-required-p
           :action-p :action-function :add-action :add-verb :remove-word
           *string-lexicon* *lexicon*
           :add-game-object-words :delete-game-object-words
           :add-adjective :add-noun :add-proper-noun :add-verb
           :reload-lexicon))

(defpackage :x-if.environment
  (:use :cl :bknr.datastore)
  (:nicknames :xif.e)
  (:export :id→game-object :get-player
           :all-game-objects
           :noun→game-objects :proper-noun→game-objects :adjective→game-objects
           :all-entities
           :noun→entities :proper-noun→entities :adjective→entities
           :all-mobs
           :noun→mobs :proper-noun→mobs :adjective→mobs
           :noun→locations :proper-noun→locations :adjective→locations
           :all-locations
           :id :description
           :nouns :proper-nouns :adjectives
           :max-children :weight :hp :max-hp
           :children :parent
           :link :unlink))

(defpackage :x-if.parsing
  (:use :cl)
  (:nicknames :xif.p)
  (:export :parse
           :noun-phrases :det :prep :proper-noun :noun-name :det :prep
           :nominal-phrase :noun
           :verb-phrase :verb :direct-object :indirect
           :the-action :the-subject))

(defpackage :x-if.interpret
  (:use :cl)
  (:nicknames :xif.i))

(defpackage :x-if.client
  (:use :cl :xif.e :bknr.datastore)
  (:nicknames :xif.c)
  (:export :start
           :text :input-sentence :display-options :input-options :play-music
           :show-image :status :prompt))

(defpackage :x-if.client.terminal
  (:use :cl :xif.e)
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
          (mapcar #'clean-statement (split-statements sentence)))))

;; —————————————————————————————————————

;; STRING → TREE
(defun parse-statement (statement)
  "Parse a given statement into an Earley tree."
  (earley-parser:chart-listing->trees
    (earley-parser:earley-parse statement
      (earley-parser:load-bnf-grammar #p"example/grammar.txt")
      xif.l:*lexicon*)))

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

(defvar *string-lexicon*)
(setq *string-lexicon*
"the :class <det>
that :class <det>
this :class <det>
here :class <prep>
there :class <prep>
above :class <prep>
below :class <prep>
beside :class <prep>
across :class <prep>
be :class <aux>
to :class <prep>
with :class <prep>
a :class <det>
an :class <det>
")

(defvar *lexicon* nil)


;; STRING STRING → NIL
(defun add-word (word class)
  "Add a word of the given class to the string-lexicon and parsed lexicon."
  (setq *string-lexicon*
        (concatenate 'string *string-lexicon*
                     (format nil "~A :class \<~A\>~%" word class)))
  (reload-lexicon))


;; STRING → NIL
(defun remove-word (word)
  "Remove a word from the string-lexicon and parsed lexicon."
  (when (is-word-p word)
    (setq *string-lexicon*
          (xif.m:remove-line *string-lexicon* word
                             :test #'cl-strings:starts-with))
    (reload-lexicon)))

;; STRING → BOOLEAN
(defun is-word-p (word)
  "Return whether or not a given word is in the lexicon."
  (xif.m:line-position *string-lexicon* word :test #'cl-strings:starts-with))


;; SYMBOL SYMBOL STRING → (DEFUN … )
(defmacro defun-add-wordclass (function-name class-symbol class-string)
  "Define the add-WORD function of the given word-class. #'add-noun, etc."
  `(defun ,function-name (,class-symbol)
     ,(format nil "Add a word of class ~A to the lexicon." class-string)
     (add-word ,class-symbol ,class-string)))

(defun-add-wordclass add-proper-noun proper-noun "proper-noun")
(defun-add-wordclass add-noun noun "noun")
(defun-add-wordclass add-adjective adjective "adjective")
(defun-add-wordclass add-verb verb "verb")


;; GAME-OBJECT → NIL
(defmethod add-game-object-words ((object xif.e::game-object))
  "Add a game-object's words (nouns, adjectives, proper-nouns) to the lexicon."
  (mapcar #'add-adjective (xif.e:adjectives object))
  (mapcar #'add-proper-noun (xif.e:proper-nouns object))
  (mapcar #'add-noun (xif.e:nouns object)))

;; GAME-OBJECT → NIL
(defmethod remove-game-object-words ((object xif.e::game-object))
  "Remove a game-object's words (nouns, adjectives, proper-nouns) to the lexicon."
  (mapcar #'xif.l:remove-word (xif.e:nouns object))
  (mapcar #'xif.l:remove-word (xif.e:proper-nouns object))
  (mapcar #'xif.l:remove-word (xif.e:adjectives object)))

;; STRING → LEXICON
(defun load-string-lexicon (lex-string)
  "Read all words from a dictionary file into a lexicon and a part of speech.
  Abridged version of #'earley-parser:load-lexicon (which reads from a file)."
  (with-input-from-string (lex-str-stream lex-string)
  (let ((lexicon (make-hash-table :test earley-parser::*string-comparer*))
        (part-of-speech nil))
    (loop :while (listen lex-str-stream)
          :do (let ((w (earley-parser::read-lexicon-line lex-str-stream)))
                (pushnew (earley-parser::terminal-class w)
                         part-of-speech
                         :test earley-parser::*string-comparer*)
                (push w (gethash (earley-parser::terminal-word w) lexicon))))
    (earley-parser::make-lexicon :dictionary lexicon
                                 :part-of-speech part-of-speech))))

;; NIL → NIL
(defun reload-lexicon ()
  "Updates the lexicon by parsing the string-lexicon."
  (setq *lexicon* (load-string-lexicon *string-lexicon*)))




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

;; If a word *isn't* in lexicon, or something else crops up, then
;; #'x.if.parsing:parse will return an error symbol instead of a queued-action
;; object— which means that #'interpret will recieve that error symbol from the
;; engine. In this case, #'interpret will return the symbol as if it were its
;; own error-symbol, but with an error-symbol of 'PARSE-DIE.
;; If the error is in the interpretation method, it'll obviously return the
;; second error-symbol as 'INTERPRET-DIE.

;; Anyway, ultimately the engine will actually execute the actionable list
;; generated by #'interpret.

;; —————————————————————————————————————

;; TODO: Obviously, if there are multiple matches it should error TF out
;; and die, and... and... AHHHH good luck ;w;

;; TREE_OF_STATEMENT → LIST || (SYMBOL SYMBOL)
(defmethod interpret ((statement-tree list))
  "Actually interpret a parsed statement-tree; returns a list with the
  applicable function-name for the action, and the objects for the direct and
  indirect objects, as well as the subject."
  (let* ((subject  (or (the-subject statement-tree) (xif.e:get-player)))
         (action   (the-action statement-tree))
         (verb     (verb action))
         (indirect (xif.e:noun→game-objects (indirect-object action)))
         (direct   (xif.e:noun→game-objects (direct-object action))))
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
;; OBJECTS, GAME-STATE, ETC.

(in-package :x-if.environment)

;; —————————————————————————————————————

;; This package contains all aspect of the actual game 'environment'—
;; all objects (rooms, entities, etc), means of accessing and modifying them,
;; etc.

;; There is a main overarching class (game-object), of which everything else is
;; derived. From there, there are two divering subclasses— the 'location'

;; —————————————————————————————————————
;; CLASSES

;; The overarching class
(define-persistent-class game-object ()
  ((id :read
      :initarg :id :reader id
      :index-type bknr.datastore::unique-index
      :index-initargs (:test #'equal)
      :index-reader id→game-object
      :index-values all-game-objects)
   (nouns :update
      :initarg :nouns :reader nouns)
   (proper-nouns :update
      :initarg :proper-name :reader proper-nouns
      :initform nil)
   (adjectives :update
      :initarg :adjectives :reader adjectives
      :initform nil)
   (description :update
      :initarg :desc :reader description
      :initform nil)
   (parent :update
      :initarg :parent :reader parent
      :initform nil)
   (children :update
      :initarg :children :reader children
      :initform nil)
   (max-children :update
      :initarg :max-children :reader max-children
      :initform nil)))

;; For any object that can interacted with
(define-persistent-class entity (game-object)
  ((hp :update
      :initarg :hp :reader hp
      :initform nil)
   (max-hp :update
       :initarg :max-hp :reader max-hp
       :initform nil)
   (weight :update
       :initarg :weight :reader weight
       :initform 0)))

;; These classes only exist for indexing and semantic purposes;
;; they are identical to their super-classes

;; For NPCs, animals, etc.
(define-persistent-class mob (entity) ())

;; For the PLAYER.
(define-persistent-class player (mob) ())

;; For LOCATIONS (rooms and such).
(define-persistent-class location (game-object) ())


;; For hypothetical ACTIONS;
;; that is, commands the player can enter which will execute a given function.
(define-persistent-class action ()
  ((function-name :read
      :initarg :function :reader function-name)
      ; :index-type bknr.datastore::hash-index
      ; :index-reader function→action
   (verbs :read
       :initarg :verbs :reader verbs)
       ; :index-type bknr.datastore::hash-index
       ; :index-reader verb→action
   (direct-object-p :read
       :initarg :direct-object-p :initform nil :reader direct-object-required-p)
   (indirect-object-p :read
       :initarg :indirect-object-p :initform nil :reader indirect-object-required-p)))


;; For tuŝeblaj ACTIONS; generated by :x-if.interpret from interpreting a user
;; statement. This is what will actually be executed by :x-if.interpret.
(defclass queued-action ()
  ((function-name
       :initarg :function :accessor function-name)
   (direct-object
       :initarg :direct-object :initform nil :accessor direct-object)
   (indirect-object
       :initarg :indirect-object :initform nil :accessor indirect-object)
   (subject
       :initarg :subject :initform (get-player) :accessor subject)))


;; —————————————————
;; OBJECT ADD/DEL

;; These are here to appropriately modify the LEXICON (xif.l:*lexicon*)
;; according to objects in the game. Words used to describe objects
;; and actions will be added when an object is initialized; deleted
;; when destroyed.

;; TODO
;; Nuance is necessary; if a noun/adjective/etc is used for other objects,
;; it shouldn't be added again nor deleted from the lexicon when adding a new
;; object or deleting one, respectively.
;; For now, it's assumed all words are new, and are all used by one object.

(defmethod initialize-instance :after ((game-object game-object) &key)
  (xif.l:add-game-object-words game-object))

(defmethod destroy-object :after ((game-object game-object) &key)
  (xif.l:delete-game-object-words game-object))



(defmethod initialize-instance :after ((action action) &key)
  (mapcar #'xif.l:add-verb (verbs action)))

;; TODO: write #'xif.l:remove-action
(defmethod destroy-object :after ((action action) &key))


;; —————————————————
;; PARENT/CHILD

;; GAME-OBJECT GAME-OBJECT → NIL
(deftransaction link (child parent)
  "Link two objects together, as child and parent."
  (setf (slot-value parent 'children)
        (nconc (slot-value parent 'children) (list child)))
  ;; --
  (if (child-p child) (unlink child))
  (setf (slot-value child 'parent) parent))

;; GAME-OBJECT → NIL
(deftransaction unlink (child)
  "Seperate a child from it's parent (and vice-versa)."
  (let ((parent (slot-value child 'parent)))
    (setf (slot-value parent 'children)
          (delete child (slot-value parent 'children)))
    ;; --
    (setf (slot-value child 'parent) nil)))


;; GAME-OBJECT → BOOLEAN
(defmethod parent-p ((object game-object))
  "Return whether or not an object is a parent (has 1+ children)."
  (slot-value object 'children))

;; GAME-OBJECT → BOOLEAN
(defmethod child-p ((object game-object))
  "Return whether or not an object is a child (has a parent)."
  (slot-value object 'parent))


;; —————————————————
;; INDEXING

;; NIL → PLAYER
(defun get-player ()
  "Return the player object."
  (car (store-objects-with-class 'player)))

;; NIL → LIST_OF_LOCATIONS
(defun all-locations ()
  "Get all location objects."
  (store-objects-with-class 'location))

;; NIL → LIST_OF_MOBS
(defun all-mobs ()
  "Get all mob objects."
  (store-objects-with-class 'mob))

;; NIL → LIST_OF_ENTITIES
(defun all-entities ()
  "Get all entity objects."
  (store-objects-with-class 'entity))

;; NIL → LIST_OF_ENTITIES
(defun all-actions ()
  "Get all entity objects."
  (store-objects-with-class 'action))


;; LIST_OF_OBJECTS STRING → LIST_OF_OBJECTS
(defun noun→objects (objects noun)
  "Return objects within the current list of objects of the given noun."
  (index-by-slot-list objects 'nouns noun))

;; LIST_OF_OBJECTS STRING → LIST_OF_OBJECTS
(defun adjective→objects (objects adjective)
  "Return objects within the current list of objects of the given adjective."
  (index-by-slot-list objects 'adjectives adjective))

;; LIST_OF_OBJECTS STRING → LIST_OF_OBJECTS
(defun proper-noun→objects (objects proper-noun)
  "Return objects within the current list of objects of the given proper-noun"
  (index-by-slot-list objects 'proper-nouns proper-noun))

;; STRING → LIST_OF_ACTIONS
(defun verb→actions (verb)
  "Return all actions matching the given verb."
  (index-by-slot-list (all-actions) 'verbs verb))


;; SYMBOL SYMBOL FUNCTION SYMBOL FUNCTION → (DEFUN …)
(defmacro defun-obj-word-index (name obj-type all-obj-type word-type word-fun)
  "Makes an index function for the given object type and word-type.
  I.E., will generate #'adjective→mobs for object-type 'mobs' and word-type of
  'adjective'"
  `(defun ,name (,word-type)
     ,(format nil "Return the ~A corresponding with the given ~A."
              obj-type word-type)
     (funcall ,word-fun (funcall ,all-obj-type) ,word-type)))

;; SYMBOL FUNCTION SYMBOL SYMBOL SYMBOL → ( (DEFUN …) (DEFUN …) (DEFUN …))
(defmacro defuns-obj-word-indices (obj-type all-obj adj→obj noun→obj pnoun→obj)
  "Makes the index functions for a given object type for every word type.
  I.E., when given 'mobs', it'll make #'noun→mobs, #'adjective→mobs, etc."
  `(progn
    (defun-obj-word-index ,adj→obj ,obj-type ,all-obj adjective
      #'adjective→objects)
    (defun-obj-word-index ,noun→obj ,obj-type ,all-obj noun #'noun→objects)
    (defun-obj-word-index ,pnoun→obj ,obj-type ,all-obj proper-noun
      #'proper-noun→objects)))


(defuns-obj-word-indices game-object #'all-game-objects
  adjective→game-objects noun→game-objects proper-noun→game-objects)
(defuns-obj-word-indices entity #'all-entities
  adjective→entities noun→entities proper-noun→entities)
(defuns-obj-word-indices mob #'all-mobs
  adjective→mobs noun→mobs proper-noun→mobs)
(defuns-obj-word-indices locations #'all-locations
  adjective→locations noun→locations proper-noun→locations)


;; LIST_OF_OBJECTS SYMBOL VARYING → LIST_OF_OBJECTS
(defun index-by-slot-list (objects slot-name target)
  "Return objects from the given list that contain a target item in the list
  of the given slot."
  (index-by-slot objects slot-name target :test #'xif.m:position-equal))

;; LIST_OF_OBJECTS SYMBOL VARYING :FUNCTION → LIST_OF_OBJECTS
(defun index-by-slot (objects slot-name target &key (test #'equal))
  "Return objects from the given list that pass the given test between
  slot-value and target."
  (loop :for object :in objects
        :if (funcall test target (slot-value object slot-name))
        :collect object))




;; —————————————————————————————————————
;; X-IF.CLIENT
;; ACTUALLY PLAY THE GAME

(in-package :x-if.client)

;; —————————————————————————————————————

;; This is the general client package— everything else uses it.

;; —————————————————————————————————————

(defun status ()
  (format nil "~A~%" (slot-value (get-player) 'xif.e::parent)))

(defun prompt () ">> ")

(defun game-loop ()
  (let ((m 1))
    (display-status)
    (setq m (input-sentence))
    (text "You said: ~A~%" m)
    (text "~A~%" (xif.e:all-game-objects))
    (text "I LOVE YOU~%")
    (sleep 2)
    (game-loop)))

(defun start (game)
  (make-instance 'mp-store :directory #p"~/.local/share/x-if/"
                 :subsystems (list (make-instance 'store-object-subsystem)))
  (if (not (all-game-objects))
    (populate-world))
  (game-loop))

(defun examine (object)
  (text (xif.e:description object)))

(defun populate-world ()
  (make-instance 'xif.e::location :id 0  :proper-nouns '("Lobby")
                 :nouns '("room")
                 :adjectives '("ugly")
                 :description "It's rather ugly, really.")

  (make-instance 'xif.e::mob :id 101  :proper-nouns '("Barry")
                 :nouns '("human" "person" "man" "gentleman" "sir" "dude")
                 :adjectives '("suspicious")
                 :description "He looks suspicious, no?")

  (make-instance 'xif.e::player :id 100
                 :proper-nouns '("Maria" "I" "myself" "me")
                 :nouns '("human" "person" "woman" "lady" "lass" "dudette")
                 :adjectives '("hideous")
                 :description "A rather hideous lass.")

  (make-instance 'xif.e::action :function-name 'xif.c::examine :direct-object-p T
                 :verbs '("examine" "look" "view"))
  
  (link (get-player) (id→game-object 0))
  (link (id→game-object 100) (id→game-object 0)))




;; —————————————————————————————————————
;; X-IF.CLIENT.TERMINAL
;; A SIMPLE CLIENT

(in-package :x-if.client.terminal)

;; —————————————————————————————————————

;; This package is a client package— it uses only basic terminal input/output,
;; so ought to be the most portable possible. No image or music support.

;; —————————————————————————————————————

(defun xif.c::text (string &rest format-args)
  (apply #'format (nconc (list t string) format-args)))

(defun xif.c::input-sentence ()
  (read-line))




;; —————————————————————————————————————
;; X-IF.MISC
;; MISC HELPER FUNCTIONS

(in-package :x-if.misc)

;; —————————————————————————————————————

;; This package just contains random, useful functions that're used throughout
;; x-if. Mainly they're for string manipulations, etc.

;; STRING → LIST_OF_STRINGS
(defun string-lines (string)
  "Turn a multi-line string into a list of lines."
  (cl-strings:split string #\newline))

;; LIST_OF_STRINGS → STRING
(defun lines-string (lines)
  "Turn a list of strings into a multi-lined string, with each string being
  a seperate line."
  (cl-strings:join lines :separator "
"))


;; STRING → STRING
(defmethod line-cdr ((string string))
  "Get the 'cdr' of a multi-lined string (pop off the first line)."
  (line-cdr (string-lines string)))

;; LIST_OF_STRINGS → STRING
(defmethod line-cdr ((lines list))
  "Get the 'cdr' of a list of lines, then turn back into a string."
  (lines-string (cdr lines)))

;; STRING → STRING
(defmethod line-car ((string string))
  "Get a multi-lined string's 'car' (the first line)."
  (line-car (string-lines string)))

;; LIST_OF_STRINGS → STRING
(defmethod line-car ((lines list))
  "Get the 'car' of a list of lines."
  (car lines))


;; STRING → NUMBER
(defun line-length (string)
  "Return the amount of lines in a given string."
  (length (string-lines string)))

;; STRING STRING FUNCTION → NUMBER
(defmethod line-position ((string string) target &key (test #'in-string-p))
  "Return the number of the line in which a given target-string can be found,
  within a multi-lined string."
  (line-position (string-lines string) target :test test))

;; LIST_OF_STRINGS STRING FUNCTION → NUMBER
(defmethod line-position ((lines list) target &key (test #'in-string-p))
  "Return which number string the given target-string can be found in."
  (position T (mapcar (lambda (line) (funcall test line target)) lines)))

;; STRING STRING → BOOLEAN
(defun in-string-p (string target)
  "Return whether or not a target-string is within another string."
  (< 1 (length (cl-strings:split string target))))

;; STRING STRING FUNCTION → STRING
(defmethod remove-line ((string string) target &key (test #'in-string-p))
  "Remove a line matching the given test, given a target string."
  (remove-line (string-lines string) target :test test))

;; LIST_OF_STRINGS STRING FUNCTION → STRING
(defmethod remove-line ((lines list) target &key (test #'in-string-p))
  "Remove a line matching the test, return a multi-lined string based on
  given list, but sans that removed line, ofc."
  (aif (ignore-errors (line-position lines target :test test))
       (lines-string
        (remove (nth it lines) lines :test #'equal :count 1))
       (lines-string list)))

;; ITEM LIST → NUMBER
(defun position-equal (item list)
  "Literally just #'cl:position but with the test equal."
  (position item list :test #'equal))
