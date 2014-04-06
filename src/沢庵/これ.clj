(ns 沢庵.これ
  (:require
    (clojure [string :as string]))
  (:import
    (java.lang Integer
               Float)))

;MARK            = '('   # push special markobject on stack
;STOP            = '.'   # every pickle ends with STOP
;POP             = '0'   # discard topmost stack item
;POP_MARK        = '1'   # discard stack top through topmost markobject
;DUP             = '2'   # duplicate top stack item
;FLOAT           = 'F'   # push float object; decimal string argument
;INT             = 'I'   # push integer or bool; decimal string argument
;BININT          = 'J'   # push four-byte signed int
;BININT1         = 'K'   # push 1-byte unsigned int
;LONG            = 'L'   # push long; decimal string argument
;BININT2         = 'M'   # push 2-byte unsigned int
;NONE            = 'N'   # push None
;PERSID          = 'P'   # push persistent object; id is taken from string arg
;BINPERSID       = 'Q'   #  "       "         "  ;  "  "   "     "  stack
;REDUCE          = 'R'   # apply callable to argtuple, both on stack
;STRING          = 'S'   # push string; NL-terminated string argument
;BINSTRING       = 'T'   # push string; counted binary string argument
;SHORT_BINSTRING = 'U'   #  "     "   ;    "      "       "      " < 256 bytes
;UNICODE         = 'V'   # push Unicode string; raw-unicode-escaped'd argument
;BINUNICODE      = 'X'   #   "     "       "  ; counted UTF-8 string argument
;APPEND          = 'a'   # append stack top to list below it
;BUILD           = 'b'   # call __setstate__ or __dict__.update()
;GLOBAL          = 'c'   # push self.find_class(modname, name); 2 string args
;DICT            = 'd'   # build a dict from stack items
;EMPTY_DICT      = '}'   # push empty dict
;APPENDS         = 'e'   # extend list on stack by topmost stack slice
;GET             = 'g'   # push item from memo on stack; index is string arg
;BINGET          = 'h'   #   "    "    "    "   "   "  ;   "    " 1-byte arg
;INST            = 'i'   # build & push class instance
;LONG_BINGET     = 'j'   # push item from memo on stack; index is 4-byte arg
;LIST            = 'l'   # build list from topmost stack items
;EMPTY_LIST      = ']'   # push empty list
;OBJ             = 'o'   # build & push class instance
;PUT             = 'p'   # store stack top in memo; index is string arg
;BINPUT          = 'q'   #   "     "    "   "   " ;   "    " 1-byte arg
;LONG_BINPUT     = 'r'   #   "     "    "   "   " ;   "    " 4-byte arg
;SETITEM         = 's'   # add key+value pair to dict
;TUPLE           = 't'   # build tuple from topmost stack items
;EMPTY_TUPLE     = ')'   # push empty tuple
;SETITEMS        = 'u'   # modify dict by adding topmost key+value pairs
;BINFLOAT        = 'G'   # push float; arg is 8-byte float encoding
;
;TRUE            = 'I01\n'  # not an opcode; see INT docs in pickletools.py
;FALSE           = 'I00\n'  # not an opcode; see INT docs in pickletools.py

(defn readline 
  "Reads until the next newline and retrns a vector of what was read and the
  remainder of the sequence."
  [s]
  (loop [[nchar & remaining] s
         new-s []]
    (if (or (= nchar \newline)
            (not nchar))
      [(apply str new-s) remaining]
      (recur remaining (conj new-s nchar)))))

(defn keywordify
  "Takes a map and returns the same map where all keys are keywords."
  [mapp]
    (reduce (fn [m [k v]] (assoc m (keyword k) v)) {} mapp))


(defn load-int [s stack memo]
  (let [[line n-in] (readline s)]
    [n-in
     (conj stack (cond 
                   (= line "01") true  ; True and false are encoded as
                   (= line "00") false ; special cases of integers.
                   :else (Integer/parseInt line)))
     memo]))

(def hex-vals
  (set (map char (concat (range 48 58) (range 97 103) (range 65 71)))))

(def oct-vals
  (set (map char (range 48 56))))

(defn read-rest
  "Takes a sequence and a set of values, returns the first consecutive set of
  the values in the sequence."
  [s values]
  (loop [[n & r] s
         ret []]
    (if (values n)
      (recur r (conj ret n))
      ret)))

(defn unescape-string 
  "Takes a string representing an escaped python string and returns a clojure
  string approximating how it would eval in python as closely as possible"
  [s]
  (loop [[nchar & remaining] (seq s)
         rstr []]
    (cond
      (not nchar) (apply str rstr)
      (not (= nchar \\)) (recur remaining (conj rstr nchar))
      :else 
        (let [f-char (first remaining)
              e-char (case f-char
                       \\ \\
                       \' \'
                       \" \"
                       \a (char 7)
                       \b \backspace
                       \f \formfeed
                       \n \newline
                       \r \return
                       \t \tab
                       \v (char 13)
                       \x :hex
                       (\0 \1 \2 \3 \4 \5 \6 \7 \8 \9) :oct)]
          (cond
            (char? e-char) (recur (rest remaining) (conj rstr e-char))
            (#{:hex :oct} e-char)
              (let [[values base] (case e-char
                                    :hex [hex-vals 16]
                                    :oct [oct-vals 8])
                    code (read-rest (rest remaining) values)
                    unesc (char (Integer/parseInt (apply str code) base))]
                (recur (nthrest remaining (inc (count code))) 
                       (conj rstr unesc))))))))


(defn load-stringです
  "Because apparently load-string is a builtin."
  [s stack memo]
    (let [[line n-in] (readline s)]
      [n-in 
       (conj stack
             (unescape-string (string/replace line #"(^'|'$)" ""))) 
       memo]))

(defn load-float [s stack memo]
  (let [[line n-in] (readline s)]
    [n-in (conj stack (Float/parseFloat line)) memo]))

(defn load-none [s stack memo]
  [s (conj stack :None) memo])

(defn load-put [s stack memo]
  "Store first item on stack in memo on key specified by opcode arg."
  (let [[line n-in] (readline s)]
    [n-in stack (assoc memo line (first stack))]))

(defn load-mark [s stack memo]
  [s (conj stack :mark) memo])

(defn load-dict [s stack memo]
  ; Loop through the stack backwards and two at a time building up a "dict".
  ; Store the resulting map on the top of the stack.
  (loop [[v k & rem-stack] stack
         dict {}]
    (cond
      (= v :mark) 
        (if k
          [s (conj rem-stack k dict) memo]
          [s (conj rem-stack dict) memo])
      (= k :mark) (throw (Throwable. "Odd number of key/value pairs on stack"))
      (not (or v k (seq rem-stack))) 
        (throw (Throwable. "Exhausted stack without finding a :mark."))
      :else (recur rem-stack (assoc dict k v)))))

(defn load-append [s stack memo]
  (let [[v l & remaining] stack]
    [s (conj remaining (conj l v)) memo]))

(defn load-list [s stack memo]
  (loop [[i & rem-stack] stack
         l []]
    (cond
      (= i :mark) [s (conj rem-stack l) memo]
      (not i) (throw (Throwable. "Exhausted stack without finding a :mark."))
      :else (recur rem-stack (conj l i)))))

(defn load-set-item [s stack memo]
  (let [[v k d & remaining] stack]
    [s (conj remaining (assoc d k v)) memo]))

(defn load-tuple [s stack memo]
  (loop [[c & remaining] stack
         tuple '()]
    (if (or (= c :mark) (nil? c))
      [s (conj remaining tuple) memo]
      (recur remaining (conj tuple c)))))

(defn reconstructor [cls base state] cls)

(def lib-hierarchy
  {"copy_reg" {"_reconstructor" reconstructor}})

(defn load-global
  "As it stands, this is a pretty silly function. Pretend like we're looking
  up a callable like python's importlib. In reality we're just checking for
  the special case of a generic reconstructor (which we'll emulate) otherwise
  just keep the name of the thing"
  [s stack memo]
    (let [[module-name new-s] (readline s)
          [class-name new-s] (readline new-s)
          module (get lib-hierarchy module-name {})
          klass (get module class-name nil)]
      (if klass
        [new-s (conj stack klass) memo]
        [new-s 
         (conj stack {:__module__ module-name
                      :__name__ class-name}) 
         memo])))

(defn load-reduce [s stack memo]
  (let [[args f & new-stack] stack]
    [s (conj new-stack (apply f args)) memo]))

(defn load-build [s stack memo]
  (let [[state inst & new-stack] stack]
    [s (conj new-stack (merge inst (keywordify state))) memo]))

(def default-instructions
  {\( load-mark
   \F load-float
   \I load-int
   \N load-none
   \R load-reduce
   \S load-stringです
   \a load-append
   \b load-build
   \c load-global
   \d load-dict
   \l load-list
   \p load-put
   \s load-set-item
   \t load-tuple })



(defn load-seq
  "Loads, as best we can, a Python pickle given as a seq of characters."
  ([pickle]
   (load-seq pickle default-instructions))
  ([pickle instructions]
   (loop [[op & remaining] pickle
          stack '()
          memo {}]
     (if (= op \.)
       (first stack)
       (let [op-fn (get instructions op)
             [n-remaining n-stack n-memo] (op-fn remaining stack memo)]
         (recur n-remaining n-stack n-memo))))))
