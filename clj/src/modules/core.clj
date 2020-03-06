(ns modules.core
  (:require [hiccup.core :as hiccup]))

(def esop "European Symposium on Programming")
(def fool "Foundations of Object-Oriented Languages")
(def icalp "ICALP")
(def icfp "ICFP")
(def ifl "Implementation and Application of Functional Languages")
(def jfp "Journal of Functional Programming")
(def oopsla "Object-oriented Programming, Systems, Languages, and Applications")
(def pldi "PLDI")
(def plilp "Programming Languages Implementation and Logic Programming")
(def popl "POPL")
(def ppdp "Principles and Practice of Declarative Programming")
(def tapsoft "TAPSOFT")
(def tldi "Types in Language Design and Implementation")
(def toplas "ACM Transactions on Programming Languages and Systems")

(def ancona "Davide Ancona")
(def bracha "Gilad Bracha")
(def cardelli "Luca Cardelli")
(def crary "Karl Crary")
(def dreyer "Derek Dreyer")
(def flatt "Matthew Flatt")
(def frisch "Alain Frisch")
(def garrigue "Jacques Garrigue")
(def harper "Robert Harper")
(def leroy "Xavier Leroy")
(def macqueen "David B. MacQueen")
(def makholm "Henning Makholm")
(def mitchell "John C. Mitchell")
(def moggi "Eugenio Moggi")
(def montagu "Benoît Montagu")
(def mp-jones "Mark P. Jones")
(def nakata "Keiko Nakata")
(def rossberg "Andreas Rossberg")
(def russo "Claudio V. Russo")
(def stone "Christopher Stone")
(def tofte "Mads Tofte")
(def wells "J. B. Wells")
(def white "Leo P. White")
(def zucca "Elena Zucca")

(def harvard-cs-group "Harvard Computer Science Group")

(defn write
  [fname x]
  (with-open [w (clojure.java.io/writer fname)]
    (.write w x)))

(defn prepend-comma [n] (str ", " n))

(defn paren [s] (str "(" s ")"))

(defn render
  [{:keys [key title author date location doi url tr-url tr-with tr-date slides appendix]}]
  [:div
   [:h4 title]
   [:p author]
   [:p (str (if (sequential? location)
              (apply str (interpose ", " (map #(hiccup/html %) location)))
              location)
            (if location ", ") date)]
   (if doi
     [:p "DOI: "
      [:a {:href (str "https://doi.org/" doi)} doi]])
   (if url
     (into [] (concat
               [:p "Available at "
                [:a {:href url} url]]
               (if tr-url ; Technical Report URL
                 [[:br]
                  (if tr-with
                    (str "Technical Report (with " tr-with (if tr-date (prepend-comma tr-date)) "): ")
                    (str "Technical Report" (if tr-date (str " " (paren tr-date))) ": "))
                  [:a {:href tr-url} tr-url]]))))
   (if slides
     [:p "Slides: "
      [:a {:href slides} slides]])
   (if appendix
     [:p "Appendix: "
      [:a {:href appendix} appendix]])])

(defn html-entries
  [xs]
  (apply str (interpose "\n" (map #(hiccup/html (render %)) xs))))

(defn authors
  ([x] x)
  ([x & ys]
   (let [l (last (cons x ys))
         xs (butlast (cons x ys))]
     (str (apply str (interpose ", " xs)) " and " l))))

(defn str-pages
  [pages]
  (apply str (interpose \u2013 pages)))

(defn proceedings-location
  [location & {:keys [pages]}]
  (apply str location (if pages [", pp. " (str-pages pages)])))

(defn techrpt-location
  [& {:keys [institution number]}]
  (str institution ", " number))

(defn dissertation-location
  [& {:keys [institution degree]}]
  (str degree " dissertation, " institution))

(defn wrap-with-paren
  [x]
  (if x
    (str "(" x ")")))

(defn journal-location
  [title & {:keys [pages volume number]}]
  (if volume
    [[:i title] (str volume (wrap-with-paren number) (if pages
                                                       (if (string? pages)
                                                         (str ", " pages)
                                                         (str ", pp. " (str-pages pages)))))]
    [[:i title]]))

(defn book-location
  [title & {:keys [publisher]}]
  (if publisher
    [[:i title] publisher]
    [[:i title]]))

(def entries
  (array-map
   :rey1983
   {:key      "Rey1983"
    :title    "Types, abstraction and parametric polymorphism"
    :author   "J. C. Reynolds"
    :date     "1983"
    :location (proceedings-location "Information Processing" :pages '(513 523))
    :url      "http://www.cse.chalmers.se/edu/year/2010/course/DAT140_Types/Reynolds_typesabpara.pdf"}

   :bl1984
   {:key      "BL1984"
    :title    "A kernel language for abstract data types and modules"
    :author   (authors "R. Burstall" "B. Lampson")
    :date     "1984"
    :location (proceedings-location "Semantics of Data Types" :pages '(1 50))
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/35-KernelModules.pdf"}

   :mac1984
   {:key      "Mac1984"
    :title    "Modules for Standard ML"
    :author   macqueen
    :date     1984
    :month    "August"
    :location (proceedings-location "ACM Conference on LISP and Functional Programming" :pages '(198 207))
    :url      "https://www.researchgate.net/profile/David_Macqueen/publication/221252232_Modules_for_Standard_ML/links/0f317532689526bea1000000/Modules-for-Standard-ML.pdf"}

   :mp1985
   {:key      "MP1985"
    :title    "Abstract types have existential types"
    :author   (authors mitchell "G. D. Plotkin")
    :date     1985
    :location (proceedings-location popl :pages '(37 51))}

   :cw1985
   {:key      "CW1985"
    :title    "On understanding types, data abstraction, and polymorphism"
    :author   (authors cardelli "P. Wegner")
    :date     "1985"
    :location (techrpt-location :institution "Brown University" :number "CS-85-14")
    :url      "http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf"}

   :mit1986
   {:key      "Mit1986"
    :title    "Representation independence and data abstraction"
    :author   mitchell
    :date     "1986"
    :location (proceedings-location popl)}

   :mac1986
   {:key      "Mac1986"
    :title    "Using dependent types to express modular structure"
    :author   macqueen
    :date     "1986"
    :location (proceedings-location popl :pages '(277 286))
    :url      "https://www.researchgate.net/profile/David_Macqueen/publication/2385295_Using_Dependent_Types_to_Express_Modular_Structure/links/09e415148bbf0d4470000000.pdf"}

   :hmt1987
   {:key      "HMT1987"
    :title    "A type discipline for program modules"
    :author   (authors harper "R. Milner" tofte)
    :date     "1987"
    :location (proceedings-location tapsoft)
    :url      "https://link.springer.com/content/pdf/10.1007%2FBFb0014988.pdf"}

   :cm1988
   {:key      "CM1988"
    :title    "Persistence and type abstraction"
    :author   (authors cardelli macqueen)
    :date     "1988. First appeared in 1985"
    :location "Data types and persistence, Springer-Verlag"
    :url      "http://lucacardelli.name/Papers/Persistence%20and%20Type%20Abstraction.pdf"}

   :mp1988
   {:key      "MP1988"
    :title    "Abstract types have existential type"
    :author   (authors mitchell "G. D. Plotkin")
    :date     1988
    :location (proceedings-location toplas :pages '(470 502))
    :url      "https://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf"}

   :car1988
   {:key      "Car1988"
    :title    "Phase distinctions in type theory"
    :author   cardelli
    :date     "1988"
    :location "Manuscript"
    :url      "http://lucacardelli.name/Papers/PhaseDistinctions.A4.pdf"}

   :mog1989
   {:key      "Mog1989"
    :title    "A category-theoretic account of program modules"
    :author   moggi
    :date     "1989"
    :location (proceedings-location "Category Theory and Computer Science" :pages '(101 117))
    :url      "https://www.disi.unige.it/person/MoggiE/ftp/mscs91.pdf"}

   :hmm1990
   {:key      "HMM1990"
    :title    "Higher-order modules and the phase distinction"
    :author   (authors harper mitchell moggi)
    :date     "1990"
    :location (proceedings-location popl :pages '(341 354))
    :url      "https://www.disi.unige.it/person/MoggiE/ftp/popl90.pdf"}

   :cl1990
   {:key      "CL1990"
    :title    "Abstract types and the dot notation"
    :author   (authors cardelli leroy)
    :date     "1990"
    :location (proceedings-location "IFIP TC2 working conference on programming concepts and methods" :pages '(479 504))
    :url      "https://xavierleroy.org/publi/abstract-types-dot-notation.pdf"}

   :bc1990
   {:key      "BC1990"
    :title    "Mixin-based inheritance"
    :author   (authors bracha "William Cook")
    :date     "1990"
    :location (proceedings-location "OOPSLA/ECOOP")
    :url      "http://www.bracha.org/oopsla90.ps"}

   :car1991
   {:key      "Car1991"
    :title    "Typeful programming"
    :author   cardelli
    :date     "1991"
    :location (proceedings-location "Formal Description of Programming Concepts")
    :url      "http://lucacardelli.name/Papers/TypefulProg.A4.pdf"}

   :mmm1991
   {:key      "MMM1991"
    :title    "An extension of Standard ML modules with subtyping and inheritance"
    :author   (authors mitchell "Sigurd Meldal" "Neel Madhav")
    :date     "1991"
    :location (proceedings-location popl :pages '(270 278))
    :url      "https://www.researchgate.net/publication/2815527_An_extension_of_Standard_ML_modules_with_subtyping_and_inheritance"}

   :bl1991
   {:key      "BL1991"
    :title    "Modularity meets inheritance"
    :author   (authors bracha "Gary Lindstrom")
    :date     1991
    :location (techrpt-location :institution "University of Utah" :number "UUCS-91-017")
    :url      "http://www.bracha.org/modularity-meets-inheritance.ps"}

   :bra1992
   {:key      "Bra1992"
    :title    "The programming language JIGSAW: Mixins, modularity and multiple inheritance"
    :author   bracha
    :date     "1992"
    :location (dissertation-location :institution "University of Utah" :degree "PhD")
    :url      "http://www.bracha.org/jigsaw.pdf"}

   :tof1992
   {:key      "Tof1992"
    :title    "Principal signatures for higher-order program modules"
    :author   tofte
    :date     "1992"
    :location (proceedings-location popl :pages '(189 199))}

   :apo1993
   {:key      "Apo1993"
    :title    "Extending record typing to type parametric modules with sharing"
    :author   "María Virginia Aponte"
    :date     "1993"
    :location (proceedings-location popl :pages '(465 478))
    :url      "https://www.researchgate.net/publication/2416181_Extending_Record_typing_to_type_parametric_modules_with_sharing"}

   :hm1993
   {:key      "HM1993"
    :title    "On the type structure of standard ML"
    :author   (authors harper mitchell)
    :date     1993
    :location (proceedings-location toplas :pages '(211 252))
    :url      "https://crypto.stanford.edu/~jcm/papers/harper-mitch-TOPLAS-93.pdf"}

   :mg1993
   {:key      "MG1993"
    :title    "Studying the ML module system in HOL"
    :author   (authors "Savi Maharaj" "Elsa Gunter")
    :date     "1993"
    :location (journal-location "The Computer Journal" :volume 36 :number 5)
    :url      "http://www.cs.stir.ac.uk/~sma/publications/HOLML.ps"}

   :hl1994
   {:key      "HL1994"
    :title    "A type-theoretic approach to higher-order modules with sharing"
    :author   (authors harper "Mark Lillibridge")
    :date     "1994"
    :location (proceedings-location popl :pages '(123 137))
    :url      "https://www.cs.cmu.edu/~rwh/papers/sharing/popl94.pdf"}

   :ler1994
   {:key      "Ler1994"
    :title    "Manifest types, modules, and separate compilation"
    :author   leroy
    :date     "1994"
    :location (proceedings-location popl :pages '(109 122))
    :url      "https://xavierleroy.org/publi/manifest-types-popl.pdf"}

   :mt1994
   {:key      "MT1994"
    :title    "A semantics for higher-order functors"
    :author   (authors macqueen tofte)
    :date     "1994"
    :location (proceedings-location "Programming Languages and Systems – ESOP ’94" :pages '(409 423))
    :url      "https://rd.springer.com/content/pdf/10.1007%2F3-540-57880-3_27.pdf"}

   :bis1995
   {:key      "Bis1995"
    :title    "Higher-order functors with transparent signatures"
    :author   "Sandip K. Biswas"
    :date     1995
    :location (proceedings-location popl :pages '(154 163))
    :doi      "10.1145/199448.199478"
    :url      "http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9123&rep=rep1&type=pdf"}

   :ler1995
   {:key      "Ler1995"
    :title    "Applicative functors and fully transparent higher-order modules"
    :author   leroy
    :date     "1995"
    :location (proceedings-location popl :pages '(142 153))
    :url      "https://xavierleroy.org/publi/applicative-functors.pdf"}

   :jon1995
   {:key      "Jon1995"
    :title    "From Hindley-Milner types to first-class structures"
    :author   mp-jones
    :date     1995
    :month    "June"
    :location (proceedings-location "The Haskell workshop")
    :url      "http://web.cecs.pdx.edu/~mpj/pubs/haskwork95.pdf"}

   :ds1996
   {:key      "DS1996"
    :title    "Mixin modules"
    :author   (authors "Dominic Duggan" "Constantinos Sourelis")
    :date     "1996"
    :location (proceedings-location icfp :pages '(262 273))
    :url      "https://www.cs.tufts.edu/~nr/cs257/archive/dominic-duggan/Mixin%20Modules.pdf"}

   :jon1996
   {:key      "Jon1996"
    :title    "Using parameterized signatures to express modular structure"
    :author   "Mark P. Jones"
    :date     1996
    :location (proceedings-location popl :pages '(66 78))
    :url      "https://web.cecs.pdx.edu/~mpj/pubs/paramsig.pdf"}

   :ler1996
   {:key      "Ler1996"
    :title    "A syntactic theory of type generativity and sharing"
    :author   leroy
    :date     "1996"
    :location (journal-location jfp :number 5 :volume 6 :pages '(667 698))
    :url      "https://xavierleroy.org/publi/syntactic-generativity.pdf"}

   :nj1996
   {:key      "NJ1996"
    :title    "An exploration of modular programs"
    :author   (authors "Jan Nicklish" "Simon Peyton Jones")
    :date     "1996"
    :location "In The Glasgow Workshop on Functional Programming"
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/Nicklisch-modules.pdf"}

   :ad1996
   {:key      "AD1996"
    :title    "Type isomorphisms for module signatures"
    :author   (authors "María-Virginia Aponte" "Roberto Di Cosmo")
    :date     1996
    :location (proceedings-location plilp :pages '(334 346))
    :url      "http://www.dicosmo.org/Articles/1996-AponteDiCosmo-PLILP.pdf"}

   :asp1997
   {:key      "Asp1997"
    :title    "Type systems for modular programs and specifications"
    :author   "David R. Aspinall"
    :date     "1997"
    :location (dissertation-location :institution "Edinburgh University, Edinburgh, Scotland" :degree "PhD")
    :url      "https://www.era.lib.ed.ac.uk/handle/1842/11587"}

   :car1997
   {:key      "Car1997"
    :title    "Program fragments, linking, and modularization"
    :author   "Luca Cardelli"
    :date     1997
    :month    "January"
    :location (proceedings-location popl :pages '(266 277))
    :url      "http://lucacardelli.name/Papers/Linking.A4.pdf"}

   :cou1997
   {:key      "Cou1997"
    :title    "An applicative module calculus"
    :author   "Judicäel Courant"
    :date     "1997"
    :location (proceedings-location tapsoft)}

   :lil1997
   {:key      "Lil1997"
    :title    "Translucent sums: A foundation for higher-order module systems"
    :author   "Mark Lillibridge"
    :date     1997
    :location (dissertation-location :institution "Carnegie Mellon University" :degree "PhD")
    :url      "https://www.cs.cmu.edu/Groups/fox/papers/mdl-thesis.ps"}

   :hs1997
   {:key      "HS1997"
    :title    "An interpretation of Standard ML in type theory"
    :author   (authors harper stone)
    :date     1997
    :location (techrpt-location :institution "Carnegie Mellon University" :number "CMU-CS-97-147")
    :url      "https://www.cs.cmu.edu/Groups/fox/papers/sml96-v3.ps"}

   :rus1998
   {:key      "Rus1998"
    :title    "Types for modules"
    :author   russo
    :date     "1998"
    :location (dissertation-location :institution "University of Edinburgh, UK" :degree "PhD")
    :url      "http://www.dcs.ed.ac.uk/home/cvr/ECS-LFCS-98-389.pdf"}

   :sha1998
   {:key      "Sha1998"
    :title    "Typed cross-module compilation"
    :author   "Zhong Shao"
    :date     "1998"
    :location (proceedings-location icfp :pages '(141 152))
    :url      "http://flint.cs.yale.edu/flint/publications/tcc.pdf"
    :tr-url   "http://flint.cs.yale.edu/flint/publications/tcc-tr.pdf"}

   :ds1998
   {:key      "DS1998"
    :title    "Parameterized modules, recursive modules and mixin modules"
    :author   (authors "Dominic Duggan" "Constantinos Sourelis")
    :date     "1998"
    :location "In ACM SIGPLAN Workshop on ML, pages 87–96, Baltimore, MA, USA, September 1998"}

   :ff1998a
   {:key      "FF1998a"
    :title    "Modular object-oriented programming with units and mixins"
    :author   (authors "Robert Bruce Findler" flatt)
    :date     "1998"
    :location (proceedings-location icfp :pages '(94 104))
    :url      "https://www2.ccs.neu.edu/racket/pubs/icfp98-ff.pdf"}

   :ff1998b
   {:key      "FF1998b"
    :title    "Units: Cool modules for HOT languages"
    :author   (authors flatt "Matthias Felleisen")
    :date     "1998"
    :location (proceedings-location pldi :pages '(236 248))
    :url      "http://www.ccs.neu.edu/scheme/pubs/pldi98-ff.ps.gz"}

   :az1998a
   {:key      "AZ1998a"
    :title    "A theory of mixin modules: Basic and derived operators"
    :author   (authors ancona zucca)
    :date     "1998"
    :location (journal-location "Mathematical Structures in Computer Science" :volume 8 :number 4 :pages '(401 446))}

   :az1998b
   {:key      "AZ1998b"
    :title    "An algebra of mixin modules"
    :author   (authors ancona zucca)
    :date     "1998"
    :location "In 12th Workshop on Algebraic Development Techniques - Selected Papers, pp. 92–106"}

   :az1999
   {:key      "AZ1999"
    :title    "A primitive calculus for module systems"
    :author   (authors ancona zucca)
    :date     "1999"
    :location (proceedings-location ppdp :pages '(62 79))}

   :chp1999
   {:key      "CHP1999"
    :title    "What is a recursive module?"
    :author   (authors crary harper "Sidd Puri")
    :date     "1999"
    :location (proceedings-location pldi :pages '(50 63))
    :url      "http://www.cs.cmu.edu/~crary/papers/1999/recmod/recmod.ps.gz"}

   :rus1999
   {:key      "Rus1999"
    :title    "Non-dependent types for Standard ML modules"
    :author   russo
    :date     "1999"
    :location (proceedings-location ppdp :pages '(80 97))
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/1999/09/Non-Dependent-Types-for-Standard-ML-Modules.pdf"}

   :sha1999
   {:key      "Sha1999"
    :title    "Transparent modules with fully syntactic signatures"
    :author   "Zhong Shao"
    :date     "1999"
    :location (proceedings-location icfp :pages '(220 232))
    :url      "http://flint.cs.yale.edu/flint/publications/fullsig.pdf"
    :tr-url   "http://flint.cs.yale.edu/flint/publications/fullsig-tr.pdf"}

   :els1999a
   {:key      "Els1999a"
    :title    "Program modules, separate compilation, and intermodule optimisation"
    :author   "Martin Elsman"
    :date     1999
    :month    "January"
    :location (dissertation-location :institution "Department of Computer Science, University of Copenhagen" :degree "PhD")
    :url      "https://elsman.com/pdf/phd.pdf"}

   :els1999b
   {:key      "Els1999b"
    :title    "Static interpretation of modules"
    :author   "Martin Elsman"
    :date     1999
    :month    "September"
    :location (proceedings-location icfp)
    :url      "https://elsman.com/pdf/icfp99.pdf"}

   :wv2000
   {:key      "WV2000"
    :title    "Equational reasoning for linking with first-class primitive modules"
    :author   (authors "J. B. Wells" "R. Vestergaard")
    :date     "2000"
    :location (proceedings-location "Programming Languages and Systems" :pages '(412 428))
    :url      "http://www.macs.hw.ac.uk/~jbw/papers/Wells+Vestergaard:Equational-Reasoning-for-Linking-with-First-Class-Primitive-Modules:ESOP-2000.ps.gz"}

   :hs2000
   {:key      "HS2000"
    :title    "A type-theoretic interpretation of Standard ML"
    :author   (authors harper stone)
    :date     2000
    :location (book-location "Proof, language, and interaction: Essays in honor of robin milner" :publisher "MIT Press")
    :url      "https://www.cs.cmu.edu/~rwh/papers/ttisml/ttisml.pdf"}

   :ler2000
   {:key      "Ler2000"
    :title    "A modular module system"
    :author   leroy
    :date     "2000"
    :location (journal-location jfp :volume 10 :number 3 :pages '(269 303))
    :url      "https://xavierleroy.org/publi/modular-modules-jfp.pdf"}

   :rus2000
   {:key      "Rus2000"
    :title    "First-class structures for Standard ML"
    :author   russo
    :date     "2000"
    :location (proceedings-location esop :pages '(336 350))
    :url      "https://link.springer.com/content/pdf/10.1007%2F3-540-46425-5_22.pdf"}

   :rus2001
   {:key      "Rus2001"
    :title    "Recursive structures for Standard ML"
    :author   russo
    :date     "2001"
    :location (proceedings-location icfp :pages '(50 61))
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/2001/09/Recursive-Structures-for-Standard-ML.pdf"}

   :dhc2001
   {:key      "DHC2001"
    :title    "Towards a practical type theory for recursive modules"
    :author   (authors dreyer harper crary)
    :date     2001
    :location (techrpt-location :institution "Carnegie Mellon University School of Computer Science" :number "CMU-CS-01-112")
    :url      "https://www.cs.cmu.edu/~rwh/papers/ttrm/rmtr.pdf"}

   :sew2001
   {:key      "Sew2001"
    :title    "Modules, abstract types, and distributed versioning"
    :author   "Peter Sewell"
    :date     2001
    :location (proceedings-location popl :pages '(236 247))
    :url      "https://www.cl.cam.ac.uk/~pes20/versions-popl.pdf"}

   :hl2002
   {:key      "HL2002"
    :title    "Mixin modules in a call-by-value setting"
    :author   (authors "T. Hirschowitz" leroy)
    :date     "2002"
    :location (proceedings-location esop :pages '(6 20))
    :url      "https://xavierleroy.org/publi/mixins-cbv-esop2002.pdf"}

   :az2002
   {:key      "AZ2002"
    :title    "A calculus of module systems"
    :author   (authors ancona zucca)
    :date     "2002"
    :location (journal-location jfp :volume 12 :number 2 :pages '(91 132))}

   :sp2002
   {:key      "SP2002"
    :title    "First-class modules for Haskell"
    :author   (authors "Mark Shields" "Simon Peyton Jones")
    :date     2002
    :location (proceedings-location fool)
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/first_class_modules.pdf"}

   :djh2002
   {:key      "DJH2002"
    :title    "A formal specification for the Haskell 98 module system"
    :author   (authors "Iavor S. Diatchki" mp-jones "Thomas Hallgren")
    :date     2002
    :month    "October"
    :location (proceedings-location "ACM SIGPLAN 2002 Haskell Workshop")
    :url      "http://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf"}

   :afmz2003
   {:key      "AFMZ2003"
    :title    "Mixin modules and computational effects"
    :author   (authors ancona "Sonia Fagorzi" moggi zucca)
    :date     "2003"
    :location (proceedings-location icalp :pages '(224 238))
    :url      "https://www.disi.unige.it/person/MoggiE/ftp/icalp03.pdf"}

   :ler2003
   {:key      "Ler2003"
    :title    "A proposal for recursive modules in Objective Caml"
    :author   leroy
    :date     "2003"
    :location "Unpublished"
    :url      "http://caml.inria.fr/pub/papers/xleroy-recursive_modules-03.pdf"}

   :dch2003
   {:key      "DCH2003"
    :title    "A type system for higher-order modules"
    :author   (authors dreyer crary harper)
    :date     "2003"
    :location (proceedings-location popl)
    :url      "http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms.pdf"
    :tr-url   "http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms-tr.pdf"}

   :rus2003
   {:key      "Rus2003"
    :title    "Types for modules"
    :author   russo
    :date     2003
    :location (journal-location "Electronic Notes in Theoretical Computer Science" :volume 60)
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/1998/03/Types-for-Modules.pdf"}

   :dre2004
   {:key      "Dre2004"
    :title    "A type system for well-founded recursion"
    :author   dreyer
    :date     2004
    :location (proceedings-location popl :pages '(293 305))
    :url      "https://people.mpi-sws.org/~dreyer/papers/recursion/popl.pdf"
    :tr-url   "https://people.mpi-sws.org/~dreyer/papers/recursion/tr/main.pdf"
    :tr-with  (authors harper crary)
    :tr-date  2003}

   :dre2005a
   {:key      "Dre2005a"
    :title    "Understanding and evolving the ML module system"
    :author   dreyer
    :date     "2005"
    :location (dissertation-location :institution "Carnegie Mellon University, Pittsburgh, Pennsylvania" :degree "PhD")
    :url      "https://people.mpi-sws.org/~dreyer/thesis/main.pdf"}

   :dre2005b
   {:key      "Dre2005b"
    :title    "Recursive type generativity"
    :author   dreyer
    :date     2005
    :location (proceedings-location icfp :pages '(41 53))
    :url      "https://people.mpi-sws.org/~dreyer/papers/dps/main.pdf"}

   :gov2005
   {:key      "Gov2005"
    :title    "Type generativity in higher-order module systems"
    :author   "Paul Govereau"
    :date     2005
    :location (techrpt-location :institution harvard-cs-group :number "TR-05-05")
    :url      "https://dash.harvard.edu/bitstream/handle/1/23853816/tr-05-05.pdf"}

   :rfg2005
   {:key      "RFG2005"
    :title    "An expressive language of signatures"
    :author   (authors "Norman Ramsey" "Kathleen Fisher" "Paul Govereau")
    :date     2005
    :location (proceedings-location icfp :pages '(27 40))
    :url      "https://www.cs.tufts.edu/~nr/pubs/els.pdf"}

   :nig2005
   {:key      "NIG2005"
    :title    "Recursive object-oriented modules"
    :author   (authors nakata "Akira Ito" garrigue)
    :date     2005
    :location (proceedings-location fool)
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/fool_2005.pdf"}

   :mw2005
   {:key      "MW2005"
    :title    "Type inference, principal typings, and let-polymorphism for first-class mixin modules"
    :author   (authors makholm wells)
    :date     2005
    :location (proceedings-location icfp)
    :url      "http://henning.makholm.net/papers/icfp2005.pdf"}

   :ng2006
   {:key      "NG2006"
    :title    "Recursive modules for programming"
    :author   (authors "Keiko Nakata" "Jacques Garrigue")
    :date     "2006"
    :location (proceedings-location icfp :pages '(74 86))
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/nakata-icfp2006.pdf"
    :tr-url   "http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1546.pdf"}

   :of2006
   {:key      "OF2006"
    :title    "From structures and functors to modules and units"
    :author   (authors "Scott Owens" flatt)
    :date     "2006"
    :location (proceedings-location icfp :pages '(87 98))
    :url      "http://www.cs.utah.edu/plt/publications/icfp06-of.pdf"}

   :dre2006
   {:key      "Dre2006"
    :title    "Practical type theory for recursive modules"
    :author   dreyer
    :date     "2006"
    :location (techrpt-location :institution "University of Chicago, Department of Computer Science" :number "TR-2006-07")
    :url      "https://people.mpi-sws.org/~dreyer/papers/bimod/main.pdf"}

   :sha2006
   {:key      "Sha2006"
    :title    "Higher-order modules in System Fω and Haskell"
    :author   "Chung-chieh Shan"
    :date     "May 15, 2006"
    :location "Manuscript"
    :url      "http://homes.soic.indiana.edu/ccshan/xlate/xlate.pdf"}

   :dre2007a
   {:key      "Dre2007a"
    :title    "A type system for recursive modules"
    :author   dreyer
    :date     "2007"
    :location (proceedings-location icfp)
    :url      "https://people.mpi-sws.org/~dreyer/papers/recmod/main-short.pdf"
    :tr-url   "https://people.mpi-sws.org/~dreyer/papers/recmod/main-long.pdf"}

   :dre2007b
   {:key      "Dre2007b"
    :title    "Recursive type generativity"
    :author   dreyer
    :date     2007 ; earlier version: ICFP 2005
    :location (journal-location jfp :volume 17 :number "4&5" :pages '(433 471))
    :url      "https://people.mpi-sws.org/~dreyer/papers/dps/jfp.pdf"}

   :db2007
   {:key      "DB2007"
    :title    "Principal type schemes for modular programs"
    :author   (authors dreyer "Matthias Blume")
    :date     2007
    :location (proceedings-location esop)
    :url      "https://people.mpi-sws.org/~dreyer/papers/infmod/main-short.pdf"
    :tr-url   "https://people.mpi-sws.org/~dreyer/papers/infmod/main-long.pdf"}

   :nak2007
   {:key      "Nak2007"
    :title    "A module system with applicative functors and recursive path references"
    :author   nakata
    :date     2007
    :location (dissertation-location :institution "Kyoto University" :degree "PhD")
    :url      "http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1583.pdf"}

   :ng2007
   {:key      "NG2007"
    :title    "Path resolution for recursive nested modules is undecidable"
    :author   (authors nakata garrigue)
    :date     2007
    :location (proceedings-location "9th International Workshop on Termination")
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/wst2007.pdf"}

   :dr2008
   {:key      "DR2008"
    :title    "Mixin’ up the ML module system"
    :author   (authors dreyer rossberg)
    :date     "2008"
    :location (proceedings-location icfp :pages '(307 320))
    :url      "https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08.pdf"
    :tr-url   "https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08-extended.pdf"}

   :mr2008a
   {:key      "MR2008a"
    :title    "Towards a simpler account of modules and generativity: Abstract types have open existential types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     "January 2008"
    :url      "http://gallium.inria.fr/~remy/modules/fzip.pdf"}

   :mr2008b
   {:key      "MR2008b"
    :title    "A logical account of type generativity: Abstract types have open existential types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     "April 14, 2008"
    :url      "http://gallium.inria.fr/~remy/modules/oat.pdf"
    :slides   "http://gallium.inria.fr/~remy/modules/fzip@msr2008.pdf"}

   :mr2009
   {:key      "MR2009"
    :title    "Modeling abstract types in modules with open existential types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     2009
    :location (proceedings-location popl :pages '(354 365))
    :url      "http://gallium.inria.fr/~remy/modules/Montagu-Remy@popl09:fzip.pdf"}

   :km2009
   {:key      "KM2009"
    :title    "Engineering higher-order modules in SML/NJ"
    :author   (authors "George Kuan" macqueen)
    :date     2009
    :location (proceedings-location ifl :pages '(218 235))
    :url      "https://www.researchgate.net/profile/David_Macqueen/publication/226219412_Engineering_Higher-Order_Modules_in_SMLNJ/links/0912f50a29752482c0000000.pdf"}

   :kua2010
   {:key      "Kua2010"
    :title    "A true higher-order module system"
    :author   "George Kuan"
    :date     2010
    :location (dissertation-location :institution "University of Chicago" :degree "PhD")
    :url      "http://smlnj-gforge.cs.uchicago.edu/scm/viewvc.php/*checkout*/papers/hofsem/dissertation/kuan-dissertation.pdf?root=smlnj"}

   :mon2010
   {:key      "Mon2010"
    :title    "Programming with first-class modules in a core language with subtyping, singleton kinds and open existential types"
    :author   montagu
    :date     2010
    :location (dissertation-location :institution "Ecole Polytechnique X" :degree "PhD")
    :url      "https://pastel.archives-ouvertes.fr/tel-00550331/document"}

   :bo2010
   {:key      "BO2010"
    :title    "A flattening strategy for SML module compilation and its implementation"
    :author   (authors "Liu Bochao" "Atsushi Ohori")
    :date     2010
    :location (journal-location "Information and Media Technologies" :volume 5 :number 1 :pages '(58 76))
    :url      "https://www.jstage.jst.go.jp/article/imt/5/1/5_1_58/_pdf/-char/en"}

   :rrd2010
   {:key      "RRD2010"
    :title    "F-ing modules"
    :author   (authors rossberg russo dreyer)
    :date     2010
    :location (proceedings-location tldi)
    :url      "https://people.mpi-sws.org/~rossberg/f-ing/f-ing.pdf"}

   :fg2010
   {:key      "FG2010"
    :title    "First-class modules and composable signatures in Objective Caml 3.12"
    :author   (authors frisch garrigue)
    :date     2010
    :location (proceedings-location "ML Workshop")
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010.pdf"
    :slides   "http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010-show.pdf"}

   :ingp2011
   {:key      "INGP2011"
    :title    "A syntactic type system for recursive modules"
    :author   (authors "Hyonseung Im" nakata garrigue "Sungwoo Park")
    :date     2011
    :location (proceedings-location oopsla)
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/oopsla2011.pdf"}

   :gn2012
   {:key      "GN2012"
    :title    "Path resolution for recursive nested modules"
    :author   (authors garrigue nakata)
    :date     2012
    :location (journal-location "Higher-Order and Symbolic Computation" :volume 24 :number 3 :pages '(207 237))
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/path-resolution-1205.pdf"}

   :rd2013
   {:key      "DR2013"
    :title    "Mixin’ up the ML module system"
    :author   (authors rossberg dreyer)
    :date     2013
    :location (proceedings-location toplas)
    :url      "https://people.mpi-sws.org/~rossberg/mixml/mixml-toplas.pdf"}

   :inp2013
   {:key      "INP2013"
    :title    "Contractive signatures with recursive types, type parameters, and abstract types"
    :author   (authors "Hyeonseung Im" nakata "Sungwoo Park")
    :date     2013
    :location (proceedings-location icalp)
    :url      "http://pl.postech.ac.kr/~gla/paper/icalp2013.pdf"}

   :rrd2014
   {:key      "RRD2014"
    :title    "F-ing modules"
    :author   (authors rossberg russo dreyer)
    :date     2014
    :month    "September"
    :location (journal-location jfp :volume 24 :number 5)
    :url      "https://people.mpi-sws.org/~rossberg/f-ing/f-ing-jfp.pdf"}

   :kdpm2014
   {:key      "KDPM2014"
    :title    "Backpack: Retrofitting Haskell with interfaces"
    :author   (authors "Scott Kilpatrick" dreyer "Simon Peyton Jones" "Simon Marlow")
    :date     2014
    :location (proceedings-location popl :pages '(19 31))
    :url      "https://people.mpi-sws.org/~dreyer/papers/backpack/paper.pdf"
    :appendix "https://people.mpi-sws.org/~dreyer/papers/backpack/appendix.pdf"
    :slides   "https://plv.mpi-sws.org/backpack/backpack-popl.pdf"}

   :gw2014
   {:key      "GW2014"
    :title    "Type-level module aliases: independent and equal"
    :author   (authors garrigue white)
    :date     2014
    :month    "September"
    :location (proceedings-location "ML Family Workshop")
    :url      "http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias.pdf"
    :slides   "http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias-show.pdf"}

   :ros2015
   {:key      "Ros2015"
    :title    "1ML — Core and modules united (F-ing first-class modules)"
    :author   rossberg
    :date     2015
    :location (proceedings-location icfp)
    :url      "https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf"
    :tr-url   "https://people.mpi-sws.org/~rossberg/1ml/1ml-extended.pdf"}

   :ros2016
   {:key      "Ros2016"
    :title    "1ML with special effects (F-ing generativity polymorphism)"
    :author   rossberg
    :date     2016
    :location "In WadlerFest"
    :url      "https://people.mpi-sws.org/~rossberg/1ml/1ml-effects.pdf"}

   :pykd2016
   {:key      "PYKD2016"
    :title    "Backpack to work: Towards practical mixin linking for Haskell"
    :author   (authors "Simon Peyton Jones" "Edward Yang" "Scott Kilpatrick" dreyer)
    :date     "March 2016"
    :location "In submission"
    :url      "https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf"}

   :yan2017
   {:key      "Yan2017"
    :title    "Backpack: Towards practical mix-in linking in Haskell"
    :author   "Edward Z. Yang"
    :date     "June 2017"
    :location (dissertation-location :institution "Stanford University" :degree "PhD")
    :url      "https://github.com/ezyang/thesis/releases"}

   :ly2017
   {:key      "LY2017"
    :title    "Extending OCaml's open (extended abstract)"
    :author   (authors "Runhang Li" "Jeremy Yallop")
    :date     2017
    :location (proceedings-location "The OCaml Users and Developers Workshop")
    :url      "https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open.pdf"}

   :cra2017
   {:key      "Cra2017"
    :title    "Modules, abstraction, and parametric polymorphism"
    :author   crary
    :date     "2017"
    :location (proceedings-location popl)
    :url      "http://www.cs.cmu.edu/~crary/papers/2017/mapp.pdf"}

   :ehao2018
   {:key      "EHAO2018"
    :title    "Static interpretation of higher-order modules in Futhark: Functional GPU programming in the large"
    :author   (authors "Martin Elsman" "Troels Henriksen" "Danil Annenkov" "Cosmin E. Oancea")
    :date     2018
    :location (proceedings-location icfp :pages '(1 30))
    :url      "https://futhark-lang.org/publications/icfp18.pdf"}

   :ros2018
   {:key      "Ros2018"
    :title    "1ML — Core and modules united"
    :author   rossberg
    :date     2018
    :location (journal-location jfp :volume 28 :pages "e22")
    :url      "https://people.mpi-sws.org/~rossberg/papers/Rossberg%20-%201ML%20--%20Core%20and%20modules%20united%20[JFP].pdf"}

   :cra2019
   {:key      "Cra2019"
    :title    "Fully abstract module compilation"
    :author   crary
    :date     2019
    :location (journal-location popl :volume 3 :number "POPL" :pages '("10:1" "10:29"))
    :url      "https://dl.acm.org/ft_gateway.cfm?id=3290323"
    :slides   "https://popl19.sigplan.org/event/popl-2019-research-papers-fully-abstract-module-compilation"}

   :rfto2019
   {:key      "RFTO2019"
    :title    "Characterising renaming within OCaml’s module system: Theory and implementation"
    :author   (authors "Reuben N. S. Rowe" "Hugo Férée" "Simon J. Thompson" "Scott Owens")
    :date     2019
    :location (proceedings-location pldi :pages '(950 965))
    :url      "https://www.cs.kent.ac.uk/people/staff/rnsr/docs/renaming-pldi2019.pdf"}

   :ly
   {:key      "LY"
    :title    "Extending OCaml's open"
    :author   (authors "Runhang Li" "Jeremy Yallop")
    :date     "to appear" ; 2019
    :location (proceedings-location "ML & OCaml 2017 post-proceedings")
    :url      "https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open-draft.pdf"}
   ))

(defn -main
  []
  (write "../README.md" (str "# Modules\n\n"
                             "Type-theoretic analysis of ML-style modules.\n\n"
                             "This repository contains "
                             (hiccup/html [:ul
                                           [:li
                                            "An implementation ("
                                            [:a {:href "https://github.com/elpinal/modules/blob/master/src/Language/Modules/RRD2014.hs"}
                                             "RRD2014.hs"]
                                            ") of F-ing modules ("
                                            [:a {:href "https://github.com/elpinal/modules#f-ing-modules-1"}
                                             "Rossberg et al. 2014"]
                                            "),"]
                                           [:li
                                            "An interpreter ("
                                            [:a {:href "https://github.com/elpinal/modules/blob/master/src/Language/Modules/Ros2018.hs"}
                                             "Ros2018.hs"]
                                            ") of 1ML" [:sub "ex"] "("
                                            [:a {:href "https://github.com/elpinal/modules#1ml--core-and-modules-united"}
                                             "Rossberg 2018"]
                                            "), and"]
                                           [:li "A bibliography of modules and data abstraction, which is given below."]])
                             "\n\n"
                             "## See also\n\n"
                             "- [elpinal/modules-rs](https://github.com/elpinal/modules-rs)\n"
                             "contains another implementation of F-ing modules in Rust (with more bugs fixed than Haskell implementation here)."
                             "\n\n"
                             "## 1ML interpreter\n\n"
                             "`stack install` installs `1mlex`, which is an interpreter of 1ML *without* type inference.\n\n"
                             "Status: All parts of [Rossberg 2018], except type inference, are implemented. There are few known bugs.\n\n"
                             "- [elpinal/1ml-vim](https://github.com/elpinal/1ml-vim) is a Vim plugin providing syntax highlighting for 1ML."
                             "\n\n"
                             "## Bibliography of Modules and Data Abstraction\n\n"
                             "_DISCLAIMER: There is no warranty of accuracy._\n\n"
                             (html-entries (vals entries)))))
