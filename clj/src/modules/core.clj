(ns modules.core
  (:require [hiccup.core :as hiccup]))

(def icalp "ICALP")
(def icfp "ICFP")
(def jfp "Journal of Functional Programming")
(def pldi "PLDI")
(def popl "POPL")
(def ppdp "Principles and Practice of Declarative Programming")
(def tapsoft "TAPSOFT")
(def tldi "Types in Language Design and Implementation")

(def ancona "Davide Ancona")
(def cardelli "Luca Cardelli")
(def crary "Karl Crary")
(def harper "Robert Harper")
(def leroy "Xavier Leroy")
(def macqueen "David B. MacQueen")
(def mitchell "John C. Mitchell")
(def moggi "Eugenio Moggi")
(def rossberg "Andreas Rossberg")
(def russo "Claudio V. Russo")
(def tofte "Mads Tofte")
(def zucca "Elena Zucca")

(defn write
  [fname x]
  (with-open [w (clojure.java.io/writer fname)]
    (.write w x)))

(defn render
  [{:keys [key title author date location]}]
  [:div
   [:h4 title]
   [:p author]
   [:p (str (if (sequential? location)
              (apply str (interpose ", " (map #(hiccup/html %) location) ))
              location) ", " date)]])

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

(defn journal-location
  [title & {:keys [pages volume number]}]
  [[:i title] (str volume "(" number ")" (if pages (str ", pp. " (str-pages pages))))])

(def rey1983
  {:key      "Rey1983"
   :title    "Types, abstraction and parametric polymorphism"
   :author   "J. C. Reynolds"
   :date     "1983"
   :location (proceedings-location "Information Processing" :pages '(513 523))})

(def bl1984
  {:key      "BL1984"
   :title    "A kernel language for abstract data types and modules"
   :author   (authors "R. Burstall" "B. Lampson")
   :date     "1984"
   :location (proceedings-location "Semantics of Data Types" :pages '(1 50))})

(def mp1985
  {:key      "MP1985"
   :title    "Abstract types have existential types"
   :author   (authors mitchell "G. D. Plotkin")
   :date     "1985"
   :location (proceedings-location popl :pages '(37 51))})

(def cw1985
  {:key      "CW1985"
   :title    "On understanding types, data abstraction, and polymorphism"
   :author   (authors cardelli "P. Wegner")
   :date     "1985"
   :location (techrpt-location :institution "Brown University" :number "CS-85-14")})

(def mac1985
  {:key      "Mac1985"
   :title    "Modules for Standard ML (Revised)"
   :author   macqueen
   :date     "1985"
   :location "Polymorphism Newsletter, II, 2"})

(def mit1986
  {:key      "Mit1986"
   :title    "Representation independence and data abstraction"
   :author   mitchell
   :date     "1986"
   :location (proceedings-location popl)})

(def mac1986
  {:key      "Mac1986"
   :title    "Using dependent types to express modular structure"
   :author   macqueen
   :date     "1986"
   :location (proceedings-location popl :pages '(277 286))})

(def hmt1987
  {:key      "HMT1987"
   :title    "A type discipline for program modules"
   :author   (authors harper "R. Milner" tofte)
   :date     "1987"
   :location (proceedings-location tapsoft)})

(def cm1988
  {:key      "CM1988"
   :title    "Persistence and Type Abstraction"
   :author   (authors cardelli macqueen)
   :date     "1988. First appeared in 1985"
   :location "Data types and persistence, Springer-Verlag"})

(def car1988
  {:key      "Car1988"
   :title    "Phase distinctions in type theory"
   :author   cardelli
   :date     "1988"
   :location "Manuscript"})

(def mog1989
  {:key      "Mog1989"
   :title    "A category-theoretic account of program modules"
   :author   moggi
   :date     "1989"
   :location (proceedings-location "Category Theory and Computer Science" :pages '(101 117))})

(def hmm1990
  {:key      "HMM1990"
   :title    "Higher-order modules and the phase distinction"
   :author   (authors harper mitchell moggi)
   :date     "1990"
   :location (proceedings-location popl :pages '(341 354))})

(def cl1990
  {:key      "CL1990"
   :title    "Abstract types and the dot notation"
   :author   (authors cardelli leroy)
   :date     "1990"
   :location (proceedings-location "IFIP TC2 working conference on programming concepts and methods" :pages '(479 504))})

(def bc1990
  {:key      "BC1990"
   :title    "Mixin-based inheritance"
   :author   (authors "G. Bracha" "W. Cook")
   :date     "1990"
   :location (proceedings-location "OOPSLA/ECOOP")})

(def car1991
  {:key      "Car1991"
   :title    "Typeful programming"
   :author   cardelli
   :date     "1991"
   :location (proceedings-location "Formal Description of Programming Concepts")})

(def mmm1991
  {:key      "MMM1991"
   :title    "An extension of Standard ML modules with subtyping and inheritance"
   :author   (authors mitchell "Sigurd Meldal" "Neel Madhav")
   :date     "1991"
   :location (proceedings-location popl :pages '(270 278))})

(def bra1992
  {:key      "Bra1992"
   :title    "The Programming Language JIGSAW: Mixins, Modularity and Multiple Inheritance"
   :author   "G. Bracha"
   :date     "1992"
   :location (dissertation-location :institution "Department of Comp. Sci., Univ. of Utah" :degree "PhD")})

(def tof1992
  {:key      "Tof1992"
   :title    "Principal signatures for higher-order program modules"
   :author   tofte
   :date     "1992"
   :location (proceedings-location popl :pages '(189 199))})

(def apo1993
  {:key      "Apo1993"
   :title    "Extending record typing to type parametric modules with sharing"
   :author   "María Virginia Aponte"
   :date     "1993"
   :location (proceedings-location popl :pages '(465 478))})

(def mg1993
  {:key      "MG1993"
   :title    "Studying the ML Module System in HOL"
   :author   (authors "Savi Maharaj" "Elsa Gunter")
   :date     "1993"
   :location (journal-location "The Computer Journal" :volume 36 :number 5)})

(def hl1994
  {:key      "HL1994"
   :title    "A type-theoretic approach to higher-order modules with sharing"
   :author   (authors harper "Mark Lillibridge")
   :date     "1994"
   :location (proceedings-location popl :pages '(123 137))})

(def ler1994
  {:key      "Ler1994"
   :title    "Manifest types, modules, and separate compilation"
   :author   leroy
   :date     "1994"
   :location (proceedings-location popl :pages '(109 122))})

(def mt1994
  {:key      "MT1994"
   :title    "A semantics for higher-order functors"
   :author   (authors macqueen tofte)
   :date     "1994"
   :location (proceedings-location "Programming Languages and Systems – ESOP ’94" :pages '(409 423))})

(def bis1995
  {:key      "Bis1995"
   :title    "Higher-order functors with transparent signatures"
   :author   "S. K. Biswas"
   :date     "1995"
   :location (proceedings-location popl :pages '(154 163))})

(def ler1995
  {:key      "Ler1995"
   :title    "Applicative functors and fully transparent higher-order modules"
   :author   leroy
   :date     "1995"
   :location (proceedings-location popl :pages '(142 153))})

(def ds1996
  {:key      "DS1996"
   :title    "Mixin modules"
   :author   (authors "Dominic Duggan" "Constantinos Sourelis")
   :date     "1996"
   :location (proceedings-location icfp :pages '(262 273))})

(def ler1996
  {:key      "Ler1996"
   :title    "A syntactic theory of type generativity and sharing"
   :author   leroy
   :date     "1996"
   :location (journal-location jfp :number 5 :volume 6 :pages '(667 698))})

(def nj1996
  {:key      "NJ1996"
   :title    "An exploration of modular programs"
   :author   (authors "Jan Nicklish" "Simon Peyton Jones")
   :date     "1996"
   :location "In The Glasgow Workshop on Functional Programming"})

(def asp1997
  {:key      "Asp1997"
   :title    "Type Systems for Modular Programs and Specifications"
   :author   "David R. Aspinall"
   :date     "1997"
   :location (dissertation-location :institution "Edinburgh University, Edinburgh, Scotland" :degree "PhD")})

(def cou1997
  {:key      "Cou1997"
   :title    "An applicative module calculus"
   :author   "Judicäel Courant"
   :date     "1997"
   :location (proceedings-location tapsoft)})

(def rus1998
  {:key      "Rus1998"
   :title    "Types for modules"
   :author   russo
   :date     "1998"
   :location (dissertation-location :institution "University of Edinburgh, UK" :degree "PhD")})

(def sha1998
  {:key      "Sha1998"
   :title    "Typed cross-module compilation"
   :author   "Zhong Shao"
   :date     "1998"
   :location (proceedings-location icfp :pages '(141 152))})

(def ds1998
  {:key      "DS1998"
   :title    "Parameterized modules, recursive modules and mixin modules"
   :author   (authors "Dominic Duggan" "Constantinos Sourelis")
   :date     "1998"
   :location "In ACM SIGPLAN Workshop on ML, pages 87–96, Baltimore, MA, USA, September 1998"})

(def ff1998a
  {:key      "FF1998a"
   :title    "Modular object-oriented programming with units and mixins"
   :author   (authors "R. B. Findler" "M. Flatt")
   :date     "1998"
   :location (proceedings-location icfp :pages '(94 104))})

(def ff1998b
  {:key      "FF1998b"
   :title    "Units: Cool modules for HOT languages"
   :author   (authors "M. Flatt" "M. Felleisen")
   :date     "1998"
   :location (proceedings-location pldi :pages '(236 248))})

(def az1998a
  {:key      "AZ1998a"
   :title    "A Theory of Mixin Modules: Basic and Derived Operators"
   :author   (authors ancona zucca)
   :date     "1998"
   :location (journal-location "Mathematical Structures in Computer Science" :volume 8 :number 4 :pages '(401 446))})

(def az1998b
  {:key      "AZ1998b"
   :title    "An algebra of mixin modules"
   :author   (authors ancona zucca)
   :date     "1998"
   :location "In 12th Workshop on Algebraic Development Techniques - Selected Papers, pp. 92–106"})

(def az1999
  {:key      "AZ1999"
   :title    "A primitive calculus for module systems"
   :author   (authors ancona zucca)
   :date     "1999"
   :location (proceedings-location ppdp :pages '(62 79))})

(def chp1999
  {:key      "CHP1999"
   :title    "What is a recursive module?"
   :author   (authors crary harper "Sidd Puri")
   :date     "1999"
   :location (proceedings-location pldi :pages '(50 63))})

(def rus1999
  {:key      "Rus1999"
   :title    "Non-dependent Types for Standard ML Modules"
   :author   russo
   :date     "1999"
   :location (proceedings-location ppdp :pages '(80 97))})

(def sha1999
  {:key      "Sha1999"
   :title    "Transparent modules with fully syntactic signatures"
   :author   "Zhong Shao"
   :date     "1999"
   :location (proceedings-location icfp :pages '(220 232))})

(def wv2000
  {:key      "WV2000"
   :title    "Equational reasoning for linking with first-class primitive modules"
   :author   (authors "J. B. Wells" "R. Vestergaard")
   :date     "2000"
   :location (proceedings-location "Programming Languages and Systems" :pages '(412 428))})

(def ler2000
  {:key      "Ler2000"
   :title    "A modular module system"
   :author   leroy
   :date     "2000"
   :location (journal-location jfp :volume 10 :number 3 :pages '(269 303))})

(def rus2000
  {:key      "Rus2000"
   :title    "First-class structures for Standard ML"
   :author   russo
   :date     "2000"
   :location (journal-location "Nordic Journal of Computing" :volume 7 :number 4 :pages '(348 374))})

(def rus2001
  {:key      "Rus2001"
   :title    "Recursive structures for Standard ML"
   :author   russo
   :date     "2001"
   :location (proceedings-location icfp :pages '(50 61))})

(def hl2002
  {:key      "HL2002"
   :title    "Mixin modules in a call-by-value setting"
   :author   (authors "T. Hirschowitz" leroy)
   :date     "2002"
   :location (proceedings-location "Programming Languages and Systems" :pages '(6 20))})

(def az2002
  {:key      "AZ2002"
   :title    "A calculus of module systems"
   :author   (authors ancona zucca)
   :date     "2002"
   :location (journal-location jfp :volume 12 :number 2 :pages '(91 132))})

(def afmz2003
  {:key      "AFMZ2003"
   :title    "Mixin modules and computational effects"
   :author   (authors ancona "Sonia Fagorzi" moggi zucca)
   :date     "2003"
   :location (proceedings-location icalp :pages '(224 238))})

(def ler2003
  {:key      "Ler2003"
   :title    "proposal for recursive modules in Objective Caml"
   :author   leroy
   :date     "2003"
   :location "Available at http://caml.inria.fr/about/papers.en.html"})

(def dch2003
  {:key      "DCH2003"
   :title    "A type system for higher-order modules"
   :author   (authors "Derek Dreyer" crary harper)
   :date     "2003"
   :location (proceedings-location popl)})

(def dre2005
  {:key      "Dre2005"
   :title    "Understanding and Evolving the ML Module System"
   :author   "Derek Dreyer"
   :date     "2005"
   :location (dissertation-location :institution "Carnegie Mellon University, Pittsburgh, Pennsylvania" :degree "PhD")})

(def ng2006
  {:key      "NG2006"
   :title    "Recursive modules for programming"
   :author   (authors "Keiko Nakata" "Jacques Garrigue")
   :date     "2006"
   :location (proceedings-location icfp :pages '(74 86))})

(def of2006
  {:key      "OF2006"
   :title    "From structures and functors to modules and units"
   :author   (authors "Scott Owens" "Matthew Flatt")
   :date     "2006"
   :location (proceedings-location icfp :pages '(87 98))})

(def dre2006
  {:key      "Dre2006"
   :title    "Practical type theory for recursive modules"
   :author   "Derek Dreyer"
   :date     "2006"
   :location (techrpt-location :institution "University of Chicago, Department of Computer Science" :number "TR-2006-07")})

(def dre2007a
  {:key      "Dre2007a"
   :title    "A Type System for Recursive Modules"
   :author   "Derek Dreyer"
   :date     "2007"
   :location (proceedings-location icfp)})

(def dre2007b
  {:key      "Dre2007b"
   :title    "Recursive Type Generativity"
   :author   "Derek Dreyer"
   :date     "2007"
   :location (journal-location jfp :volume 17 :number "4&5" :pages '(433 471))})

(def dr2008
  {:key      "DR2008"
   :title    "Mixin' up the ML module system"
   :author   (authors "Derek Dreyer" rossberg)
   :date     "2008"
   :location (proceedings-location icfp :pages '(307 320))})

(def rrd2010
  {:key      "RRD2010"
   :title    "F-ing Modules"
   :author   (authors rossberg russo "Derek Dreyer")
   :date     "2010; 2014 (JFP)"
   :location (proceedings-location tldi)})

(def ros2015
  {:key      "Ros2015"
   :title    "1ML — Core and modules united (F-ing first-class modules)"
   :author   rossberg
   :date     "2015; 2016 (JFP); 2018 (updated draft)"
   :location (proceedings-location icfp)})

(def ros2016
  {:key      "Ros2016"
   :title    "1ML with Special Effects (F-ing Generativity Polymorphism)"
   :author   rossberg
   :date     "2016"
   :location "In WadlerFest"})

(def cra2017
  {:key      "Cra2017"
   :title    "Modules, Abstraction, and Parametric Polymorphism"
   :author   crary
   :date     "2017"
   :location (proceedings-location popl)})

(def cra2019
  {:key      "Cra2019"
   :title    "Fully Abstract Module Compilation"
   :author   crary
   :date     "2019, to appear"
   :location (proceedings-location popl)})

(def entries
  [rey1983
   bl1984
   mp1985
   cw1985
   mac1985
   mit1986
   mac1986
   hmt1987
   cm1988
   car1988
   mog1989
   hmm1990
   cl1990
   bc1990
   car1991
   mmm1991
   bra1992
   tof1992
   apo1993
   mg1993
   hl1994
   ler1994
   mt1994
   bis1995
   ler1995
   ds1996
   ler1996
   nj1996
   asp1997
   cou1997
   rus1998
   sha1998
   ds1998
   ff1998a
   ff1998b
   az1998a
   az1998b
   az1999
   chp1999
   rus1999
   sha1999
   wv2000
   ler2000
   rus2000
   rus2001
   hl2002
   az2002
   afmz2003
   ler2003
   dch2003
   dre2005
   ng2006
   of2006
   dre2006
   dre2007a
   dre2007b
   dr2008
   rrd2010
   ros2015
   ros2016
   cra2017
   cra2019])

(defn -main
  []
  (write "../README.md" (str "# Modules\n\n"
                             "Type-theoretic analysis of ML-style modules.\n\n"
                             "## Incomplete Bibliography of Modules and Type Abstraction\n\n"
                             (html-entries entries))))
