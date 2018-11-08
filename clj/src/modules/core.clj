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
(def dreyer "Derek Dreyer")
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
  [{:keys [key title author date location url slides]}]
  [:div
   [:h4 title]
   [:p author]
   [:p (str (if (sequential? location)
              (apply str (interpose ", " (map #(hiccup/html %) location)))
              location)
            (if location ", ") date)]
   (if url
     [:p "Available at "
      [:a {:href url} url]])
   (if slides
     [:p "Slides: "
      [:a {:href slides} slides]])])

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

(def entries
  (array-map
   :rey1983
   {:key      "Rey1983"
    :title    "Types, abstraction and parametric polymorphism"
    :author   "J. C. Reynolds"
    :date     "1983"
    :location (proceedings-location "Information Processing" :pages '(513 523))}

   :bl1984
   {:key      "BL1984"
    :title    "A kernel language for abstract data types and modules"
    :author   (authors "R. Burstall" "B. Lampson")
    :date     "1984"
    :location (proceedings-location "Semantics of Data Types" :pages '(1 50))}

   :mp1985
   {:key      "MP1985"
    :title    "Abstract types have existential types"
    :author   (authors mitchell "G. D. Plotkin")
    :date     "1985"
    :location (proceedings-location popl :pages '(37 51))}

   :cw1985
   {:key      "CW1985"
    :title    "On understanding types, data abstraction, and polymorphism"
    :author   (authors cardelli "P. Wegner")
    :date     "1985"
    :location (techrpt-location :institution "Brown University" :number "CS-85-14")}

   :mac1985
   {:key      "Mac1985"
    :title    "Modules for Standard ML (Revised)"
    :author   macqueen
    :date     "1985"
    :location "Polymorphism Newsletter, II, 2"}

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
    :location (proceedings-location popl :pages '(277 286))}

   :hmt1987
   {:key      "HMT1987"
    :title    "A type discipline for program modules"
    :author   (authors harper "R. Milner" tofte)
    :date     "1987"
    :location (proceedings-location tapsoft)}

   :cm1988
   {:key      "CM1988"
    :title    "Persistence and Type Abstraction"
    :author   (authors cardelli macqueen)
    :date     "1988. First appeared in 1985"
    :location "Data types and persistence, Springer-Verlag"}

   :car1988
   {:key      "Car1988"
    :title    "Phase distinctions in type theory"
    :author   cardelli
    :date     "1988"
    :location "Manuscript"}

   :mog1989
   {:key      "Mog1989"
    :title    "A category-theoretic account of program modules"
    :author   moggi
    :date     "1989"
    :location (proceedings-location "Category Theory and Computer Science" :pages '(101 117))}

   :hmm1990
   {:key      "HMM1990"
    :title    "Higher-order modules and the phase distinction"
    :author   (authors harper mitchell moggi)
    :date     "1990"
    :location (proceedings-location popl :pages '(341 354))}

   :cl1990
   {:key      "CL1990"
    :title    "Abstract types and the dot notation"
    :author   (authors cardelli leroy)
    :date     "1990"
    :location (proceedings-location "IFIP TC2 working conference on programming concepts and methods" :pages '(479 504))}

   :bc1990
   {:key      "BC1990"
    :title    "Mixin-based inheritance"
    :author   (authors "G. Bracha" "W. Cook")
    :date     "1990"
    :location (proceedings-location "OOPSLA/ECOOP")}

   :car1991
   {:key      "Car1991"
    :title    "Typeful programming"
    :author   cardelli
    :date     "1991"
    :location (proceedings-location "Formal Description of Programming Concepts")}

   :mmm1991
   {:key      "MMM1991"
    :title    "An extension of Standard ML modules with subtyping and inheritance"
    :author   (authors mitchell "Sigurd Meldal" "Neel Madhav")
    :date     "1991"
    :location (proceedings-location popl :pages '(270 278))}

   :bra1992
   {:key      "Bra1992"
    :title    "The Programming Language JIGSAW: Mixins, Modularity and Multiple Inheritance"
    :author   "G. Bracha"
    :date     "1992"
    :location (dissertation-location :institution "Department of Comp. Sci., Univ. of Utah" :degree "PhD")}

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
    :location (proceedings-location popl :pages '(465 478))}

   :mg1993
   {:key      "MG1993"
    :title    "Studying the ML Module System in HOL"
    :author   (authors "Savi Maharaj" "Elsa Gunter")
    :date     "1993"
    :location (journal-location "The Computer Journal" :volume 36 :number 5)}

   :hl1994
   {:key      "HL1994"
    :title    "A type-theoretic approach to higher-order modules with sharing"
    :author   (authors harper "Mark Lillibridge")
    :date     "1994"
    :location (proceedings-location popl :pages '(123 137))}

   :ler1994
   {:key      "Ler1994"
    :title    "Manifest types, modules, and separate compilation"
    :author   leroy
    :date     "1994"
    :location (proceedings-location popl :pages '(109 122))}

   :mt1994
   {:key      "MT1994"
    :title    "A semantics for higher-order functors"
    :author   (authors macqueen tofte)
    :date     "1994"
    :location (proceedings-location "Programming Languages and Systems – ESOP ’94" :pages '(409 423))}

   :bis1995
   {:key      "Bis1995"
    :title    "Higher-order functors with transparent signatures"
    :author   "S. K. Biswas"
    :date     "1995"
    :location (proceedings-location popl :pages '(154 163))}

   :ler1995
   {:key      "Ler1995"
    :title    "Applicative functors and fully transparent higher-order modules"
    :author   leroy
    :date     "1995"
    :location (proceedings-location popl :pages '(142 153))}

   :ds1996
   {:key      "DS1996"
    :title    "Mixin modules"
    :author   (authors "Dominic Duggan" "Constantinos Sourelis")
    :date     "1996"
    :location (proceedings-location icfp :pages '(262 273))}

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
    :location (journal-location jfp :number 5 :volume 6 :pages '(667 698))}

   :nj1996
   {:key      "NJ1996"
    :title    "An exploration of modular programs"
    :author   (authors "Jan Nicklish" "Simon Peyton Jones")
    :date     "1996"
    :location "In The Glasgow Workshop on Functional Programming"}

   :asp1997
   {:key      "Asp1997"
    :title    "Type Systems for Modular Programs and Specifications"
    :author   "David R. Aspinall"
    :date     "1997"
    :location (dissertation-location :institution "Edinburgh University, Edinburgh, Scotland" :degree "PhD")}

   :cou1997
   {:key      "Cou1997"
    :title    "An applicative module calculus"
    :author   "Judicäel Courant"
    :date     "1997"
    :location (proceedings-location tapsoft)}

   :rus1998
   {:key      "Rus1998"
    :title    "Types for modules"
    :author   russo
    :date     "1998"
    :location (dissertation-location :institution "University of Edinburgh, UK" :degree "PhD")}

   :sha1998
   {:key      "Sha1998"
    :title    "Typed cross-module compilation"
    :author   "Zhong Shao"
    :date     "1998"
    :location (proceedings-location icfp :pages '(141 152))}

   :ds1998
   {:key      "DS1998"
    :title    "Parameterized modules, recursive modules and mixin modules"
    :author   (authors "Dominic Duggan" "Constantinos Sourelis")
    :date     "1998"
    :location "In ACM SIGPLAN Workshop on ML, pages 87–96, Baltimore, MA, USA, September 1998"}

   :ff1998a
   {:key      "FF1998a"
    :title    "Modular object-oriented programming with units and mixins"
    :author   (authors "R. B. Findler" "M. Flatt")
    :date     "1998"
    :location (proceedings-location icfp :pages '(94 104))}

   :ff1998b
   {:key      "FF1998b"
    :title    "Units: Cool modules for HOT languages"
    :author   (authors "M. Flatt" "M. Felleisen")
    :date     "1998"
    :location (proceedings-location pldi :pages '(236 248))}

   :az1998a
   {:key      "AZ1998a"
    :title    "A Theory of Mixin Modules: Basic and Derived Operators"
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
    :location (proceedings-location pldi :pages '(50 63))}

   :rus1999
   {:key      "Rus1999"
    :title    "Non-dependent Types for Standard ML Modules"
    :author   russo
    :date     "1999"
    :location (proceedings-location ppdp :pages '(80 97))}

   :sha1999
   {:key      "Sha1999"
    :title    "Transparent modules with fully syntactic signatures"
    :author   "Zhong Shao"
    :date     "1999"
    :location (proceedings-location icfp :pages '(220 232))}

   :wv2000
   {:key      "WV2000"
    :title    "Equational reasoning for linking with first-class primitive modules"
    :author   (authors "J. B. Wells" "R. Vestergaard")
    :date     "2000"
    :location (proceedings-location "Programming Languages and Systems" :pages '(412 428))}

   :ler2000
   {:key      "Ler2000"
    :title    "A modular module system"
    :author   leroy
    :date     "2000"
    :location (journal-location jfp :volume 10 :number 3 :pages '(269 303))}

   :rus2000
   {:key      "Rus2000"
    :title    "First-class structures for Standard ML"
    :author   russo
    :date     "2000"
    :location (journal-location "Nordic Journal of Computing" :volume 7 :number 4 :pages '(348 374))}

   :rus2001
   {:key      "Rus2001"
    :title    "Recursive structures for Standard ML"
    :author   russo
    :date     "2001"
    :location (proceedings-location icfp :pages '(50 61))}

   :hl2002
   {:key      "HL2002"
    :title    "Mixin modules in a call-by-value setting"
    :author   (authors "T. Hirschowitz" leroy)
    :date     "2002"
    :location (proceedings-location "Programming Languages and Systems" :pages '(6 20))}

   :az2002
   {:key      "AZ2002"
    :title    "A calculus of module systems"
    :author   (authors ancona zucca)
    :date     "2002"
    :location (journal-location jfp :volume 12 :number 2 :pages '(91 132))}

   :afmz2003
   {:key      "AFMZ2003"
    :title    "Mixin modules and computational effects"
    :author   (authors ancona "Sonia Fagorzi" moggi zucca)
    :date     "2003"
    :location (proceedings-location icalp :pages '(224 238))}

   :ler2003
   {:key      "Ler2003"
    :title    "proposal for recursive modules in Objective Caml"
    :author   leroy
    :date     "2003"
    :location "Available at http://caml.inria.fr/about/papers.en.html"}

   :dch2003
   {:key      "DCH2003"
    :title    "A type system for higher-order modules"
    :author   (authors dreyer crary harper)
    :date     "2003"
    :location (proceedings-location popl)}

   :dre2005
   {:key      "Dre2005"
    :title    "Understanding and Evolving the ML Module System"
    :author   dreyer
    :date     "2005"
    :location (dissertation-location :institution "Carnegie Mellon University, Pittsburgh, Pennsylvania" :degree "PhD")}

   :ng2006
   {:key      "NG2006"
    :title    "Recursive modules for programming"
    :author   (authors "Keiko Nakata" "Jacques Garrigue")
    :date     "2006"
    :location (proceedings-location icfp :pages '(74 86))}

   :of2006
   {:key      "OF2006"
    :title    "From structures and functors to modules and units"
    :author   (authors "Scott Owens" "Matthew Flatt")
    :date     "2006"
    :location (proceedings-location icfp :pages '(87 98))}

   :dre2006
   {:key      "Dre2006"
    :title    "Practical type theory for recursive modules"
    :author   dreyer
    :date     "2006"
    :location (techrpt-location :institution "University of Chicago, Department of Computer Science" :number "TR-2006-07")}

   :dre2007a
   {:key      "Dre2007a"
    :title    "A Type System for Recursive Modules"
    :author   dreyer
    :date     "2007"
    :location (proceedings-location icfp)}

   :dre2007b
   {:key      "Dre2007b"
    :title    "Recursive Type Generativity"
    :author   dreyer
    :date     "2007"
    :location (journal-location jfp :volume 17 :number "4&5" :pages '(433 471))}

   :dr2008
   {:key      "DR2008"
    :title    "Mixin’ up the ML module system"
    :author   (authors dreyer rossberg)
    :date     "2008"
    :location (proceedings-location icfp :pages '(307 320))}

   :mr2008a
   {:key      "MR2008a"
    :title    "Towards a Simpler Account of Modules and Generativity: Abstract Types have Open Existential Types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     "January 2008"
    :url      "http://gallium.inria.fr/~remy/modules/fzip.pdf"}

   :mr2008b
   {:key      "MR2008b"
    :title    "A Logical Account of Type Generativity: Abstract types have open existential types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     "April 14, 2008"
    :url      "http://gallium.inria.fr/~remy/modules/oat.pdf"
    :slides   "http://gallium.inria.fr/~remy/modules/fzip@msr2008.pdf"}

   :mr2009
   {:key      "MR2009"
    :title    "Modeling Abstract Types in Modules with Open Existential Types"
    :author   (authors "Benoît Montagu" "Didier Rémy")
    :date     2009
    :location (proceedings-location popl :pages '(354 365))
    :url      "http://gallium.inria.fr/~remy/modules/Montagu-Remy@popl09:fzip.pdf"}

   :rrd2010
   {:key      "RRD2010"
    :title    "F-ing Modules"
    :author   (authors rossberg russo dreyer)
    :date     "2010; 2014 (JFP)"
    :location (proceedings-location tldi)}

   :ros2015
   {:key      "Ros2015"
    :title    "1ML — Core and modules united (F-ing first-class modules)"
    :author   rossberg
    :date     "2015; 2016 (JFP); 2018 (updated draft)"
    :location (proceedings-location icfp)}

   :ros2016
   {:key      "Ros2016"
    :title    "1ML with Special Effects (F-ing Generativity Polymorphism)"
    :author   rossberg
    :date     "2016"
    :location "In WadlerFest"}

   :cra2017
   {:key      "Cra2017"
    :title    "Modules, Abstraction, and Parametric Polymorphism"
    :author   crary
    :date     "2017"
    :location (proceedings-location popl)}

   :cra2019
   {:key      "Cra2019"
    :title    "Fully Abstract Module Compilation"
    :author   crary
    :date     "2019, to appear"
    :location (proceedings-location popl)}
   ))

(defn -main
  []
  (write "../README.md" (str "# Modules\n\n"
                             "Type-theoretic analysis of ML-style modules.\n\n"
                             "## Incomplete Bibliography of Modules and Type Abstraction\n\n"
                             (html-entries (vals entries)))))
