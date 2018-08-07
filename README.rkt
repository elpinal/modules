#lang scribble/manual

@(require scriblib/autobib)

@title{Modules}

Type-theoretic analysis of ML-style modules.

@(define icalp "ICALP")
@(define icfp "ICFP")
@(define jfp "Journal of Functional Programming")
@(define pldi "PLDI")
@(define popl "POPL")
@(define ppdp "Principles and Practice of Declarative Programming")
@(define tapsoft "TAPSOFT")

@(define ancona "Davide Ancona")
@(define cardelli "Luca Cardelli")
@(define harper "Robert Harper")
@(define leroy "Xavier Leroy")
@(define macqueen "David B. MacQueen")
@(define mitchell "John C. Mitchell")
@(define moggi "Eugenio Moggi")
@(define russo "Claudio V. Russo")
@(define tofte "Mads Tofte")
@(define zucca "Elena Zucca")

@section{Incomplete Bibliography of Modules and Type Abstraction}

@(bibliography
   (bib-entry
    #:key      "Rey1983"
    #:title    "Types, abstraction and parametric polymorphism"
    #:author   "J. C. Reynolds"
    #:date     "1983"
    #:location (proceedings-location "Information Processing" #:pages '(513 523)))

   (bib-entry
    #:key      "BL1984"
    #:title    "A kernel language for abstract data types and modules"
    #:author   (authors "R. Burstall" "B. Lampson")
    #:date     "1984"
    #:location (proceedings-location "Semantics of Data Types" #:pages '(1 50)))

   (bib-entry
    #:key      "MP1985"
    #:title    "Abstract types have existential types"
    #:author   (authors mitchell "G. D. Plotkin")
    #:date     "1985"
    #:location (proceedings-location popl #:pages '(37 51)))

   (bib-entry
    #:key      "CW1985"
    #:title    "On understanding types, data abstraction, and polymorphism"
    #:author   (authors cardelli "P. Wegner")
    #:date     "1985"
    #:location (techrpt-location #:institution "Brown University" #:number "CS-85-14"))

   (bib-entry
    #:key      "Mac1985"
    #:title    "Modules for Standard ML (Revised)"
    #:author   macqueen
    #:date     "1985"
    #:location "Polymorphism Newsletter, II, 2")

   (bib-entry
    #:key      "Mit1986"
    #:title    "Representation independence and data abstraction"
    #:author   mitchell
    #:date     "1986"
    #:location (proceedings-location popl))

   (bib-entry
    #:key      "Mac1986"
    #:title    "Using dependent types to express modular structure"
    #:author   macqueen
    #:date     "1986"
    #:location (proceedings-location popl #:pages '(277 286)))

   (bib-entry
    #:key      "HMT1987"
    #:title    "A type discipline for program modules"
    #:author   (authors harper "R. Milner" tofte)
    #:date     "1987"
    #:location (proceedings-location tapsoft))

   (bib-entry
    #:key      "CM1988"
    #:title    "Persistence and Type Abstraction"
    #:author   (authors cardelli macqueen)
    #:date     "1988. First appeared in 1985"
    #:location "Data types and persistence, Springer-Verlag")

   (bib-entry
    #:key      "Car1988"
    #:title    "Phase distinctions in type theory"
    #:author   cardelli
    #:date     "1988"
    #:location "Manuscript")

   (bib-entry
    #:key      "Mog1989"
    #:title    "A category-theoretic account of program modules"
    #:author   moggi
    #:date     "1989"
    #:location (proceedings-location "Category Theory and Computer Science" #:pages '(101 117)))

   (bib-entry
    #:key      "HMM1990"
    #:title    "Higher-order modules and the phase distinction"
    #:author   (authors harper mitchell moggi)
    #:date     "1990"
    #:location (proceedings-location popl #:pages '(341 354)))

   (bib-entry
    #:key      "CL1990"
    #:title    "Abstract types and the dot notation"
    #:author   (authors cardelli leroy)
    #:date     "1990"
    #:location (proceedings-location "IFIP TC2 working conference on programming concepts and methods" #:pages '(479 504)))

   (bib-entry
    #:key      "BC1990"
    #:title    "Mixin-based inheritance"
    #:author   (authors "G. Bracha" "W. Cook")
    #:date     "1990"
    #:location (proceedings-location "OOPSLA/ECOOP"))

   (bib-entry
    #:key      "Car1991"
    #:title    "Typeful programming"
    #:author   cardelli
    #:date     "1991"
    #:location (proceedings-location "Formal Description of Programming Concepts"))

   (bib-entry
    #:key      "MMM1991"
    #:title    "An extension of Standard ML modules with subtyping and inheritance"
    #:author   (authors mitchell "Sigurd Meldal" "Neel Madhav")
    #:date     "1991"
    #:location (proceedings-location popl #:pages '(270 278)))

   (bib-entry
    #:key      "Bra1992"
    #:title    "The Programming Language JIGSAW: Mixins, Modularity and Multiple Inheritance"
    #:author   "G. Bracha"
    #:date     "1992"
    #:location (dissertation-location #:institution "Department of Comp. Sci., Univ. of Utah" #:degree "PhD"))

   (bib-entry
    #:key      "Tof1992"
    #:title    "Principal signatures for higher-order program modules"
    #:author   tofte
    #:date     "1992"
    #:location (proceedings-location popl #:pages '(189 199)))

   (bib-entry
    #:key      "Apo1993"
    #:title    "Extending record typing to type parametric modules with sharing"
    #:author   "María Virginia Aponte"
    #:date     "1993"
    #:location (proceedings-location popl #:pages '(465 478)))

   (bib-entry
    #:key      "MG1993"
    #:title    "Studying the ML Module System in HOL"
    #:author   (authors "Savi Maharaj" "Elsa Gunter")
    #:date     "1993"
    #:location (journal-location "The Computer Journal" #:volume 36 #:number 5))

   (bib-entry
    #:key      "HL1994"
    #:title    "A type-theoretic approach to higher-order modules with sharing"
    #:author   (authors harper "Mark Lillibridge")
    #:date     "1994"
    #:location (proceedings-location popl #:pages '(123 137)))

   (bib-entry
    #:key      "Ler1994"
    #:title    "Manifest types, modules, and separate compilation"
    #:author   leroy
    #:date     "1994"
    #:location (proceedings-location popl #:pages '(109 122)))

   (bib-entry
    #:key      "MT1994"
    #:title    "A semantics for higher-order functors"
    #:author   (authors macqueen tofte)
    #:date     "1994"
    #:location (proceedings-location "Programming Languages and Systems – ESOP ’94" #:pages '(409 423)))

   (bib-entry
    #:key      "Bis1995"
    #:title    "Higher-order functors with transparent signatures"
    #:author   "S. K. Biswas"
    #:date     "1995"
    #:location (proceedings-location popl #:pages '(154 163)))

   (bib-entry
    #:key      "Ler1995"
    #:title    "Applicative functors and fully transparent higher-order modules"
    #:author   leroy
    #:date     "1995"
    #:location (proceedings-location popl #:pages '(142 153)))

   (bib-entry
    #:key      "DS1996"
    #:title    "Mixin modules"
    #:author   (authors "Dominic Duggan" "Constantinos Sourelis")
    #:date     "1996"
    #:location (proceedings-location icfp #:pages '(262 273)))

   (bib-entry
    #:key      "Ler1996"
    #:title    "A syntactic theory of type generativity and sharing"
    #:author   leroy
    #:date     "1996"
    #:location (journal-location jfp #:number 5 #:volume 6 #:pages '(667 698)))

   (bib-entry
    #:key      "NJ1996"
    #:title    "An exploration of modular programs"
    #:author   (authors "Jan Nicklish" "Simon Peyton Jones")
    #:date     "1996"
    #:location "In The Glasgow Workshop on Functional Programming")

   (bib-entry
    #:key      "Asp1997"
    #:title    "Type Systems for Modular Programs and Specifications"
    #:author   "David R. Aspinall"
    #:date     "1997"
    #:location (dissertation-location #:institution "Edinburgh University, Edinburgh, Scotland" #:degree "PhD"))

   (bib-entry
    #:key      "Cou1997"
    #:title    "An applicative module calculus"
    #:author   "Judicäel Courant"
    #:date     "1997"
    #:location (proceedings-location tapsoft))

   (bib-entry
    #:key      "Rus1998"
    #:title    "Types for modules"
    #:author   russo
    #:date     "1998"
    #:location (dissertation-location #:institution "University of Edinburgh, UK" #:degree "PhD"))

   (bib-entry
    #:key      "DS1998"
    #:title    "Parameterized modules, recursive modules and mixin modules"
    #:author   (authors "Dominic Duggan" "Constantinos Sourelis")
    #:date     "1998"
    #:location "In ACM SIGPLAN Workshop on ML, pages 87–96, Baltimore, MA, USA, September 1998")

   (bib-entry
    #:key      "FF1998a"
    #:title    "Modular object-oriented programming with units and mixins"
    #:author   (authors "R. B. Findler" "M. Flatt")
    #:date     "1998"
    #:location (proceedings-location icfp #:pages '(94 104)))

   (bib-entry
    #:key      "FF1998b"
    #:title    "Units: Cool modules for HOT languages"
    #:author   (authors "M. Flatt" "M. Felleisen")
    #:date     "1998"
    #:location (proceedings-location pldi #:pages '(236 248)))

   (bib-entry
    #:key      "AZ1998a"
    #:title    "A Theory of Mixin Modules: Basic and Derived Operators"
    #:author   (authors ancona zucca)
    #:date     "1998"
    #:location (journal-location "Mathematical Structures in Computer Science" #:volume 8 #:number 4 #:pages '(401 446)))

   (bib-entry
    #:key      "AZ1998b"
    #:title    "An algebra of mixin modules"
    #:author   (authors ancona zucca)
    #:date     "1998"
    #:location "In 12th Workshop on Algebraic Development Techniques - Selected Papers, pp. 92–106")

   (bib-entry
    #:key      "AZ1999"
    #:title    "A primitive calculus for module systems"
    #:author   (authors ancona zucca)
    #:date     "1999"
    #:location (proceedings-location ppdp #:pages '(62 79)))

   (bib-entry
    #:key      "CHP1999"
    #:title    "What is a recursive module?"
    #:author   (authors "Karl Crary" harper "Sidd Puri")
    #:date     "1999"
    #:location (proceedings-location pldi #:pages '(50 63)))

   (bib-entry
    #:key      "Rus1999"
    #:title    "Non-dependent Types for Standard ML Modules"
    #:author   russo
    #:date     "1999"
    #:location (proceedings-location ppdp #:pages '(80 97)))

   (bib-entry
    #:key      "Sha1999"
    #:title    "Transparent modules with fully syntactic signatures"
    #:author   "Zhong Shao"
    #:date     "1999"
    #:location (proceedings-location icfp #:pages '(220 232)))

   (bib-entry
    #:key      "WV2000"
    #:title    "Equational reasoning for linking with first-class primitive modules"
    #:author   (authors "J. B. Wells" "R. Vestergaard")
    #:date     "2000"
    #:location (proceedings-location "Programming Languages and Systems" #:pages '(412 428)))

   (bib-entry
    #:key      "Ler2000"
    #:title    "A modular module system"
    #:author   leroy
    #:date     "2000"
    #:location (journal-location jfp #:volume 10 #:number 3 #:pages '(269 303)))

   (bib-entry
    #:key      "Rus2000"
    #:title    "First-class structures for Standard ML"
    #:author   russo
    #:date     "2000"
    #:location (journal-location "Nordic Journal of Computing" #:volume 7 #:number 4 #:pages '(348 374)))

   (bib-entry
    #:key      "Rus2001"
    #:title    "Recursive structures for Standard ML"
    #:author   russo
    #:date     "2001"
    #:location (proceedings-location icfp #:pages '(50 61)))

   (bib-entry
    #:key      "HL2002"
    #:title    "Mixin modules in a call-by-value setting"
    #:author   (authors "T. Hirschowitz" leroy)
    #:date     "2002"
    #:location (proceedings-location "Programming Languages and Systems" #:pages '(6 20)))

   (bib-entry
    #:key      "AZ2002"
    #:title    "A calculus of module systems"
    #:author   (authors ancona zucca)
    #:date     "2002"
    #:location (journal-location jfp #:volume 12 #:number 2 #:pages '(91 132)))

   (bib-entry
    #:key      "AFMZ2003"
    #:title    "Mixin modules and computational effects"
    #:author   (authors ancona "Sonia Fagorzi" moggi zucca)
    #:date     "2003"
    #:location (proceedings-location icalp #:pages '(224 238)))

   (bib-entry
    #:key      "Ler2003"
    #:title    "proposal for recursive modules in Objective Caml"
    #:author   leroy
    #:date     "2003"
    #:location "Available at http://caml.inria.fr/about/papers.en.html")

   (bib-entry
    #:key      "DCH2003"
    #:title    "A type system for higher-order modules"
    #:author   (authors "Derek Dreyer" "Karl Crary" "Robert Harper")
    #:date     "2003"
    #:location (proceedings-location popl))

   (bib-entry
    #:key      "Dre2005"
    #:title    "Understanding and Evolving the ML Module System"
    #:author   "Derek Dreyer"
    #:date     "2005"
    #:location (dissertation-location #:institution "Carnegie Mellon University, Pittsburgh, Pennsylvania" #:degree "PhD"))

   (bib-entry
    #:key      "NG2006"
    #:title    "Recursive modules for programming"
    #:author   (authors "Keiko Nakata" "Jacques Garrigue")
    #:date     "2006"
    #:location (proceedings-location icfp #:pages '(74 86)))

   (bib-entry
    #:key      "OF2006"
    #:title    "From structures and functors to modules and units"
    #:author   (authors "Scott Owens" "Matthew Flatt")
    #:date     "2006"
    #:location (proceedings-location icfp #:pages '(87 98)))

   (bib-entry
    #:key      "Dre2006"
    #:title    "Practical type theory for recursive modules"
    #:author   "Derek Dreyer"
    #:date     "2006"
    #:location (techrpt-location #:institution "University of Chicago, Department of Computer Science" #:number "TR-2006-07"))

   (bib-entry
    #:key      "Dre2007a"
    #:title    "A Type System for Recursive Modules"
    #:author   "Derek Dreyer"
    #:date     "2007"
    #:location (proceedings-location icfp))

   (bib-entry
    #:key      "Dre2007b"
    #:title    "Recursive Type Generativity"
    #:author   "Derek Dreyer"
    #:date     "2007"
    #:location (journal-location jfp #:volume 17 #:number "4&5" #:pages '(433 471)))

   (bib-entry
    #:key      "DR2008"
    #:title    "Mixin' up the ML module system"
    #:author   (authors "Derek Dreyer" "Andreas Rossberg")
    #:date     "2008"
    #:location (proceedings-location icfp #:pages '(307 320)))

   (bib-entry
    #:key      "Cra2017"
    #:title    "Modules, Abstraction, and Parametric Polymorphism"
    #:author   "Karl Crary"
    #:date     "2017"
    #:location (proceedings-location popl))
    )
