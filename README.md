# Modules

Type-theoretic analysis of ML-style modules.

This repository contains <ul><li>An implementation (<a href="https://github.com/elpinal/modules/blob/master/src/Language/Modules/RRD2014.hs">RRD2014.hs</a>) of F-ing modules (<a href="https://github.com/elpinal/modules#f-ing-modules-1">Rossberg et al. 2014</a>),</li><li>An interpreter (<a href="https://github.com/elpinal/modules/blob/master/src/Language/Modules/Ros2018.hs">Ros2018.hs</a>) of 1ML<sub>ex</sub>(<a href="https://github.com/elpinal/modules#1ml--core-and-modules-united">Rossberg 2018</a>), and</li><li>A bibliography of modules and data abstraction, which is given below.</li></ul>

## See also

- [elpinal/modules-rs](https://github.com/elpinal/modules-rs)
contains another implementation of F-ing modules in Rust (with more bugs fixed than Haskell implementation here).

## 1ML interpreter

`stack install` installs `1mlex`, which is an interpreter of 1ML *without* type inference.

Status: All parts of [Rossberg 2018], except type inference, are implemented. There are few known bugs.

- [elpinal/1ml-vim](https://github.com/elpinal/1ml-vim) is a Vim plugin providing syntax highlighting for 1ML.

## Bibliography of Modules and Data Abstraction

_DISCLAIMER: There is no warranty of accuracy._

<div><h4>Types, abstraction and parametric polymorphism</h4><p>J. C. Reynolds</p><p>Information Processing, pp. 513–523, 1983</p><p>Available at <a href="http://www.cse.chalmers.se/edu/year/2010/course/DAT140_Types/Reynolds_typesabpara.pdf">http://www.cse.chalmers.se/edu/year/2010/course/DAT140_Types/Reynolds_typesabpara.pdf</a></p></div>
<div><h4>A kernel language for abstract data types and modules</h4><p>R. Burstall and B. Lampson</p><p>Semantics of Data Types, pp. 1–50, 1984</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/35-KernelModules.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/35-KernelModules.pdf</a></p></div>
<div><h4>Modules for Standard ML</h4><p>David B. MacQueen</p><p>ACM Conference on LISP and Functional Programming, pp. 198–207, 1984</p><p>Available at <a href="https://www.researchgate.net/profile/David_Macqueen/publication/221252232_Modules_for_Standard_ML/links/0f317532689526bea1000000/Modules-for-Standard-ML.pdf">https://www.researchgate.net/profile/David_Macqueen/publication/221252232_Modules_for_Standard_ML/links/0f317532689526bea1000000/Modules-for-Standard-ML.pdf</a></p></div>
<div><h4>Abstract types have existential types</h4><p>John C. Mitchell and G. D. Plotkin</p><p>POPL, pp. 37–51, 1985</p></div>
<div><h4>On understanding types, data abstraction, and polymorphism</h4><p>Luca Cardelli and P. Wegner</p><p>Brown University, CS-85-14, 1985</p><p>Available at <a href="http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf">http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf</a></p></div>
<div><h4>Representation independence and data abstraction</h4><p>John C. Mitchell</p><p>POPL, pp. 263–276, 1986</p><p>DOI: <a href="https://doi.org/10.1145/512644.512669">10.1145/512644.512669</a></p></div>
<div><h4>Using dependent types to express modular structure</h4><p>David B. MacQueen</p><p>POPL, pp. 277–286, 1986</p><p>Available at <a href="https://www.researchgate.net/profile/David_Macqueen/publication/2385295_Using_Dependent_Types_to_Express_Modular_Structure/links/09e415148bbf0d4470000000.pdf">https://www.researchgate.net/profile/David_Macqueen/publication/2385295_Using_Dependent_Types_to_Express_Modular_Structure/links/09e415148bbf0d4470000000.pdf</a></p></div>
<div><h4>A type discipline for program modules</h4><p>Robert Harper, R. Milner and Mads Tofte</p><p>TAPSOFT, 1987</p><p>Available at <a href="https://link.springer.com/content/pdf/10.1007%2FBFb0014988.pdf">https://link.springer.com/content/pdf/10.1007%2FBFb0014988.pdf</a></p></div>
<div><h4>Persistence and type abstraction</h4><p>Luca Cardelli and David B. MacQueen</p><p>Data types and persistence, Springer-Verlag, 1988. First appeared in 1985</p><p>Available at <a href="http://lucacardelli.name/Papers/Persistence%20and%20Type%20Abstraction.pdf">http://lucacardelli.name/Papers/Persistence%20and%20Type%20Abstraction.pdf</a></p></div>
<div><h4>Abstract types have existential type</h4><p>John C. Mitchell and G. D. Plotkin</p><p>ACM Transactions on Programming Languages and Systems, pp. 470–502, 1988</p><p>Available at <a href="https://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf">https://theory.stanford.edu/~jcm/papers/mitch-plotkin-88.pdf</a></p></div>
<div><h4>Phase distinctions in type theory</h4><p>Luca Cardelli</p><p>Manuscript, 1988</p><p>Available at <a href="http://lucacardelli.name/Papers/PhaseDistinctions.A4.pdf">http://lucacardelli.name/Papers/PhaseDistinctions.A4.pdf</a></p></div>
<div><h4>A category-theoretic account of program modules</h4><p>Eugenio Moggi</p><p>Category Theory and Computer Science, pp. 101–117, 1989</p><p>Available at <a href="https://www.disi.unige.it/person/MoggiE/ftp/mscs91.pdf">https://www.disi.unige.it/person/MoggiE/ftp/mscs91.pdf</a></p></div>
<div><h4>Higher-order modules and the phase distinction</h4><p>Robert Harper, John C. Mitchell and Eugenio Moggi</p><p>POPL, pp. 341–354, 1990</p><p>Available at <a href="https://www.disi.unige.it/person/MoggiE/ftp/popl90.pdf">https://www.disi.unige.it/person/MoggiE/ftp/popl90.pdf</a></p></div>
<div><h4>Abstract types and the dot notation</h4><p>Luca Cardelli and Xavier Leroy</p><p>IFIP TC2 working conference on programming concepts and methods, pp. 479–504, 1990</p><p>Available at <a href="https://xavierleroy.org/publi/abstract-types-dot-notation.pdf">https://xavierleroy.org/publi/abstract-types-dot-notation.pdf</a></p></div>
<div><h4>Mixin-based inheritance</h4><p>Gilad Bracha and William Cook</p><p>OOPSLA/ECOOP, 1990</p><p>Available at <a href="http://www.bracha.org/oopsla90.ps">http://www.bracha.org/oopsla90.ps</a></p></div>
<div><h4>Typeful programming</h4><p>Luca Cardelli</p><p>Formal Description of Programming Concepts, 1991</p><p>Available at <a href="http://lucacardelli.name/Papers/TypefulProg.A4.pdf">http://lucacardelli.name/Papers/TypefulProg.A4.pdf</a></p></div>
<div><h4>An extension of Standard ML modules with subtyping and inheritance</h4><p>John C. Mitchell, Sigurd Meldal and Neel Madhav</p><p>POPL, pp. 270–278, 1991</p><p>Available at <a href="https://www.researchgate.net/publication/2815527_An_extension_of_Standard_ML_modules_with_subtyping_and_inheritance">https://www.researchgate.net/publication/2815527_An_extension_of_Standard_ML_modules_with_subtyping_and_inheritance</a></p></div>
<div><h4>Modularity meets inheritance</h4><p>Gilad Bracha and Gary Lindstrom</p><p>University of Utah, UUCS-91-017, 1991</p><p>Available at <a href="http://www.bracha.org/modularity-meets-inheritance.ps">http://www.bracha.org/modularity-meets-inheritance.ps</a></p></div>
<div><h4>The programming language JIGSAW: Mixins, modularity and multiple inheritance</h4><p>Gilad Bracha</p><p>PhD dissertation, University of Utah, 1992</p><p>Available at <a href="http://www.bracha.org/jigsaw.pdf">http://www.bracha.org/jigsaw.pdf</a></p></div>
<div><h4>Principal signatures for higher-order program modules</h4><p>Mads Tofte</p><p>POPL, pp. 189–199, 1992</p></div>
<div><h4>Extending record typing to type parametric modules with sharing</h4><p>María Virginia Aponte</p><p>POPL, pp. 465–478, 1993</p><p>Available at <a href="https://www.researchgate.net/publication/2416181_Extending_Record_typing_to_type_parametric_modules_with_sharing">https://www.researchgate.net/publication/2416181_Extending_Record_typing_to_type_parametric_modules_with_sharing</a></p></div>
<div><h4>On the type structure of standard ML</h4><p>Robert Harper and John C. Mitchell</p><p>ACM Transactions on Programming Languages and Systems, pp. 211–252, 1993</p><p>Available at <a href="https://crypto.stanford.edu/~jcm/papers/harper-mitch-TOPLAS-93.pdf">https://crypto.stanford.edu/~jcm/papers/harper-mitch-TOPLAS-93.pdf</a></p></div>
<div><h4>Studying the ML module system in HOL</h4><p>Savi Maharaj and Elsa Gunter</p><p><i>The Computer Journal</i>, 36(5), 1993</p><p>Available at <a href="http://www.cs.stir.ac.uk/~sma/publications/HOLML.ps">http://www.cs.stir.ac.uk/~sma/publications/HOLML.ps</a></p></div>
<div><h4>A type-theoretic approach to higher-order modules with sharing</h4><p>Robert Harper and Mark Lillibridge</p><p>POPL, pp. 123–137, 1994</p><p>Available at <a href="https://www.cs.cmu.edu/~rwh/papers/sharing/popl94.pdf">https://www.cs.cmu.edu/~rwh/papers/sharing/popl94.pdf</a></p></div>
<div><h4>Manifest types, modules, and separate compilation</h4><p>Xavier Leroy</p><p>POPL, pp. 109–122, 1994</p><p>Available at <a href="https://xavierleroy.org/publi/manifest-types-popl.pdf">https://xavierleroy.org/publi/manifest-types-popl.pdf</a></p></div>
<div><h4>A semantics for higher-order functors</h4><p>David B. MacQueen and Mads Tofte</p><p>Programming Languages and Systems – ESOP ’94, pp. 409–423, 1994</p><p>Available at <a href="https://rd.springer.com/content/pdf/10.1007%2F3-540-57880-3_27.pdf">https://rd.springer.com/content/pdf/10.1007%2F3-540-57880-3_27.pdf</a></p></div>
<div><h4>Higher-order functors with transparent signatures</h4><p>Sandip K. Biswas</p><p>POPL, pp. 154–163, 1995</p><p>DOI: <a href="https://doi.org/10.1145/199448.199478">10.1145/199448.199478</a></p><p>Available at <a href="http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9123&amp;rep=rep1&amp;type=pdf">http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9123&rep=rep1&type=pdf</a></p></div>
<div><h4>Applicative functors and fully transparent higher-order modules</h4><p>Xavier Leroy</p><p>POPL, pp. 142–153, 1995</p><p>Available at <a href="https://xavierleroy.org/publi/applicative-functors.pdf">https://xavierleroy.org/publi/applicative-functors.pdf</a></p></div>
<div><h4>From Hindley-Milner types to first-class structures</h4><p>Mark P. Jones</p><p>The Haskell workshop, 1995</p><p>Available at <a href="http://web.cecs.pdx.edu/~mpj/pubs/haskwork95.pdf">http://web.cecs.pdx.edu/~mpj/pubs/haskwork95.pdf</a></p></div>
<div><h4>Mixin modules</h4><p>Dominic Duggan and Constantinos Sourelis</p><p>ICFP, pp. 262–273, 1996</p><p>Available at <a href="https://www.cs.tufts.edu/~nr/cs257/archive/dominic-duggan/Mixin%20Modules.pdf">https://www.cs.tufts.edu/~nr/cs257/archive/dominic-duggan/Mixin%20Modules.pdf</a></p></div>
<div><h4>Using parameterized signatures to express modular structure</h4><p>Mark P. Jones</p><p>POPL, pp. 66–78, 1996</p><p>Available at <a href="https://web.cecs.pdx.edu/~mpj/pubs/paramsig.pdf">https://web.cecs.pdx.edu/~mpj/pubs/paramsig.pdf</a></p></div>
<div><h4>A syntactic theory of type generativity and sharing</h4><p>Xavier Leroy</p><p><i>Journal of Functional Programming</i>, 6(5), pp. 667–698, 1996</p><p>Available at <a href="https://xavierleroy.org/publi/syntactic-generativity.pdf">https://xavierleroy.org/publi/syntactic-generativity.pdf</a></p></div>
<div><h4>An exploration of modular programs</h4><p>Jan Nicklish and Simon Peyton Jones</p><p>In The Glasgow Workshop on Functional Programming, 1996</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/Nicklisch-modules.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/1996/01/Nicklisch-modules.pdf</a></p></div>
<div><h4>Type isomorphisms for module signatures</h4><p>María-Virginia Aponte and Roberto Di Cosmo</p><p>Programming Languages Implementation and Logic Programming, pp. 334–346, 1996</p><p>Available at <a href="http://www.dicosmo.org/Articles/1996-AponteDiCosmo-PLILP.pdf">http://www.dicosmo.org/Articles/1996-AponteDiCosmo-PLILP.pdf</a></p></div>
<div><h4>Type systems for modular programs and specifications</h4><p>David R. Aspinall</p><p>PhD dissertation, Edinburgh University, Edinburgh, Scotland, 1997</p><p>Available at <a href="https://www.era.lib.ed.ac.uk/handle/1842/11587">https://www.era.lib.ed.ac.uk/handle/1842/11587</a></p></div>
<div><h4>Program fragments, linking, and modularization</h4><p>Luca Cardelli</p><p>POPL, pp. 266–277, 1997</p><p>Available at <a href="http://lucacardelli.name/Papers/Linking.A4.pdf">http://lucacardelli.name/Papers/Linking.A4.pdf</a></p></div>
<div><h4>An applicative module calculus</h4><p>Judicaël Courant</p><p>TAPSOFT, 1997</p><p>DOI: <a href="https://doi.org/10.1007/BFb0030630">10.1007/BFb0030630</a></p><p>Available at <a href="https://link.springer.com/content/pdf/10.1007%2FBFb0030630.pdf">https://link.springer.com/content/pdf/10.1007%2FBFb0030630.pdf</a></p></div>
<div><h4>Translucent sums: A foundation for higher-order module systems</h4><p>Mark Lillibridge</p><p>PhD dissertation, Carnegie Mellon University, 1997</p><p>Available at <a href="https://www.cs.cmu.edu/Groups/fox/papers/mdl-thesis.ps">https://www.cs.cmu.edu/Groups/fox/papers/mdl-thesis.ps</a></p></div>
<div><h4>An interpretation of Standard ML in type theory</h4><p>Robert Harper and Christopher Stone</p><p>Carnegie Mellon University, CMU-CS-97-147, 1997</p><p>Available at <a href="https://www.cs.cmu.edu/Groups/fox/papers/sml96-v3.ps">https://www.cs.cmu.edu/Groups/fox/papers/sml96-v3.ps</a></p></div>
<div><h4>Types for modules</h4><p>Claudio V. Russo</p><p>PhD dissertation, University of Edinburgh, UK, 1998</p><p>Available at <a href="http://www.dcs.ed.ac.uk/home/cvr/ECS-LFCS-98-389.pdf">http://www.dcs.ed.ac.uk/home/cvr/ECS-LFCS-98-389.pdf</a></p></div>
<div><h4>Typed cross-module compilation</h4><p>Zhong Shao</p><p>ICFP, pp. 141–152, 1998</p><p>Available at <a href="http://flint.cs.yale.edu/flint/publications/tcc.pdf">http://flint.cs.yale.edu/flint/publications/tcc.pdf</a><br />Technical Report: <a href="http://flint.cs.yale.edu/flint/publications/tcc-tr.pdf">http://flint.cs.yale.edu/flint/publications/tcc-tr.pdf</a></p></div>
<div><h4>Parameterized modules, recursive modules and mixin modules</h4><p>Dominic Duggan and Constantinos Sourelis</p><p>In ACM SIGPLAN Workshop on ML, pages 87–96, Baltimore, MA, USA, September 1998, 1998</p></div>
<div><h4>Modular object-oriented programming with units and mixins</h4><p>Robert Bruce Findler and Matthew Flatt</p><p>ICFP, pp. 94–104, 1998</p><p>Available at <a href="https://www2.ccs.neu.edu/racket/pubs/icfp98-ff.pdf">https://www2.ccs.neu.edu/racket/pubs/icfp98-ff.pdf</a></p></div>
<div><h4>Units: Cool modules for HOT languages</h4><p>Matthew Flatt and Matthias Felleisen</p><p>PLDI, pp. 236–248, 1998</p><p>Available at <a href="http://www.ccs.neu.edu/scheme/pubs/pldi98-ff.ps.gz">http://www.ccs.neu.edu/scheme/pubs/pldi98-ff.ps.gz</a></p></div>
<div><h4>A theory of mixin modules: Basic and derived operators</h4><p>Davide Ancona and Elena Zucca</p><p><i>Mathematical Structures in Computer Science</i>, 8(4), pp. 401–446, 1998</p></div>
<div><h4>An algebra of mixin modules</h4><p>Davide Ancona and Elena Zucca</p><p>In 12th Workshop on Algebraic Development Techniques - Selected Papers, pp. 92–106, 1998</p></div>
<div><h4>A primitive calculus for module systems</h4><p>Davide Ancona and Elena Zucca</p><p>Principles and Practice of Declarative Programming, pp. 62–79, 1999</p></div>
<div><h4>What is a recursive module?</h4><p>Karl Crary, Robert Harper and Sidd Puri</p><p>PLDI, pp. 50–63, 1999</p><p>Available at <a href="http://www.cs.cmu.edu/~crary/papers/1999/recmod/recmod.ps.gz">http://www.cs.cmu.edu/~crary/papers/1999/recmod/recmod.ps.gz</a></p></div>
<div><h4>Non-dependent types for Standard ML modules</h4><p>Claudio V. Russo</p><p>Principles and Practice of Declarative Programming, pp. 80–97, 1999</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/1999/09/Non-Dependent-Types-for-Standard-ML-Modules.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/1999/09/Non-Dependent-Types-for-Standard-ML-Modules.pdf</a></p></div>
<div><h4>Transparent modules with fully syntactic signatures</h4><p>Zhong Shao</p><p>ICFP, pp. 220–232, 1999</p><p>Available at <a href="http://flint.cs.yale.edu/flint/publications/fullsig.pdf">http://flint.cs.yale.edu/flint/publications/fullsig.pdf</a><br />Technical Report: <a href="http://flint.cs.yale.edu/flint/publications/fullsig-tr.pdf">http://flint.cs.yale.edu/flint/publications/fullsig-tr.pdf</a></p></div>
<div><h4>Program modules, separate compilation, and intermodule optimisation</h4><p>Martin Elsman</p><p>PhD dissertation, Department of Computer Science, University of Copenhagen, 1999</p><p>Available at <a href="https://elsman.com/pdf/phd.pdf">https://elsman.com/pdf/phd.pdf</a></p></div>
<div><h4>Static interpretation of modules</h4><p>Martin Elsman</p><p>ICFP, 1999</p><p>Available at <a href="https://elsman.com/pdf/icfp99.pdf">https://elsman.com/pdf/icfp99.pdf</a></p></div>
<div><h4>Equational reasoning for linking with first-class primitive modules</h4><p>J. B. Wells and R. Vestergaard</p><p>Programming Languages and Systems, pp. 412–428, 2000</p><p>Available at <a href="http://www.macs.hw.ac.uk/~jbw/papers/Wells+Vestergaard:Equational-Reasoning-for-Linking-with-First-Class-Primitive-Modules:ESOP-2000.ps.gz">http://www.macs.hw.ac.uk/~jbw/papers/Wells+Vestergaard:Equational-Reasoning-for-Linking-with-First-Class-Primitive-Modules:ESOP-2000.ps.gz</a></p></div>
<div><h4>A type-theoretic interpretation of Standard ML</h4><p>Robert Harper and Christopher Stone</p><p><i>Proof, language, and interaction: Essays in honor of robin milner</i>, MIT Press, 2000</p><p>Available at <a href="https://www.cs.cmu.edu/~rwh/papers/ttisml/ttisml.pdf">https://www.cs.cmu.edu/~rwh/papers/ttisml/ttisml.pdf</a></p></div>
<div><h4>A modular module system</h4><p>Xavier Leroy</p><p><i>Journal of Functional Programming</i>, 10(3), pp. 269–303, 2000</p><p>Available at <a href="https://xavierleroy.org/publi/modular-modules-jfp.pdf">https://xavierleroy.org/publi/modular-modules-jfp.pdf</a></p></div>
<div><h4>First-class structures for Standard ML</h4><p>Claudio V. Russo</p><p>European Symposium on Programming, pp. 336–350, 2000</p><p>Available at <a href="https://link.springer.com/content/pdf/10.1007%2F3-540-46425-5_22.pdf">https://link.springer.com/content/pdf/10.1007%2F3-540-46425-5_22.pdf</a></p></div>
<div><h4>Recursive structures for Standard ML</h4><p>Claudio V. Russo</p><p>ICFP, pp. 50–61, 2001</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/2001/09/Recursive-Structures-for-Standard-ML.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/2001/09/Recursive-Structures-for-Standard-ML.pdf</a></p></div>
<div><h4>Towards a practical type theory for recursive modules</h4><p>Derek Dreyer, Robert Harper and Karl Crary</p><p>Carnegie Mellon University School of Computer Science, CMU-CS-01-112, 2001</p><p>Available at <a href="https://www.cs.cmu.edu/~rwh/papers/ttrm/rmtr.pdf">https://www.cs.cmu.edu/~rwh/papers/ttrm/rmtr.pdf</a></p></div>
<div><h4>Modules, abstract types, and distributed versioning</h4><p>Peter Sewell</p><p>POPL, pp. 236–247, 2001</p><p>Available at <a href="https://www.cl.cam.ac.uk/~pes20/versions-popl.pdf">https://www.cl.cam.ac.uk/~pes20/versions-popl.pdf</a></p></div>
<div><h4>Mixin modules in a call-by-value setting</h4><p>Tom Hirschowitz and Xavier Leroy</p><p>European Symposium on Programming, pp. 6–20, 2002</p><p>Available at <a href="https://xavierleroy.org/publi/mixins-cbv-esop2002.pdf">https://xavierleroy.org/publi/mixins-cbv-esop2002.pdf</a></p></div>
<div><h4>A calculus of module systems</h4><p>Davide Ancona and Elena Zucca</p><p><i>Journal of Functional Programming</i>, 12(2), pp. 91–132, 2002</p></div>
<div><h4>First-class modules for Haskell</h4><p>Mark Shields and Simon Peyton Jones</p><p>Foundations of Object-Oriented Languages, 2002</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/first_class_modules.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/first_class_modules.pdf</a></p></div>
<div><h4>A formal specification for the Haskell 98 module system</h4><p>Iavor S. Diatchki, Mark P. Jones and Thomas Hallgren</p><p>ACM SIGPLAN 2002 Haskell Workshop, 2002</p><p>Available at <a href="http://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf">http://web.cecs.pdx.edu/~mpj/pubs/hsmods.pdf</a></p></div>
<div><h4>Mixin modules and computational effects</h4><p>Davide Ancona, Sonia Fagorzi, Eugenio Moggi and Elena Zucca</p><p>ICALP, pp. 224–238, 2003</p><p>Available at <a href="https://www.disi.unige.it/person/MoggiE/ftp/icalp03.pdf">https://www.disi.unige.it/person/MoggiE/ftp/icalp03.pdf</a></p></div>
<div><h4>A proposal for recursive modules in Objective Caml</h4><p>Xavier Leroy</p><p>Unpublished, 2003</p><p>Available at <a href="http://caml.inria.fr/pub/papers/xleroy-recursive_modules-03.pdf">http://caml.inria.fr/pub/papers/xleroy-recursive_modules-03.pdf</a></p></div>
<div><h4>A type system for higher-order modules</h4><p>Derek Dreyer, Karl Crary and Robert Harper</p><p>POPL, 2003</p><p>Available at <a href="http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms.pdf">http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms.pdf</a><br />Technical Report: <a href="http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms-tr.pdf">http://www.cs.cmu.edu/~crary/papers/2003/thoms/thoms-tr.pdf</a></p></div>
<div><h4>Types for modules</h4><p>Claudio V. Russo</p><p><i>Electronic Notes in Theoretical Computer Science</i>, 60, 2003</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/1998/03/Types-for-Modules.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/1998/03/Types-for-Modules.pdf</a></p></div>
<div><h4>A type system for well-founded recursion</h4><p>Derek Dreyer</p><p>POPL, pp. 293–305, 2004</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/recursion/popl.pdf">https://people.mpi-sws.org/~dreyer/papers/recursion/popl.pdf</a><br />Technical Report (with Robert Harper and Karl Crary, 2003): <a href="https://people.mpi-sws.org/~dreyer/papers/recursion/tr/main.pdf">https://people.mpi-sws.org/~dreyer/papers/recursion/tr/main.pdf</a></p></div>
<div><h4>Understanding and evolving the ML module system</h4><p>Derek Dreyer</p><p>PhD dissertation, Carnegie Mellon University, Pittsburgh, Pennsylvania, 2005</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/thesis/main.pdf">https://people.mpi-sws.org/~dreyer/thesis/main.pdf</a></p></div>
<div><h4>Recursive type generativity</h4><p>Derek Dreyer</p><p>ICFP, pp. 41–53, 2005</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/dps/main.pdf">https://people.mpi-sws.org/~dreyer/papers/dps/main.pdf</a></p></div>
<div><h4>Type generativity in higher-order module systems</h4><p>Paul Govereau</p><p>Harvard Computer Science Group, TR-05-05, 2005</p><p>Available at <a href="https://dash.harvard.edu/bitstream/handle/1/23853816/tr-05-05.pdf">https://dash.harvard.edu/bitstream/handle/1/23853816/tr-05-05.pdf</a></p></div>
<div><h4>An expressive language of signatures</h4><p>Norman Ramsey, Kathleen Fisher and Paul Govereau</p><p>ICFP, pp. 27–40, 2005</p><p>Available at <a href="https://www.cs.tufts.edu/~nr/pubs/els.pdf">https://www.cs.tufts.edu/~nr/pubs/els.pdf</a></p></div>
<div><h4>Recursive object-oriented modules</h4><p>Keiko Nakata, Akira Ito and Jacques Garrigue</p><p>Foundations of Object-Oriented Languages, 2005</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/fool_2005.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/fool_2005.pdf</a></p><p>Extended version: <a href="http://www.kurims.kyoto-u.ac.jp/~keiko/papers/room_ext.pdf">http://www.kurims.kyoto-u.ac.jp/~keiko/papers/room_ext.pdf</a></p></div>
<div><h4>Recursion for structured modules</h4><p>Keiko Nakata</p><p>JSSST Workshop on Programming and Programming Languages, 2005</p><p>Available at <a href="http://www.kurims.kyoto-u.ac.jp/~keiko/papers/ppl05.pdf">http://www.kurims.kyoto-u.ac.jp/~keiko/papers/ppl05.pdf</a></p></div>
<div><h4>Type inference, principal typings, and let-polymorphism for first-class mixin modules</h4><p>Henning Makholm and J. B. Wells</p><p>ICFP, 2005</p><p>Available at <a href="http://henning.makholm.net/papers/icfp2005.pdf">http://henning.makholm.net/papers/icfp2005.pdf</a></p></div>
<div><h4>Mixin modules in a call-by-value setting</h4><p>Tom Hirschowitz and Xavier Leroy</p><p>ACM Transactions on Programming Languages and Systems, pp. 857–881, 2005</p><p>DOI: <a href="https://doi.org/10.1145/1086642.1086644">10.1145/1086642.1086644</a></p><p>Available at <a href="https://xavierleroy.org/publi/mixins-cbv-toplas.pdf">https://xavierleroy.org/publi/mixins-cbv-toplas.pdf</a></p></div>
<div><h4>Path resolution for recursive modules</h4><p>Keiko Nakata</p><p>Kyoto University, RIMS-1545, 2006</p><p>Available at <a href="http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1545.pdf">http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1545.pdf</a></p></div>
<div><h4>Recursive modules for programming</h4><p>Keiko Nakata and Jacques Garrigue</p><p>ICFP, pp. 74–86, 2006</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/nakata-icfp2006.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/nakata-icfp2006.pdf</a><br />Technical Report: <a href="http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1546.pdf">http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1546.pdf</a></p></div>
<div><h4>From structures and functors to modules and units</h4><p>Scott Owens and Matthew Flatt</p><p>ICFP, pp. 87–98, 2006</p><p>Available at <a href="http://www.cs.utah.edu/plt/publications/icfp06-of.pdf">http://www.cs.utah.edu/plt/publications/icfp06-of.pdf</a></p></div>
<div><h4>Practical type theory for recursive modules</h4><p>Derek Dreyer</p><p>University of Chicago, Department of Computer Science, TR-2006-07, 2006</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/bimod/main.pdf">https://people.mpi-sws.org/~dreyer/papers/bimod/main.pdf</a></p></div>
<div><h4>Higher-order modules in System Fω and Haskell</h4><p>Chung-chieh Shan</p><p>Manuscript, May 15, 2006</p><p>Available at <a href="http://homes.soic.indiana.edu/ccshan/xlate/xlate.pdf">http://homes.soic.indiana.edu/ccshan/xlate/xlate.pdf</a></p></div>
<div><h4>A type system for recursive modules</h4><p>Derek Dreyer</p><p>ICFP, 2007</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/recmod/main-short.pdf">https://people.mpi-sws.org/~dreyer/papers/recmod/main-short.pdf</a><br />Technical Report: <a href="https://people.mpi-sws.org/~dreyer/papers/recmod/main-long.pdf">https://people.mpi-sws.org/~dreyer/papers/recmod/main-long.pdf</a></p></div>
<div><h4>Recursive type generativity</h4><p>Derek Dreyer</p><p><i>Journal of Functional Programming</i>, 17(4&5), pp. 433–471, 2007</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/dps/jfp.pdf">https://people.mpi-sws.org/~dreyer/papers/dps/jfp.pdf</a></p></div>
<div><h4>Principal type schemes for modular programs</h4><p>Derek Dreyer and Matthias Blume</p><p>European Symposium on Programming, 2007</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/infmod/main-short.pdf">https://people.mpi-sws.org/~dreyer/papers/infmod/main-short.pdf</a><br />Technical Report: <a href="https://people.mpi-sws.org/~dreyer/papers/infmod/main-long.pdf">https://people.mpi-sws.org/~dreyer/papers/infmod/main-long.pdf</a></p></div>
<div><h4>A module system with applicative functors and recursive path references</h4><p>Keiko Nakata</p><p>PhD dissertation, Kyoto University, 2007</p><p>Available at <a href="http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1583.pdf">http://www.kurims.kyoto-u.ac.jp/preprint/file/RIMS1583.pdf</a></p></div>
<div><h4>Path resolution for recursive nested modules is undecidable</h4><p>Keiko Nakata and Jacques Garrigue</p><p>9th International Workshop on Termination, 2007</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/wst2007.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/wst2007.pdf</a></p></div>
<div><h4>Mixin’ up the ML module system</h4><p>Derek Dreyer and Andreas Rossberg</p><p>ICFP, pp. 307–320, 2008</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08.pdf">https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08.pdf</a><br />Technical Report: <a href="https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08-extended.pdf">https://people.mpi-sws.org/~rossberg/mixml/mixml-icfp08-extended.pdf</a></p></div>
<div><h4>Towards a simpler account of modules and generativity: Abstract types have open existential types</h4><p>Benoît Montagu and Didier Rémy</p><p>January 2008</p><p>Available at <a href="http://gallium.inria.fr/~remy/modules/fzip.pdf">http://gallium.inria.fr/~remy/modules/fzip.pdf</a></p></div>
<div><h4>A logical account of type generativity: Abstract types have open existential types</h4><p>Benoît Montagu and Didier Rémy</p><p>April 14, 2008</p><p>Available at <a href="http://gallium.inria.fr/~remy/modules/oat.pdf">http://gallium.inria.fr/~remy/modules/oat.pdf</a></p><p>Slides: <a href="http://gallium.inria.fr/~remy/modules/fzip@msr2008.pdf">http://gallium.inria.fr/~remy/modules/fzip@msr2008.pdf</a></p></div>
<div><h4>Lazy mixins and disciplined effects</h4><p>Keiko Nakata</p><p>2008</p><p>Available at <a href="http://cs.ioc.ee/~keiko/papers/Lyre08.pdf">http://cs.ioc.ee/~keiko/papers/Lyre08.pdf</a></p></div>
<div><h4>Lazy modules: A lazy evaluation strategy for more recursive initialization patterns</h4><p>Keiko Nakata</p><p>2008</p><p>Available at <a href="http://cs.ioc.ee/~keiko/papers/OsanLong.pdf">http://cs.ioc.ee/~keiko/papers/OsanLong.pdf</a></p></div>
<div><h4>Modeling abstract types in modules with open existential types</h4><p>Benoît Montagu and Didier Rémy</p><p>POPL, pp. 354–365, 2009</p><p>Available at <a href="http://gallium.inria.fr/~remy/modules/Montagu-Remy@popl09:fzip.pdf">http://gallium.inria.fr/~remy/modules/Montagu-Remy@popl09:fzip.pdf</a></p></div>
<div><h4>Engineering higher-order modules in SML/NJ</h4><p>George Kuan and David B. MacQueen</p><p>Implementation and Application of Functional Languages, pp. 218–235, 2009</p><p>Available at <a href="https://www.researchgate.net/profile/David_Macqueen/publication/226219412_Engineering_Higher-Order_Modules_in_SMLNJ/links/0912f50a29752482c0000000.pdf">https://www.researchgate.net/profile/David_Macqueen/publication/226219412_Engineering_Higher-Order_Modules_in_SMLNJ/links/0912f50a29752482c0000000.pdf</a></p></div>
<div><h4>A true higher-order module system</h4><p>George Kuan</p><p>PhD dissertation, University of Chicago, 2010</p><p>Available at <a href="http://smlnj-gforge.cs.uchicago.edu/scm/viewvc.php/*checkout*/papers/hofsem/dissertation/kuan-dissertation.pdf?root=smlnj">http://smlnj-gforge.cs.uchicago.edu/scm/viewvc.php/*checkout*/papers/hofsem/dissertation/kuan-dissertation.pdf?root=smlnj</a></p></div>
<div><h4>Programming with first-class modules in a core language with subtyping, singleton kinds and open existential types</h4><p>Benoît Montagu</p><p>PhD dissertation, Ecole Polytechnique X, 2010</p><p>Available at <a href="https://pastel.archives-ouvertes.fr/tel-00550331/document">https://pastel.archives-ouvertes.fr/tel-00550331/document</a></p></div>
<div><h4>A flattening strategy for SML module compilation and its implementation</h4><p>Liu Bochao and Atsushi Ohori</p><p><i>Information and Media Technologies</i>, 5(1), pp. 58–76, 2010</p><p>Available at <a href="https://www.jstage.jst.go.jp/article/imt/5/1/5_1_58/_pdf/-char/en">https://www.jstage.jst.go.jp/article/imt/5/1/5_1_58/_pdf/-char/en</a></p></div>
<div><h4>F-ing modules</h4><p>Andreas Rossberg, Claudio V. Russo and Derek Dreyer</p><p>Types in Language Design and Implementation, 2010</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/f-ing/f-ing.pdf">https://people.mpi-sws.org/~rossberg/f-ing/f-ing.pdf</a></p></div>
<div><h4>First-class modules and composable signatures in Objective Caml 3.12</h4><p>Alain Frisch and Jacques Garrigue</p><p>ML Workshop, 2010</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010.pdf</a></p><p>Slides: <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010-show.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/ml2010-show.pdf</a></p></div>
<div><h4>A syntactic type system for recursive modules</h4><p>Hyonseung Im, Keiko Nakata, Jacques Garrigue and Sungwoo Park</p><p>Object-oriented Programming, Systems, Languages, and Applications, 2011</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/oopsla2011.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/oopsla2011.pdf</a></p></div>
<div><h4>Path resolution for nested recursive modules</h4><p>Jacques Garrigue and Keiko Nakata</p><p><i>Higher-Order and Symbolic Computation</i>, 24(3), pp. 207–237, 2012</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/path-resolution-1205.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/path-resolution-1205.pdf</a></p></div>
<div><h4>Mixin’ up the ML module system</h4><p>Andreas Rossberg and Derek Dreyer</p><p>ACM Transactions on Programming Languages and Systems, 2013</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/mixml/mixml-toplas.pdf">https://people.mpi-sws.org/~rossberg/mixml/mixml-toplas.pdf</a></p></div>
<div><h4>Contractive signatures with recursive types, type parameters, and abstract types</h4><p>Hyeonseung Im, Keiko Nakata and Sungwoo Park</p><p>ICALP, 2013</p><p>Available at <a href="http://pl.postech.ac.kr/~gla/paper/icalp2013.pdf">http://pl.postech.ac.kr/~gla/paper/icalp2013.pdf</a></p></div>
<div><h4>F-ing modules</h4><p>Andreas Rossberg, Claudio V. Russo and Derek Dreyer</p><p><i>Journal of Functional Programming</i>, 24(5), 2014</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/f-ing/f-ing-jfp.pdf">https://people.mpi-sws.org/~rossberg/f-ing/f-ing-jfp.pdf</a></p></div>
<div><h4>Backpack: Retrofitting Haskell with interfaces</h4><p>Scott Kilpatrick, Derek Dreyer, Simon Peyton Jones and Simon Marlow</p><p>POPL, pp. 19–31, 2014</p><p>Available at <a href="https://people.mpi-sws.org/~dreyer/papers/backpack/paper.pdf">https://people.mpi-sws.org/~dreyer/papers/backpack/paper.pdf</a></p><p>Slides: <a href="https://plv.mpi-sws.org/backpack/backpack-popl.pdf">https://plv.mpi-sws.org/backpack/backpack-popl.pdf</a></p><p>Appendix: <a href="https://people.mpi-sws.org/~dreyer/papers/backpack/appendix.pdf">https://people.mpi-sws.org/~dreyer/papers/backpack/appendix.pdf</a></p></div>
<div><h4>Type-level module aliases: independent and equal</h4><p>Jacques Garrigue and Leo P. White</p><p>ML Family Workshop, 2014</p><p>Available at <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias.pdf</a></p><p>Slides: <a href="http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias-show.pdf">http://www.math.nagoya-u.ac.jp/~garrigue/papers/modalias-show.pdf</a></p></div>
<div><h4>1ML — Core and modules united (F-ing first-class modules)</h4><p>Andreas Rossberg</p><p>ICFP, 2015</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf">https://people.mpi-sws.org/~rossberg/1ml/1ml.pdf</a><br />Technical Report: <a href="https://people.mpi-sws.org/~rossberg/1ml/1ml-extended.pdf">https://people.mpi-sws.org/~rossberg/1ml/1ml-extended.pdf</a></p></div>
<div><h4>1ML with special effects (F-ing generativity polymorphism)</h4><p>Andreas Rossberg</p><p>In WadlerFest, 2016</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/1ml/1ml-effects.pdf">https://people.mpi-sws.org/~rossberg/1ml/1ml-effects.pdf</a></p></div>
<div><h4>Backpack to work: Towards practical mixin linking for Haskell</h4><p>Simon Peyton Jones, Edward Yang, Scott Kilpatrick and Derek Dreyer</p><p>In submission, March 2016</p><p>Available at <a href="https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf">https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/backpack-2016.pdf</a></p></div>
<div><h4>Backpack: Towards practical mix-in linking in Haskell</h4><p>Edward Z. Yang</p><p>PhD dissertation, Stanford University, June 2017</p><p>Available at <a href="https://github.com/ezyang/thesis/releases">https://github.com/ezyang/thesis/releases</a></p></div>
<div><h4>Extending OCaml's open (extended abstract)</h4><p>Runhang Li and Jeremy Yallop</p><p>The OCaml Users and Developers Workshop, 2017</p><p>Available at <a href="https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open.pdf">https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open.pdf</a></p></div>
<div><h4>Modules, abstraction, and parametric polymorphism</h4><p>Karl Crary</p><p>POPL, 2017</p><p>Available at <a href="http://www.cs.cmu.edu/~crary/papers/2017/mapp.pdf">http://www.cs.cmu.edu/~crary/papers/2017/mapp.pdf</a></p></div>
<div><h4>Static interpretation of higher-order modules in Futhark: Functional GPU programming in the large</h4><p>Martin Elsman, Troels Henriksen, Danil Annenkov and Cosmin E. Oancea</p><p>ICFP, pp. 1–30, 2018</p><p>Available at <a href="https://futhark-lang.org/publications/icfp18.pdf">https://futhark-lang.org/publications/icfp18.pdf</a></p></div>
<div><h4>1ML — Core and modules united</h4><p>Andreas Rossberg</p><p><i>Journal of Functional Programming</i>, 28, e22, 2018</p><p>Available at <a href="https://people.mpi-sws.org/~rossberg/papers/Rossberg%20-%201ML%20--%20Core%20and%20modules%20united%20[JFP].pdf">https://people.mpi-sws.org/~rossberg/papers/Rossberg%20-%201ML%20--%20Core%20and%20modules%20united%20[JFP].pdf</a></p></div>
<div><h4>Fully abstract module compilation</h4><p>Karl Crary</p><p><i>POPL</i>, 3(POPL), pp. 10:1–10:29, 2019</p><p>Available at <a href="https://dl.acm.org/ft_gateway.cfm?id=3290323">https://dl.acm.org/ft_gateway.cfm?id=3290323</a></p><p>Slides: <a href="https://popl19.sigplan.org/event/popl-2019-research-papers-fully-abstract-module-compilation">https://popl19.sigplan.org/event/popl-2019-research-papers-fully-abstract-module-compilation</a></p></div>
<div><h4>Characterising renaming within OCaml’s module system: Theory and implementation</h4><p>Reuben N. S. Rowe, Hugo Férée, Simon J. Thompson and Scott Owens</p><p>PLDI, pp. 950–965, 2019</p><p>Available at <a href="https://www.cs.kent.ac.uk/people/staff/rnsr/docs/renaming-pldi2019.pdf">https://www.cs.kent.ac.uk/people/staff/rnsr/docs/renaming-pldi2019.pdf</a></p></div>
<div><h4>Extending OCaml's open</h4><p>Runhang Li and Jeremy Yallop</p><p>ML & OCaml 2017 post-proceedings, to appear</p><p>Available at <a href="https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open-draft.pdf">https://www.cl.cam.ac.uk/~jdy22/papers/extending-ocamls-open-draft.pdf</a></p></div>