\chapter{Literature Review}
\label{chap:LiteratureReview}

\section{The Foundations of Typed Functional Languages}
\label{sec:LitReviewFoundations}

At the heart of modern, statically-typed functional languages like Haskell and OCaml lies a sophisticated type system \cite{haskell-type-systems-research, ocaml-papers}, a collection of formal rules assigning a property, or \textbf{type}, to every construct in a program \cite{pierce-types-2002}. A well-typed program is guaranteed to be free from a large class of runtime errors, such as applying an arithmetic operation to a string, a crucial property known as \textbf{type safety}. The theoretical basis for these languages is the \textbf{Lambda Calculus}, a formal system for expressing computation through function abstraction and application.

The most foundational typed variant is the \textbf{Simply-Typed Lambda Calculus (STLC)}. It ensures type safety by requiring every function parameter to be explicitly annotated with a type, and it verifies that functions are only ever applied to arguments of the matching type \cite{Pierce-SF2}. For example, the boolean `not` function, \texttt{\textbackslash(x : Bool). if x then false else true}, is correctly assigned the type \texttt{Bool -> Bool}.

However, STLC's safety comes at the cost of expressivity. The system is fundamentally \textbf{monomorphic}: each function can only have one, specific type. An identity function, for instance, can be written for booleans (\texttt{\textbackslash(x : Bool). x}) or for integers (\texttt{\textbackslash(x : Int). x}), but a single, universal identity function that works for \textit{all} types is impossible to express. This limitation makes pure STLC impractical for building reusable, large-scale software.

\section{System F: The Advent of "forall"}
\label{chap:LiteratureReview:sec:PolymorphismAndSystemF}

To overcome the rigidity of monomorphism, modern languages rely on \textbf{polymorphism}, which allows a single piece of code to operate on values of multiple types. The foundational system that formally introduced this capability is Jean-Yves Girard's \textbf{System F} \cite{girard-system-f}. System F extends the lambda calculus with type abstraction and type application, enabling the creation of truly generic functions through what is known as \textbf{parametric polymorphism}.

In System F, a universal identity function can be written with the polymorphic type \texttt{forall a. a -> a}. The \texttt{forall} quantifier introduces a \textbf{type variable} \texttt{a}, which stands for "any type" and can be instantiated with a concrete type like \texttt{Int} or \texttt{Bool} at the function's call site.

A crucial distinction within such powerful systems is the one between predicative and impredicative polymorphism. In a \textbf{predicative} system, a quantified type variable (like \texttt{a}) can only be instantiated with a \textbf{monotype}---a simple type that does not itself contain any \texttt{forall} quantifiers. In contrast, a more powerful \textbf{impredicative} system allows a type variable to be instantiated with another \textbf{polytype}. While this "first-class" polymorphism is highly expressive, full type inference for impredicative systems is undecidable \cite{wells-typability-1999, serrano-quick-2020}. This thesis, following the practical approach pioneered for GHC, operates within the simpler and more predictable predicative fragment \cite{jones-practical-2007}.

\section{Practical Polymorphism: Hindley-Milner and the Rank-N Gap}
\label{sec:LitReviewHM}

While System F provides the theoretical ideal for polymorphism, its power makes it impossible to create an algorithm that can always infer a program's types without explicit annotations from the programmer. To make type inference practical and automatic, languages like Haskell and ML adopted a decidable and well-behaved subset of System F known as the \textbf{Damas-Hindley-Milner (HM)} type system \cite{damas-milner}.

The HM system's practicality stems from a key restriction on polymorphism: it only supports \textbf{Rank-1} types. This means that \texttt{forall} quantifiers are only permitted at the very outermost level of a type definition. For example, \texttt{forall a. [a] -> Int} is a valid Rank-1 type, but a type where the quantifier is nested, such as \texttt{(forall a. a -> a) -> Int}, is known as a **higher-rank type** (specifically, a Rank-2 type) and is forbidden in a standard HM system.

This Rank-1 restriction ensures that type inference is decidable and can be implemented efficiently by algorithms like Algorithm W \cite{jones-practical-2007}. However, it introduces a significant limitation: it becomes impossible to pass a polymorphic function as an argument to another function, as doing so would require a higher-rank type. While acceptable for many programs, this constraint prevents the expression of powerful programming patterns common in functional programming, such as generic traversals or operations on monadic structures.

This limitation creates a well-defined research gap: the need for a practical system that extends beyond Rank-1 polymorphism without sacrificing decidability. This thesis follows the path laid out by Peyton Jones et al. in \cite{jones-practical-2007}, which provides a practical foundation for extending the Hindley-Milner system to support arbitrary-rank types within a predicative framework.


\label{chap:LiteratureReview:sec:TypeInferenceAlgorithm}

The practical, \textbf{bidirectional} system for arbitrary-rank types described in \cite{jones-practical-2007} provides the foundation for this thesis. However, the field of type inference is dynamic, and research has continued to evolve. This section surveys key developments since that foundational work, with a primary focus on bidirectional algorithms, which have been praised for their potential to improve error locality and produce more intuitive error messages \cite{dunfield-bidirectional-2020}. The continued interest in this area is evidenced by numerous publicly available implementations, which serve as valuable resources for researchers and students alike \cite{github-goldenberg-artem-goldenbergbidirectionalsystem-2025, github-choi-kwanghoonbidi-2025, github-chen-cu1ch3ntype-inference-zoo-2025}.

\subsection{The Rise of Formal Bidirectional Systems}

A significant thread of modern research has focused on placing bidirectional typing on a more rigorous formal foundation than the operational, constraint-based approach of GHC. \citeauthor{dunfield-complete-2013} \cite{dunfield-complete-2013} presented a declarative, bidirectional algorithm for higher-rank predicative polymorphism that achieves results similar to those in \cite{jones-practical-2007}, including soundness and completeness with respect to System F \cite{selinger-lecture-2013}. The authors argue that their formulation—using ordered contexts and existential variables—provides a cleaner type-theoretic basis than the "bag of constraints" and skolemization techniques common in practical compilers.

This foundational work was subsequently extended by \citeauthor{dunfield-sound-2019} \cite{dunfield-sound-2019} to a much richer language with products, sums, and pattern matching, demonstrating the scalability of the formal approach. The body of work is summarized in a comprehensive survey by \citeauthor{dunfield-bidirectional-2020} \cite{dunfield-bidirectional-2020}, which offers practical guidance for designing new, programmer-friendly bidirectional systems.

\subsection{Refining and Generalizing Bidirectional Typing}

Beyond establishing the core system, subsequent research has focused on refining the bidirectional model to make it more powerful and flexible. \citeauthor{xie-higher-rank} \cite{xie-higher-rank} review and propose refinements to the algorithm from \cite{dunfield-complete-2013}, including a novel algorithm for kind inference.

More recently, \citeauthor{xue-contextual-2024} \cite{xue-contextual-2024} generalized bidirectional typing to \textbf{contextual typing}. Instead of a binary inference/checking mode, this approach propagates arbitrary contextual information, allowing for a more fine-grained specification of precisely where programmer annotations are required.

\subsection{Exploring Alternatives: Impredicativity and Subtyping}

In parallel to the work on predicative systems, another line of research has explored supporting full, first-class impredicativity. As mentioned in \cref{chap:LiteratureReview:sec:PolymorphismAndSystemF}, this allows type variables to be instantiated with polytypes, a feature that unification-based systems struggle with.

\citeauthor{parreaux-when-2024} \cite{parreaux-when-2024} propose \textbf{SuperF}, a novel type inference algorithm that supports impredicative polymorphism by replacing unification with \textbf{subtype inference}. SuperF infers a type for each subterm and then checks its compatibility with user annotations. The authors demonstrate that this approach can type a wide variety of terms, often without explicit annotations. While extremely powerful, this approach represents a significant departure from the GHC-style, unification-based system that is the focus of this thesis. The algorithm has been implemented in Scala and is publicly available\footnote{The SuperF implementation can be found at \url{https://github.com/hkust-taco/superf}.}.

This survey confirms that while many innovative approaches exist, the practical, predicative, and bidirectional system pioneered in \cite{jones-practical-2007} remains a robust and pedagogically valuable foundation for building an educational compiler.