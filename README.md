# FT_TURING

The origin of programming: Ever heard of Turing’s machine ?

![A Turing machine](/img/turing_machine.jpg)

<details>
    <summary>Table of Contents</summary>
    <ol>
        <li>
            <a href="#some-history-and-fundamental-mathematic-cannot-hurt,-can-it?">Some history and fundamental mathematic cannot hurt, can it?</a>
            <ul>
                <li><a href="#the-man-the-legend...-and-his-wikipedia">The Man, the Legend... and his Wikipedia</a></li>
                <li>
                    <a href="#stanford's-encyclopedia's-definition-of-the-machine">Stanford's Encyclopedia's definition of the Machine</a>
                    <ul>
                        <li><a href="#introduction">Introduction</a></li>
                        <li><a href="#turing's-definition">Turing's definition</a></li>
                    </ul>
                </li>
            </ul>
        </li>
        <li>
            <a href="#a-functionnal-programming-journey">A Functionnal Programming Journey</a>
            <ul>
            </ul>
        </li>
        <li>
            <a href="#usage">Usage</a>
            <ul>
                <li><a href="#ocaml">Ocaml</a></li>
                <li><a href="#the-machine">The Machine</a></li>
                <li><a href="#the-machine-in-the-machine">The Machine in the Machine</a></li>
            </ul>
        </li>
        <li><a href="#sources">Sources</a></li>
        <li><a href="#about-me">About me</a></li>
    </ol>
</details>

## Some history and fundamental mathematic cannot hurt, can it?

I would love to write the biography of Alan Turing, a most important man for most of us.  
But it appears that time is precious and Wikipedia exists.  
I would love to be able to give you the clearest definition of the Turing's machine.  
But it appears that mine is not that good (cf. the TMs "<a href="/lib/compiler.md">manifest</a>" I wrote) and Stanford's Encyclopedia exists.

### The Man, the Legend... and his Wikipedia

![Alan, himself](img/turing.jpeg)

Alan Mathison Turing was a British pioneering computer scientist, mathematician,  
logician, cryptanalyst, philosopher, mathematical biologist, and marathon and ultra distance runner.  
He was highly influential in the development of computer science, providing  
a formalisation of the concepts of algorithm and computation with the Turing machine,  
which can be considered a model of a general purpose computer. Turing is widely considered   
to be the father of theoretical computer science and artificial intelligence.  
During the Second World War, Turing worked for the Government Code and Cypher  
School (GC&CS) at Bletchley Park, Britain’s codebreaking centre. For a time he led  
Hut 8, the section responsible for German naval cryptanalysis. He devised a number of  
techniques for breaking German ciphers, including improvements to the pre-war Polish  
bombe method, an electromechanical machine that could find settings for the Enigma  
machine. Turing played a pivotal role in cracking intercepted coded messages that enabled  
the Allies to defeat the Nazis in many crucial engagements, including the Battle  
of the Atlantic; it has been estimated that this work shortened the war in Europe by as  
many as two to four years.  
  
After the war, he worked at the National Physical Laboratory, where he designed  
the ACE, among the first designs for a stored-program computer. In 1948 Turing joined  
Max Newman’s Computing Laboratory at the University of Manchester, where he helped  
develop the Manchester computers and became interested in mathematical biology. He  
wrote a paper on the chemical basis of morphogenesis, and predicted oscillating chemical  
reactions such as the Belousov–Zhabotinsky reaction, first observed in the 1960s.  
Turing was prosecuted in 1952 for homosexual acts, when such behaviour was still  
a criminal act in the UK. He accepted treatment with oestrogen injections (chemical  
castration) as an alternative to prison. Turing died in 1954, 16 days before his 42nd  
birthday, from cyanide poisoning. An inquest determined his death a suicide, but it has  
been noted that the known evidence is equally consistent with accidental poisoning. In  
2009, following an Internet campaign, British Prime Minister Gordon Brown made an  
official public apology on behalf of the British government for “the appalling way he was  
treated”. Queen Elizabeth II granted him a posthumous pardon in 2013.  

### Stanford's Encyclopedia's definition of the Machine

#### Introduction

Turing machines, first described by Alan Turing in Turing 1936–7, are simple abstract computational devices  
intended to help investigate the extent and limitations of what can be computed.  
Turing’s ‘automatic machines’, as he termed them in 1936, were specifically devised for the computing of real numbers.  
They were first named ‘Turing machines’ by Alonzo Church in a review of Turing’s paper (Church 1937).  
Today, they are considered to be one of the foundational models of computability and (theoretical) computer science.
  
Turing introduced Turing machines in the context of research into the foundations of mathematics.  
More particularly, he used these abstract devices to prove that there is no effective general method  
or procedure to solve, calculate or compute every instance of the following problem:  
**Entscheidungsproblem**: The problem to decide for every statement in first-order logic
(the so-called restricted functional calculus, see the entry on classical logic for an introduction)
whether or not it is derivable in that logic.
  
[...] In order to tackle this problem, one needs a formalized notion of “effective procedure” and Turing’s machines were intended to do exactly that.

#### Turing's definition

A Turing machine then, or a computing machine as Turing called it, in Turing’s original definition  
is a machine capable of a finite set of configurations q1,…,qn (the states of the machine, called m-configurations by Turing).  
It is supplied with a one-way infinite and one-dimensional tape divided into squares each capable of carrying exactly one symbol.  
At any moment, the machine is scanning the content of one square r which is either blank (symbolized by S0)  
or contains a symbol S1,…,Sm with S1=0 and S2=1  
  
The machine is an automatic machine (a-machine) which means that at any given moment,  
the behavior of the machine is completely determined by the current state and symbol (called the configuration) being scanned.  
This is the so-called determinacy condition (Section 3).  
These a-machines are contrasted with the so-called choice machines for which the next state depends on the decision of an external device or operator (Turing 1936–7: 232).  
A Turing machine is capable of three types of action:
- Print Si, move one square to the left (L) and go to state qj
- Print Si, move one square to the right (R) and go to state qj
- Print Si, do not move (N) and go to state qj (Small interruption in your assiduous reading. The machine built for this project,  
as required by the subject given by 42, does not stop. However, it is possible to implement this behavior EVENTHOUGH the main head does not stop. It is called a cursor.)

The ‘program’ of a Turing machine can then be written as a finite set of quintuples of the form:  
- qiS(j)S(i,j)M(i,j)q(i,j)
Where qi is the current state, S(j) the content of the square being scanned, S(i,j) the new content of the square;  
M(i,j) specifies whether the machine is to move one square to the left, to the right or to remain at the same square, and q(i,j) is the next state of the machine.  
These quintuples are also called the transition rules of a given machine.  
The Turing machine TSimple which, when started from a blank tape, computes the sequence S0S1S0S1… is then given by Table 1.  
  
Table 1: Quintuple representation of TSimple
- ;q1S0S0Rq2
- ;q1S1S0Rq2
- ;q2S0S1Rq1
- ;q2S1S1Rq1

Note that TSimple will never enter a configuration where it is scanning S1 so that two of the four quintuples are redundant.  
Another typical format to represent Turing machines and which was also used by Turing is the transition table. Table 2 gives the transition table of TSimple.  
  
Table 2: Transition table for TSimple

-	 |   S0    S1
- q1 | S0Rq2 S0Rq2
- q2 | S1Rq1 S1Rq1

Where current definitions of Turing machines usually have only one type of symbols (usually just 0 and 1;  
it was proven by Shannon that any Turing machine can be reduced to a binary Turing machine (Shannon 1956))
Turing, in his original definition of so-called computing machines, used two kinds of symbols: the figures which consist entirely of 0s and 1s and the so-called symbols of the second kind.  
These are differentiated on the Turing machine tape by using a system of alternating squares of figures and symbols of the second kind.  
One sequence of alternating squares contains the figures and is called the sequence of F-squares.  
It contains the sequence computed by the machine; the other is called the sequence of E-squares.  
The latter are used to mark F-squares and are there to “assist the memory” (Turing 1936–7: 232).  
The content of the E-squares is liable to change. F-squares however cannot be changed which means that one  
cannot implement algorithms whereby earlier computed digits need to be changed. Moreover, the machine will never print a symbol
on an F-square if the F-square preceding it has not been computed yet. This usage of F and E-squares can be quite useful (see Sec. 2.3)  
but, as was shown by Emil L. Post, it results in a number of complications (see Sec. 1.2).  
  
There are two important things to notice about the Turing machine setup.  
The first concerns the definition of the machine itself, namely that the machine’s tape is potentially infinite.  
This corresponds to an assumption that the memory of the machine is (potentially) infinite.  
The second concerns the definition of Turing computable, namely that a function will be Turing computable  
if there exists a set of instructions that will result in a Turing machine computing the function regardless of the amount of time it takes.  
One can think of this as assuming the availability of potentially infinite time to complete the computation.  
  
These two assumptions are intended to ensure that the definition of computation that results is not too narrow.
This is, it ensures that no computable function will fail to be Turing-computable solely because there is insufficient time or memory to complete the computation.  
It follows that there may be some Turing computable functions which may not be carried out by any existing computer, perhaps because no existing machine has sufficient memory to carry out the task.  
Some Turing computable functions may not ever be computable in practice, since they may require more memory than can be built using all of the (finite number of) atoms in the universe.  
If we moreover assume that a physical computer is a finite realization of the Turing machine, and so that the Turing machine functions as a good formal model for the computer,  
a result which shows that a function is not Turing computable is very strong, since it implies that no computer that we could ever build could carry out the computation.  
  
If you want to read more about it, you can find its url in the <a href="#sources">sources</a>.

<p align="right">(<a href="#ft_turing">back to top</a>)</p>

## A Functionnal Programming Journey

Here will be written soon how I went from 1 + 1 is easy to 1 + 1 is not that easy if it can also tell if a word is a palindrom or not.

## Usage

### Ocaml

The project has been witten with Ocaml 5.3 (it is important that the compiler you are using is of the same version else it might not work).  
The subject requires that ocamlopt is used to compile the project. However, as I am not sure if I would be able to validate the project,  
(long story, short story, I do not live in the same country as where my school is), I used the build system Dune that is the main tool used today for ANY ocaml project.  
Which means that if, one day, I need to code in Ocaml at work, Dune will be used 99.99% of the time. And I used ocamlopt for the piscine already. That's enough.  
  
Comming back to the usage. You have to install Ocaml, the project's librairies and Dune (preferably) with opam (Ocaml's package manager):
```bash
opam install ocaml dune yojson core
eval $(opam env)
```
I suggest that you put in your .bashrc or .zshrc the last command as you want to activate your updated switch when you open your terminal.  
Then you can build the project:
```bash
dune build
```

### The Machine

Now that we have a project built and ready to be run, let's talk about the machine.  
The machine takes as input a JSON file that contains all the necessary informations for the machine to run accordingly.  
The json fields are defined as follows:
- name: The name of the described machine
- alphabet: Both input and work alphabet of the machine merged into a single alphabet for simplicity’s sake, including the blank character. Each character of the alphabet must be a string of length strictly equal to 1.
- blank: The blank character, must be part of the alphabet, must NOT be part of the input.
- states: The exhaustive list of the machine’s states names.
- initial: The initial state of the machine, must be part of the states list.
- finals: The exhaustive list of the machine’s final states. This list must be a sub-list of the states list.
- transitions: A dictionnary of the machine’s transitions indexed by state name. Each transition is a list of dictionnaries, and each dictionnary describes the transition for a given character under the head of the machine.  
A transition is defined as follows:
1. read: The character of the machine’s alphabet on the tape under the machine’s head.
2. to_state: The new state of the machine after the transition is done.
3. write: The character of the machine’s alphabet to write on the tape before moving the head.
4. action: Movement of the head for this transition, either LEFT, or RIGHT.

You can find examples of machine description in the /machine_examples folder.  
To run the program with your machine description:
```bash
dune exec ft_turing machine_example.json "input_example"
```

### The Machine in the Machine

It happens that once we have built some of our machines, (as one solving the palindrom problem or 1 + 1 with unary addition), it is asked to build a machine that can **run** an unary_add machine.  
By that, I mean that I had to build a Turing machine in a Turing machine that runs other Turing machines, a Universal Turing Machine (UTM). I let you think a little about what this tasks means.  
Personally, as I have been a bit lazy, my UTM is more than 50 000 lines long.  
Thankfully for you, I have written the basis for a <a href="/lib/compiler.md">Turing Machine assembly language</a> that allows you to write it in a bit more than 300 lines of code. To use it, you just need to compile the /assembly_files/utm.s file and run it with your favorite machine.
```bash
dune exec ft_turing -- -c assembly_files/utm.s
dune exec ft_turing utm.json "`dune exec ft_turing -- -UTMc machine_examples/unary_add.json`"
```

## Sources

- <a href="https://en.wikipedia.org/wiki/Alan_Turing">Wikipedia's Turing page</a>
- <a href="https://plato.stanford.edu/entries/turing-machine/#DefiTuriMach">Stanford's Bib... Encyclopedia about the Machine</a>
- <a href="https://ocaml.org/">Ocaml, the Camel that does not smoke</a>
- <a href="https://dune.build/">Dune... but no sandworm</a>
- <a href="https://www.sciencedirect.com/science/article/pii/S0304397596000771?ref=pdf_download&fr=RR-2&rr=906951249e37ecd8">Some dark magic to make tiny Universal Turing machines</a>

## About me
email: victor.cornille@gmail.com or victor.bolheme@gmail.com  
linkedin: https://www.linkedin.com/in/victorcornille/

<p align="right">(<a href="#ft_turing">back to top</a>)</p>
