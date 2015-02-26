\documentclass[paper=a4, fontsize=12pt]{article}

\usepackage[portuguese]{babel}
\usepackage{amsmath,amsfonts,amssymb,amsthm}
\usepackage[utf8]{inputenc}
\usepackage{color}
\usepackage{proof}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\DeclareMathAlphabet{\mathkw}{OT1}{cmss}{bx}{n}
%subst keyword a = "\K{" a "}"
%subst conid a = "\C{" a "}"
%subst varid a = "\V{" a "}"
%subst numeral a = "\N{" a "}"
%format Set = "\D{Set}"
%format Nat = "\C{\mathbb{N}}"

\usepackage{color}
\newcommand{\redFG}[1]{\textcolor[rgb]{0.6,0,0}{#1}}
\newcommand{\greenFG}[1]{\textcolor[rgb]{0,0.4,0}{#1}}
\newcommand{\blueFG}[1]{\textcolor[rgb]{0,0,0.8}{#1}}
\newcommand{\orangeFG}[1]{\textcolor[rgb]{0.8,0.4,0}{#1}}
\newcommand{\purpleFG}[1]{\textcolor[rgb]{0.4,0,0.4}{#1}}
\newcommand{\yellowFG}[1]{\textcolor{yellow}{#1}}
\newcommand{\brownFG}[1]{\textcolor[rgb]{0.5,0.2,0.2}{#1}}
\newcommand{\blackFG}[1]{\textcolor[rgb]{0,0,0}{#1}}
\newcommand{\whiteFG}[1]{\textcolor[rgb]{1,1,1}{#1}}
\newcommand{\yellowBG}[1]{\colorbox[rgb]{1,1,0.2}{#1}}
\newcommand{\brownBG}[1]{\colorbox[rgb]{1.0,0.7,0.4}{#1}}

\newcommand{\ColourStuff}{
  \newcommand{\red}{\redFG}
  \newcommand{\green}{\greenFG}
  \newcommand{\blue}{\blueFG}
  \newcommand{\orange}{\orangeFG}
  \newcommand{\purple}{\purpleFG}
  \newcommand{\yellow}{\yellowFG}
  \newcommand{\brown}{\brownFG}
  \newcommand{\black}{\blackFG}
  \newcommand{\white}{\whiteFG}
}

\ColourStuff

\newcommand{\D}[1]{\blue{\mathsf{#1}}}
\newcommand{\C}[1]{\green{\mathsf{#1}}}
\newcommand{\F}[1]{\green{\mathsf{#1}}}
\newcommand{\V}[1]{\blue{\mathit{#1}}}
\newcommand{\N}[1]{\purple{\mathit{#1}}}
\newcommand{\K}[1]{\orange{\mathkw{#1}}}

\newtheorem{theorem}{Teorema}
\newtheorem{lemma}{Lema}
\newtheorem{corollary}{Corolário}

\begin{document}

\begin{center}
UNIVERSIDADE FEDERAL DE OURO PRETO \\
DEPARTAMENTO DE COMPUTAÇÃO E SISTEMAS\\
\end{center}
\vspace{2cm}
\begin{center}
PROJETO E IMPLEMENTAÇÃO DE UMA LINGUAGEM DE PROGRAMAÇÃO PARA
DESENVOLVIMENTO DE SISTEMAS EMBARCADOS CORRETOS POR CONSTRUÇÃO
\end{center}
\vspace{4cm}
\begin{flushleft}
Aluno: Raul Filipe Pimenta Lopes\\
Orientador: Dr. Rodrigo Geraldo Ribeiro\\
\end{flushleft}
\vspace{4cm}
\noindent
Relatório Final, referente ao período 02/2014 a 02/2015, apresentado à
Universidade Federal de Ouro Preto, como parte das exigências do
programa de iniciação científica do edital 99/2014 - PROGRAMA DE BOLSAS DE INICIAÇÃO À PESQUISA
\vspace{1cm}
\begin{center}
Ouro Preto - Minas Gerais - Brasil\\
\today
\end{center}

%%%%%%%%%%%%%%%%

\section{Página de Resumo}

\vspace{2cm}

\begin{center}
{\bf\large Título:} {\large Projeto e implementação de uma linguagem de
programação para desenvolvimento de sistemas embarcados corretos por construção}.
\end{center}

\vspace{1cm}

O objetivo deste trabalho foi o projeto e implementação de uma
linguagem de alto nível para desenvolvimento de sistemas
embarcados. Nesta linguagem programadores devem ser capazes de
descrever programas e propriedades que devem ser satisfeitas por
estes. Tais propriedades devem ser verificadas pelo compilador com o
intuito de que o software construído possua sua prova de correção, o
que caracteriza o paradigma de software correto por construção.
Neste projeto foi elaborada a sintaxe, semântica e sistemas de tipos,
juntamente com a respectiva formalização destes para a linguagem em
questão e um protótipo de compilador encontra-se em fase final de
implementação e pode ser acessado em:
\begin{center}
https://github.com/lives-group/arduino-ml
\end{center}
\vspace{4cm}

\begin{center}
\begin{tabular}{c}
\hline
Bolsista: Raul Filipe Pimenta Lopes \\
$\,$  \\
$\,$  \\
$\,$  \\
$\,$  \\
$\,$  \\ \hline
Orientador: Prof. Dr. Rodrigo Geraldo Ribeiro
\end{tabular}
\end{center}
\clearpage

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Introdução}

Sistemas embarcados s\~ao ub\'iquos na sociedade moderna e est\~ao presentes
nos mais diversos dispositivos eletr\^onicos como celulares, aparelhos de GPS e em
aplica\c{c}\~oes onde a corretude e seguran\c{c}a s\~ao fatores cr\'iticos
como aplica\c{c}\~oes banc\'arias e dispositivos m\'edicos. O desenvolvimento
de softwares onde a corretude \'e um fator primordial \'e uma tarefa
desafiadora pois, muitas vezes o programador deve lidar com detalhes do hardware
como registradores de tamanho fixo, acesso direto a mem\'oria, portas de
entrada e sa\'ida e concorr\^encia. Por outro lado, para garantir um
comportamento correto, incluindo propriedades de corre\c{c}\~ao e
seguran\c{c}a, o mesmo c\'odigo de baixo n\'ivel deve ser representado por
modelos abstratos que podem ser analisados rigorosamente por ferramentas como
verificadores de modelos\footnote{Um verificador de modelos \'e um software
  que permite a descri\c{c}\~ao de modelos e de propriedades que devem ser
  satisfeitas por esses. O processo de verifica\c{c}\~ao de propriedades \'e
  realizado de maneira autom\'atica pela ferramenta, sem interven\c{c}\~ao do
  usu\'ario.}\cite{Baier08} e assistentes de
provas\footnote{Um assistente de provas \'e uma linguagem de programa\c{c}\~ao
que permite ao desenvolvedor construir demonstra\c{c}\~oes de maneira
semi-autom\'atica, isto \'e, nem toda prova \'e feita pelo
assistente de provas. Na grande maioria dos casos, \'e necess\'ario que o
usu\'ario descreva, utilizando a linguagem do assistente, todos os passos
necess\'arios para demonstra\c{c}\~ao da propriedade em quest\~ao.} \cite{Coq-art,Agda-overview}.

Falhas em um sistema de software s\~ao um problema grave em qualquer dom\'inio
de aplica\c{c}\~ao, por\'em, as consequ\^encias de uma falha em softwares
embarcados tendem a ser severas: mesmo simples erros na manipula\c{c}\~ao de detalhes
de baixo n\'ivel de hardware podem comprometer o sistema por completo.
Apesar dos avan\c{c}os no projeto de linguagens de programa\c{c}\~ao, a
maioria de softwares embarcados continua sendo desenvolvido em linguagens de
baixo n\'ivel como C ou assembly. A utiliza\c{c}\~ao de tais ferramentas
permite a solu\c{c}\~ao de problemas de desempenho, mas torna dif\'icil o
racioc\'inio formal sobre a corretude do c\'odigo em quest\~ao
\cite{DiatchkiPhD}.

Linguagens funcionais modernas como ML \cite{SML} e Haskell \cite{haskell2010}
prov\^eem abstra\c{c}\~oes de alto n\'ivel e outros benef\'icios para aumentar a
produtividade e reuso de c\'odigo. Tais linguagens tamb\'em fornecem garantias
sobre tipos e gerenciamento de mem\'oria, automaticamente detectando e
eliminando erros comuns em tempo de compila\c{c}\~ao, e, devido a sua
fundamenta\c{c}\~ao matem\'atica \cite{lee07,Faxen02}, essas prove\^em oportunidades para a verifica\c{c}\~ao e
valida\c{c}\~ao de software de maneira a fornecer os maiores n\'iveis de
confiabilidade, valendo-se de assistentes de provas ou verificadores de
modelos. Por\'em, a utiliza\c{c}\~ao de assistentes de provas, como Coq
\cite{Coq-art}, ou verificadores de modelos \cite{Baier08}, como o Prism \cite{KNP11}, \'e uma tarefa
complexa e n\~ao diretamente relacionada ao desenvolvimento do programa em
quest\~ao. Isso se deve ao fato de que para utiliz\'a-las deve-se expressar tanto o software
quanto sua especifica\c{c}\~ao em uma linguagem diferente (a linguagem
utilizada pela ferramenta de verifica\c{c}\~ao), o que pode desmotivar o
desenvolvedor a provar a corretude de seu programa e valer-se apenas de testes
para valida\c{c}\~ao desse.

Neste sentido, atualmente h\'a um grande interesse em linguagens que permitem
integrar tarefas de desenvolvimento e verifica\c{c}\~ao.
Linguagens de programa\c{c}\~ao que proveem suporte a tipos
dependentes permitem o chamado desenvolvimento de software correto por
constru\c{c}\~ao \cite{Luo94}, em que programas podem ser
constru\'idos juntamente com a prova de que esse est\'a de acordo com sua
especifica\c{c}\~ao \cite{Luo94,Coq-art,nordstrom90}. Nessas linguagens,
especifica\c{c}\~oes s\~ao descritas como tipos, pois tipos dependentes
permitem definir de maneira precisa propriedades
que devem ser satisfeitas por fun\c{c}\~oes para que essas sejam consideradas
corretas \cite{OuryS08}. Dessa maneira, o processo de verificar se um determinado
programa est\'a ou n\~ao de acordo com sua
especifica\c{c}\~ao resume-se \`a verifica\c{c}\~ao de tipos realizada pelo
compilador da linguagem em quest\~ao.

Apesar de ampla pesquisa nas \'areas de l\'ogica e sistemas de tipos
\cite{Luo94,nordstrom90,Sorensen06,Girard89}, existem somente duas
implementa\c{c}\~oes ``maduras'' de linguagens com suporte a tipos dependentes: Agda
\cite{Agda-overview} e Coq \cite{Coq-art}. Por\'em, ambas s\~ao utilizadas em grande parte como
assistentes de provas e n\~ao compiladores. Isso se deve ao fato de que essas
linguagens possuem geradores de c\'odigo experimentais (como no caso de Agda)
ou apenas traduzem o programa em quest\~ao para outra linguagem (como
Coq)\cite{Swierstra12,Berger93}. O processo de extra\c{c}\~ao de Coq permite
obter c\'odigo para linguagens funcionais como ML \cite{SML} e
Haskell \cite{haskell2010} o que possibilita a gera\c{c}\~ao de um programa execut\'avel
utilizando-se um compilador para essas linguagens. Como esses
compiladores n\~ao proveem suporte a diversas plataformas de desenvolvimento
de sistemas embarcados como Ardu\'ino \cite{Arduino} e Raspberry Pi
\cite{RaspberryPi}, n\~ao h\'a uma alternativa que permita o desenvolvimento
de sistemas embarcados corretos por constru\c{c}\~ao em linguagens funcionais
com suporte a tipos dependentes. Neste sentido, o presente projeto pretende
iniciar a implementa\c{c}\~ao de um compilador, para a plataforma Ardu\'ino, do n\'ucleo de
uma linguagem de programa\c{c}\~ao funcional\footnote{Denomina-se por
  n\'ucleo de uma linguagem de programa\c{c}\~ao o conjunto m\'inimo de
  constru\c{c}\~oes sint\'aticas dessa que permite a defini\c{c}\~ao de todo e
  qualquer programa.} com suporte a tipos dependentes.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Objetivos}\label{objetivos}

O principal objetivo desse projeto \'e a defini\c{c}\~ao de um n\'ucleo de uma
linguagem funcional com tipos dependentes e da implementa\c{c}\~ao de seu
compilador para os processadores da plataforma Ardu\'ino, amplamente utilizada
no desenvolvimento de sistemas embarcados.

Mais especificamente, esse
objetivo pode ser detalhado pelas seguintes metas:
\begin{enumerate}
  \item Revis\~ao da literatura para analisar o estado da arte em projeto de
    linguagens funcionais com suporte a tipos dependentes.
  \item Defini\c{c}\~ao da sintaxe, sem\^antica e sistema de tipos da
    linguagem n\'ucleo em quest\~ao e demonstra\c{c}\~ao de propriedades
    desses formalismos.
  \item Elabora\c{c}\~ao de um algoritmo de verifica\c{c}\~ao de tipos para a
    linguagem definida e sua respectiva implementa\c{c}\~ao na linguagem Haskell.
  \item Implementa\c{c}\~ao de um compilador prot\'otipo da linguagem n\'ucleo
    considerada para c\'odigo de m\'aquina.
\end{enumerate}

%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Desenvolvimento}

Nesta seção descreveremos as tarefas desenvolvidas durante a execução deste projeto.
Na seção \ref{syntax}, apresentamos a sintaxe da linguagem núcleo considerada. A seção \ref{semantics} apresenta a
semântica de passo pequeno para a linguagem considerada. A seção \ref{typesystem} apresenta o sistema de tipos da linguagem considerada, apresentando a sintaxe da
linguagem de tipos e o sistema de kinding para operadores de tipos. O algoritmo de inferência de tipos e a elaboração de termos da linguagem
para o Sistema F \cite{Pierce02} são descritos na seção \ref{inference}. A partir da sintaxe de tipos, apresentamos
a denotação dos termos da linguagem projetada traduzindo-os em programas Agda, o que pode ser entendido como uma semântica denotacional
para a linguagem considerada. A metateoria da linguagem da linguagem apresentada é apresentada na seção
\ref{metatheory} em que os principais teoremas são enunciados e as principais estratégias utilizadas em suas demonstrações são descritas.

\subsection{Sintaxe da Linguagem Núcleo}\label{syntax}

A sintaxe da linguagem núcleo considerado consiste do $\lambda$-cálculo acrescido de produções let,
o que é conhecido na literatura como core-ML \cite{Pierce02}. Nesta linguagem utilizamos o conjunto $\mathcal{V}$
de todas as possíveis variáveis de termos. A gramática seguinte descreve os termos, em que $x\in\mathcal{V}$, representa variável qualquer.
\begin{figure}[h]
\begin{tabular}{rcl}
  $e$ & $::=$ & $x$\\
      &  $\mid$  & $\lambda x. e$ \\
      &  $\mid$  & $e\:e$ \\
      &  $\mid$  & let $x = e$ in $e$\
\end{tabular}
\centering
\end{figure}

\noindent
Evidentemente, apesar de extremamente reduzida, esta linguagem é capaz de representar quaisquer funções computáveis \cite{Pierce02}.

Como uma forma de garantir a correção da formalização da linguagem desenvolvida neste projeto, modelamos todas as definições desta
utilizando a linguagem Agda. A seguir, apresentamos a definição da sintaxe de termos, como um tipo de dados Agda.

%if False
\begin{code}
data Nat : Set where
  zero : Nat
  suc  : Nat -> Nat

{-# BUILTIN NATURAL Nat #-}


data List (A : Set) : Set where
  []   : List A
  _::_ : A -> List A -> List A

infixr 4 _++_

_++_ : {A : Set} -> List A -> List A -> List A
[] ++ ys = ys
(x :: xs) ++ ys = x :: (xs ++ ys)

[_] : {A : Set} -> A -> List A
[ x ] = x :: []

_-_ : {A : Set} -> List A -> List A -> List A
xs - ys = []
\end{code}
%endif

\begin{code}
Var : Set
Var = Nat

data Term : Set where
  var : Nat -> Term
  abs : Nat -> Term -> Term
  app : Term -> Term -> Term
  leti : Nat -> Term -> Term -> Term
\end{code}
A notação $ftv(e)$ representa o conjunto de variáveis livres presentes em um termo $e$. A definição desta função é apresentada a seguir.
\begin{code}
ftv : Term -> List Var
ftv (var n) = [ n ]
ftv (abs n e) = ftv e - [ n ]
ftv (app e e') = ftv e ++ ftv e'
ftv (leti n e e') = (ftv e ++ ftv e') - [ n ]
\end{code}
Na definição de $\V{ftv}$ a função $\V{[-]}$ constrói uma lista contendo um único elemento e $-$ representa a diferença de duas listas.

\subsection{Semântica da Linguagem Núcleo}\label{semantics}

Neste trabalho adotaremos uma semântica tradicional para o $\lambda$-cálculo. Antes de apresentar a semântica, devemos, primeiramente,
definir quais termos da linguagem deverão ser considerados valores, isto é, termos que não podem ser mais reduzidos\footnote{De maneira simplista,
podemos dizer que tais termos podem ser considerados ``respostas'' do programa representado por um certo termo.}. Como não definimos constantes na sintaxe
da linguagem (números, caracteres, etc), consideraremos como valores termos funcionais da forma $\lambda x. e$.

A semântica é representada como uma relação binária entre termos, representada por $e \Rightarrow e'$, que intuitivamente especifica que o termo $e$
reduz em um passo para o termo $e'$. A definição da semântica é apresentada na figura seguinte, em que as variáveis $e$ representam termos quaisquer,
$x$ uma variável e $v$ um valor.

\[
\begin{array}{c}
  \fbox{$e \Rightarrow e'$}\\
   \\
   \\
   \begin{array}{c}
   \infer[_{(E-App1)}]{e_1\:e_2 \Rightarrow e'_1 \:e_2}
                   {e_1 \Rightarrow e'_1}

  \\
  \\
     \infer[_{(E-App2)}]{v_1\:e_2 \Rightarrow v_1 \:e'_2}
                   {e_2 \Rightarrow e'_2}

\\
\\

\infer[_{(E-AppAbs)}]{(\lambda x. e) v \Rightarrow [x \mapsto v]e}{}
\end{array}
\end{array}
\]
O significado das regras é imediato. A regra (E-App1) especifica que, em uma aplicação de função, devemos reduzir primeiro o termo correspondente à função. Por sua vez,
a regra (E-App2) especifica que podemos reduzir um parâmetro de função assim que esta já for um valor (expressão $\lambda$) e, finalmente, a regra (E-AppAbs) especifica
a operação de substituição, representada por $[x\mapsto v] e$, que representa a substituição de cada ocorrência livre da variável livre $x$ pelo valor $v$ no termo $e$.
No modelo Agda para esta formalização, representamos variáveis usando índices DeBruijn \cite{dB72} e implementamos a operação de substituição seguindo a definição
padrão \cite{Mcbride05}, que é omitida por questões de brevidade.

Antes de apresentar o código Agda para a semântica, faz-se necessário definir o que constitui um valor. Para isso, utilizamos a função $\V{value}$, que a partir de um termo,
retorna como resultado um tipo $\C{Unit}$, caso este seja um valor, e $\C{Empty}$, caso contrário. Os tipos $\C{Unit}$ e $\C{Empty}$ são tratados de maneira especial pelo
compilador de Agda que é capaz de identificar que $\C{Unit}$ é o tipo com um único valor e $\C{Empty}$ o tipo que não possui valor. Em ambas as situações, o compilador de Agda é capaz
de sintetizar automaticamente termos possuindo um destes tipos.

\begin{code}
value : Term -> Set
value (var x) = Empty
value (abs x t) = Unit
value (app t t') = Empty
value (leti x t t') = Empty
\end{code}

Utilizando a função $\V{value}$, podemos representar a semântica da linguagem usando o tipo de dados apresentado a seguir.
O uso de tipos de dados indutivos para codificar sistemas de prova (como a semântica apresentada) é
uma das características principais de linguagens, como Agda, que provêem suporte a tipos dependentes.

%if False
\begin{code}
infix 4 _=>_

subst : Nat -> Term -> Term -> Term
subst n v e = e

data Unit : Set where
  tt : Unit

data Empty : Set where
\end{code}
%endif

%format Nat = "\C{\mathbb{N}}"
%format _=>_ = "\_\!\C{\Rightarrow}\_\!"
%format => = "\C{\Rightarrow}"
%format forall = "\C{\forall}"

\begin{code}
data _=>_ : Term -> Term -> Set where
  eApp1 : forall {e1 e1' e2 : Term} -> e1 => e1' -> app e1 e2 => app e1' e2
  eApp2 : forall {v e2 e2' : Term} -> value v -> e2 => e2' -> app v e2 => app v e2'
  eAppAbs : forall {n}{x e v : Term} -> app (abs n e) v => subst n v e
\end{code}

O leitor atento deve ter percebido que não apresentamos a semântica de produções let. Isso se deve ao fato de
que expressões let podem ser entendidas como uma forma de açúcar sintático, isto é, uma abreviação para uma aplicação de função.
Desta forma, optamos por simplificar a semântica desconsiderando expressões let por traduzí-las em aplicações.

\subsection{Sistema de Tipos}\label{typesystem}

Evidentemente nem todo programa correto sintaticamente possui significado. A principal finalidade de um sistema de tipos é
identificar quais programas sintaticamente corretos possuem semântica bem definida. Antes de apresentarmos o sistema de tipos em si,
faz-se necessário discutirmos a sintaxe de tipos e como pode-se definir novos tipos na linguagem. Além disso, discutiremos um mecanismo
de \emph{kindding}\cite{Pierce02}, que permite identificar tipos sintaticamente corretos sem semântica definida. Utilizando as definições de
sintaxe de tipos e de \emph{kindding}, a seção \ref{typesystemdef} apresenta o sistema de tipos projetado para a linguagem descrita neste relatório.

\subsubsection{Sintaxe de Tipos e Kinds}

Tipos e \emph{kinds} s\~ao expressos utilizando a seguinte gram\'atica (onde o uso de me\-ta\-va\-ri\-\'a\-veis \'e
tamb\'em indicado):

\begin{figure}[h]

\[ \begin{array}[c]{llll}
\text{{\small Kind}} & k\,\in\text{\textbf{K}}   & ::= \star \mid k\, \rightarrow k^{\prime} \\
\text{Express\~ao de Tipo Simples} & \mu\in\text{\textbf{T}} & ::= \alpha \mid T \mid
                                                       \mu_1\:\mu_2 \\
\text{Tipo Simples}      & \tau & \equiv \mu^{\star} \\
\text{Tipos}             & \sigma & ::= \tau \mid \, \forall\,\overline{\alpha}.\,\tau \\ \\
\text{Construtor de Tipos} & T \in \textbf{TC}\\
\text{Vari\'avel de Tipo} & \alpha,\beta\in\text{\textbf{V}} & \hspace*{.5cm}
\end{array} \]
\centering
\caption{Sintaxe livre de contexto de tipos}
\label{Type-syntax}
\end{figure}

Cabe ressaltar que o nome \emph{vari\'avel de tipo} \'e usado por simplicidade. Na verdade, trata-se de vari\'avel
de express\~ao de tipo.

\subsubsection{Tipos Simples e Kinds}

Um \emph{kind} \'e uma propriedade de express\~oes de tipos simples. Os valores de \emph{kind} de vari\'aveis de tipo e o de
construtores de tipo s\~ao dados, respectivamente, pelas fun\c{c}\~oes
$\text{kind}_{\text{\textbf{V}}} : \text{\textbf{V}}\rightarrow\text{\textbf{K}}$ e
$\text{kind}_{\text{\textbf{TC}}} : \text{\textbf{TC}}\rightarrow\text{\textbf{K}}$. Estas fun\c{c}\~oes induzem fam\'ilias
indexadas por \emph{kind} de vari\'aveis de tipo e de construtores, especificadas da seguinte maneira:
\[
\begin{array}{ccl}
   \textbf{V}^k & = & \{\alpha\in\textbf{V}\,\mid\,\text{kind}_{\textbf{V}}(\alpha) = k\}\\
   \textbf{TC}^k & = & \{T\in\textbf{TC}\,\mid\,\text{kind}_{\textbf{TC}}(T) = k\}\\
\end{array}
\]
Utilizamos um \'indice superior $k$ para identificar o \emph{kind} de vari\'aveis de tipo e construtores:
\begin{center}
	\begin{tabular}{ccc}
  		$\alpha^{k}$ & $\in$ & $\text{\textbf{V}}^{k}$ \\
  		$T^{k}$ & $\in$ & $\text{\textbf{TC}}^{k}$ \\
  	\end{tabular}
\end{center}

O \emph{kind} de express\~oes de tipo \'e dado pela fun\c{c}\~ao parcial
\emph{kind}$_{\text{\textbf{T}}}$ : \textbf{T}$\rightarrow$ \textit{K}, definida como:
\begin{equation*}
	\text{kind}_{\text{\textbf{T}}}(\tau)=\left\{
		\begin{array}{ll}
			k & \text{se $\tau=\alpha^{k}$ ou $\tau = T^{k}$}\\
			k & \text{se $\tau=\tau_{1}\,\tau_{2}$, kind$_{\text{\textbf{T}}}(\tau_{1}) = k^{\prime}\rightarrow k$ e
			      $kind_{\text{\textbf{T}}}(\tau_{2})=k^{\prime}$}
		\end{array}
	\right.
\end{equation*}
Consideramos que o conjunto de tipos \textbf{T} inclui somente express\~oes de
tipo \emph{bem formadas},
isto \'e, aquelas que t\^em um \emph{kind}
definido.  Por defini\c{c}\~ao, o
\emph{kind} de tipos simples \'e igual a $\star$.

Como usual,  express\~oes de tipo e de \emph{kind} que envolvem o construtor ($\rightarrow$) s\~ao escritas de forma
infixa.

A nota\c{c}\~ao $[\tau]$ \'e usada para expressar a aplica\c{c}\~ao do construtor de listas (que possui
 \emph{kind} $\star\rightarrow\star$) ao tipo $\tau$.

 O conjunto de vari\'aveis de uma express\~ao de tipo simples \'e dado pela fun\c{c}\~ao
 \emph{tv} : \textbf{T}$\rightarrow\,\mathcal{P}(\text{\textbf{V}})$, definida como:
\begin{equation*}
	tv(\mu) = \left\{
		\begin{array}{lll}
			\{\alpha\}          & \text{se } \mu = \alpha, & \text{para algum }\alpha\in\text{\textbf{V}}\\
			\emptyset       & \text{se } \mu = T, & \text{para algum }T\in\text{\textbf{TC}} \\
			tv(\mu_{1})\, \cup\, tv(\mu_{2})& \text{se } \mu = \mu_{1}\,\mu_{2}
		\end{array}
	           \right.
\end{equation*}
Esta opera\c{c}\~ao pode ser utilizada para denotar a fam\'ilia de vari\'aveis de um conjunto
\textbf{J} de express\~oes de tipo:
\begin{center}
   $tv(\text{\textbf{J}})=\bigcup\{tv(\mu)\,\mid\,\mu\in\text{\textbf{J}}\}$
\end{center}

\subsubsection{Tipos}

Uma express\~ao $\sigma=\forall\,\overline{\alpha}.\tau$
denota um tipo, onde
$\overline{\alpha}$ denota uma sequ\^encia, possivelmente vazia, de elementos
do conjunto
$\{\alpha_1,...\alpha_n\}$. Caso $\overline{\alpha}=\emptyset$,
dizemos que este \'e um tipo monom\'orfico, caso contr\'ario dizemos que \'e um tipo polim\'orfico.
Utilizaremos a seguinte abrevia\c{c}\~ao s\~ao usadas: se $\sigma = \forall\,\overline{\alpha}.\,\tau$ e
$\overline{\alpha}=\emptyset$, representaremos $\sigma$ somente por  $\tau$.

O conjunto de vari\'aveis livres de um tipo \'e definido como:
\begin{center}
	$tv(\forall\,\overline{\alpha}.\tau)=tv(\tau) - \overline{\alpha}$
\end{center}

Novamente, esta fun\c{c}\~ao pode ser estendida para conjuntos de tipos de
maneira trivial:
\begin{center}
   $tv(\text{\textbf{X}})=\bigcup\{tv(\sigma)\,\mid\,\sigma\in\text{\textbf{X}}\}$
\end{center}

\subsubsection{Substitui\c{c}\~oes}\label{substituicao}

Uma substitui\c{c}\~ao $S$ \'e uma fun\c{c}\~ao de vari\'aveis de tipo para express\~oes de tipo.
Representamos a substitui\c{c}\~ao identidade por \textit{id}.

Define-se o dom\'inio de uma substitui\c{c}\~ao $S$, $dom(S)$, como o conjunto de vari\'aveis
$\alpha\in\text{\textbf{V}}$ para as quais $S(\alpha)$ \'e distinto de $\alpha$, isto \'e:
\begin{center}
	$dom(S) = \{\alpha\,\mid\,S(\alpha)\neq\alpha\}$
\end{center}

Neste trabalho consideramos apenas substitui\c{c}\~oes $S$ para as quais o
conjunto dom($S$) \'e finito, e tais que $S$ preserva
o $kind$ das vari\'aveis, i.e., para toda va\-ri\-\'a\-vel $\alpha$, $S(\alpha^{k})\in \texttt{T}^{k}$.

A nota\c{c}\~ao $[\alpha_1\,\mapsto\,\mu_1,...,\alpha_n\,\mapsto\,\mu_n]$,
para $n\geq 1$ e $\alpha_i \neq \alpha_j$ para $1\leq i < j \leq n$ denota a substitui\c{c}\~ao $S$
definida como:
\begin{equation*}
    S(\alpha) = \left\{
                   \begin{array}{ll}
                   	   \mu_{i} & \text{se } \alpha = \alpha_{i}\\
                   	   \alpha   & \text{caso contr\'ario}
                   \end{array}
                \right.
\end{equation*}
Utilizamos a nota\c{c}\~ao $[\overline{\alpha\mapsto\mu}]$
para representar $[\alpha_{1}\mapsto\mu_{1},...,\alpha_{n}\mapsto\mu_{n}]$.

\subsubsection{Contextos de Tipos e de Kinds}

Um contexto de tipos (kinds) cont\'em informa\c{c}\~oes sobre um programa, que podem ser
utilizadas pelas regras do sistema de tipos (de kinds). De maneira simplista, contextos são funções finitas
de nomes em objetos de interesse, que neste trabalho serão tipos e kinds. Denotaremos contextos de tipos pela variável $\Gamma$
e contextos de kinds por $\Delta$.

As seguintes operações são definidas sobre contexto $\Gamma$:
\[
\begin{array}{lcl}
  \Gamma(x) & = & \{\sigma\,\mid\,x : \sigma \in \Gamma\} \\
  \Gamma, x : \sigma & = & (\Gamma - \Gamma(x))\cup\{x : \sigma\}\\
\end{array}
\]
As operações acima aplicam-se igualmente a contextos de kinds.

\subsubsection{Inferência de Kinds}

O processo de inferência e verificação de kinds é necessário para a detecção de tipos sintaticamente corretos porém, semanticamente inválidos.
Comparado com o sistema de verificação de tipos em si, o processo de verificação e inferência de kinds é bastante simples e é equivalente a
verificação de tipos para o $\lambda$-cálculo tipado simples \cite{Pierce02}.

O sistema de kinding é definido por um sistema de provas para julgamentos
da forma $\Delta\vdash \mu : k$, em que $\Delta$ é um contexto de kinds, $\mu$ uma expressão de tipo simples e $k$ um kind.

\[
\begin{array}{c}
  \fbox{$\Delta \vdash \mu : k$}\\
  \\
  \begin{array}{c}
    \infer[_{(K-Var)}]{\Delta \vdash \alpha : k}
                 {\Delta(\alpha) = k} \\ \\
    \infer[_{(K-TyCon)}]{\Delta \vdash T : k}
                 {\Delta(T) = k} \\ \\
    \infer[_{(K-TyApp)}]{\Delta \vdash \mu_1\:\mu_2 : k}
                        {\Delta \vdash \mu_1 : k' \rightarrow k & \Delta \vdash \mu_2 : k'}
  \end{array}
\end{array}
\]

Uma formalização do processo de inferência de kinds, usando a linguagem Agda, é parte do código produzido durante este projeto e encontra-se disponível em
\cite{arduino-ml}.

\subsubsection{Definição do Sistema de Tipos}\label{typesystemdef}

A principal finalidade de sistemas de tipos em linguagens funcionais é evitar que funções sejam aplicadas a parâmetros de tipos incompatíveis com seus parâmetros.
Definiremos o sistema de tipos como um sistema de provas para derivar julgamentos da forma $\Gamma\vdash e : \tau$, em que $\Gamma$ é um contexto de tipos, $e$ é uma expressão (programa) e
$\tau$ é um tipo bem formado (isto é, é possível provar que $\Delta \vdash \tau : k$, para algum $k$ e $\Delta$).

Além disso, optamos por definir o sistema de tipos de maneira que este
seja dirigido por sintaxe. Um sistema de tipos é dito ser dirigido por sintaxe se existe uma única regra aplicável a cada construção sintática da linguagem em questão. Um sistema de tipos
sem esta propriedade é útil para demonstração de resultados metateóricos, pois simplifica as demonstrações. Porém, ao usar um sistema de tipos dirigido por sintaxe, a correção e completude
do algoritmo de inferência de tipos, apresentado na seção \ref{inference}, é imediata, o que, em nossa opinião, compensa os detalhes adicionais necessários a outras propriedades.
O sistema de tipos é definido a seguir.

\[
\begin{array}{c}
  \fbox{$\Gamma \vdash e : \tau$}\\
  \\
  \begin{array}{c}
    \infer[_{(Var)}]{\Gamma \vdash x : [\overline{\alpha}\mapsto\overline{\beta}]\tau}
                    {\Gamma(x) = \forall \overline{\alpha}.\tau & \overline{\beta}\text{ são novas variáveis}} \\ \\

    \infer[_{(Abs)}]{\Gamma\vdash \lambda x. e : \tau' \to \tau}
                    {\Gamma , x : \tau' \vdash e : \tau} \\ \\

    \infer[_{(App)}]{\Gamma\vdash e\,e' : \tau}
                    {\Gamma \vdash e : \tau' \to \tau & \Gamma \vdash e' : \tau'} \\ \\

    \infer[_{(Let)}]{\Gamma\vdash \text{let $x = e'$ in }e : \tau}
                    {\Gamma \vdash e' : \tau' & \overline{\alpha} = fv(\tau') - fv(\Gamma) & \Gamma , x : \forall \overline{\alpha}.\tau' \vdash e : \tau}

  \end{array}
\end{array}
\]


\subsection{Inferência de Tipos}\label{inference}

O algoritmo de inferência de tipos é apresentado a seguir como um sistema de provas dirigido pela sintaxe para derivar julgamentos da
$\Gamma \vdash_I e : (\tau , S)$, em que $\Gamma$ é um contexto de tipos, $e$ uma expressão, $\tau$ um tipo simples e $S$ uma substituição.
Aparentemente, a definição do algoritmo de inferência é bem próxima do sistema de tipos porém, cabe ressaltar as seguintes diferenças importantes:
\begin{itemize}
  \item O julgamento para o algoritmo de inferência retorna, além do tipo calculado, uma substituição que é mantida durante o processo recursivo de inferência.
  \item As regras Abs e App utilizam novas variáveis para permitir o cálculo do tipo do parâmetro e de retorno de funções.
\end{itemize}

\[
\begin{array}{c}
  \fbox{$\Gamma \vdash_{I} e : (\tau,S)$}\\
  \\
  \begin{array}{c}
    \infer[_{(Var)}]{\Gamma \vdash_{I} x : ([\overline{\alpha}\mapsto\overline{\beta}]\tau, id)}
                    {\Gamma(x) = \forall \overline{\alpha}.\tau & \overline{\beta}\text{ são novas variáveis}} \\ \\

    \infer[_{(Abs)}]{\Gamma\vdash_{I} \lambda x. e : S(\alpha \to \tau)}
                    {\Gamma , x : \alpha \vdash_{I} e : (\tau , S) & \alpha \text{ é uma variável nova}} \\ \\

    \infer[_{(App)}]{\Gamma\vdash_{I} e\,e' : (S' \circ S_2 \circ S_1 (\alpha), S' \circ S_2 \circ S_1)}
                    {\Gamma \vdash_{I} e : (\tau_1 , S_1) & \Gamma \vdash_{I} e' : (\tau_2, S_2) & S' = mgu(\tau_1, \tau_2\to \alpha) & \alpha \text{ é uma variável nova}} \\ \\

    \infer[_{(Let)}]{\Gamma\vdash_{I} \text{let $x = e'$ in }e : (\tau,S)}
                    {\Gamma \vdash_{I} e' : (\tau',S') & \overline{\alpha} = fv(\tau') - fv(\Gamma) & S' (\Gamma , x : \forall \overline{\alpha}.\tau') \vdash_{I} e : (\tau , S)}

  \end{array}
\end{array}
\]
O algoritmo de inferência faz uso da função $mgu$ que, a partir de dois tipos, retorna o unificador\footnote{O unificador de dois tipos $\tau_1$ e $\tau_2$ é uma substituição $S$ tal que $S \tau_1 = S \tau_2$.} mais geral entre eles ou erro indicando que este não existe\cite{Robinson65}.


\subsection{Resultados Metateóricos}\label{metatheory}

Nesta seção enunciamos os principais resultados metateóricos dos formalismos desenvolvidos durante este projeto. Para cada teorema ou lema apresentaremos superficialmente as demonstrações
uma vez que estas serão formalizadas utilizando a linguagem Agda em trabalhos futuros.

A primeira propriedade é o determinismo da semântica que especifica que as regras de execução da linguagem garantem que programas sempre produzem o mesmo resultado.

\begin{theorem}[Determinismo da semântica]
Para todas as expressões $e$, $e_1$ e $e_2$, se $e \Rightarrow e_1$ e $e \Rightarrow e_2$ então $e_1 \equiv e_2$.
\end{theorem}
\begin{proof}
Por indução sobre a derivação de $e \Rightarrow e_1$ e análise de casos sobre a última regra utilizada na prova de $e \Rightarrow e_2$.
\end{proof}

Outra propriedade importante demonstrada é a de progresso, que especifica que toda expressão bem tipada é um valor ou é uma expressão que pode ser reduzida de acordo com as regras da semântica.

\begin{theorem}[Progresso]
Para toda expressão $e$ tal que $\Gamma \vdash e : \tau$, ou $e$ é um valor ou existe $e'$ tal que $e \Rightarrow e'$.
\end{theorem}
\begin{proof}
Por indução sobre a estrutura de $e$.
\end{proof}

A propriedade de preservação mostra que a semântica preserva os tipos, isto é, se uma expressão $e$ reduz para uma expressão $e'$, então $e$ possui o mesmo tipo de $e'$.

\begin{theorem}[Preservação]
Para toda expressão $e$ se $\Gamma \vdash e : \tau$ e $e\Rightarrow e'$ então $\Gamma \vdash e' : \tau$.
\end{theorem}
\begin{proof}
Indução sobre a derivação de  $\Gamma \vdash e : \tau$ e análise de casos sobre a última regra utilizada para concluir $e\Rightarrow e'$.
\end{proof}

\begin{corollary}[Correção do sistema de tipos]
Para toda expressão $e$ se $\Gamma \vdash e : \tau$, então $e$ ``não gera erros em tempo de execução''.
\end{corollary}
\begin{proof}
Consequência imediata dos teoremas 2 e 3.
\end{proof}

\begin{theorem}[Correção do algoritmo de inferência de tipos]
Para todo $\Gamma$, $e$, $\tau$ e $S$, se $\Gamma \vdash_I e : (\tau , S)$ então, $\Gamma \vdash e : \tau$.
\end{theorem}
\begin{proof}
Por indução sobre a derivação de $\Gamma \vdash_I e : (\tau , S)$.
\end{proof}

\begin{theorem}[Completude do algoritmo de inferência de tipos]
Para todo $\Gamma$, $e$, $\tau$, se $\Gamma \vdash e : \tau$ então existem $\tau'$, $S$ e $S'$ tais que $\Gamma\vdash_I e : (S \tau, S')$.
\end{theorem}
\begin{proof}
Por indução sobre $\Gamma \vdash e : \tau$.
\end{proof}

%%%%%%%%%%%%%%%%%%%%

\section{Resultados Obtidos}

Todo o código para implementação da parte de análise do compilador proposto está implementado na linguagem Haskell \cite{haskell2010} e está disponível on-line \cite{arduino-ml}.
No presente momento o bolsista está finalizando o gerador de código do compilador. O processo de geração de código utiliza a máquina abstrata categórica \cite{Cousineau87}, como linguagem intermediária
que é posteriormente traduzida em um programa C que, finalmente, é usado para geração de código executável utilizando o compilador AVR-GCC do arduíno.

Como resultados obtidos na execução deste projeto podemos destacar:

\begin{itemize}
  \item Formalização do núcleo de uma linguagem funcional para desenvolvimento de sistemas embarcados.
  \item Modelagem da formalização da linguagem utilizando Agda.
  \item Implementação da fase de análise desta linguagem, utilizando a linguagem Haskell.
\end{itemize}

A principal dificuldade encontrada para conclusão do compilador protótipo da linguagem encontra-se no fato de que o arduíno possui mémoria RAM limitada (2kb), o que impossibilita a utilização
de coletores de lixo para gerenciamento automático de memória necessário para implementação de recursos presentes em linguagens funcioanais como funções de ordem superior e aplicação parcial.
No presente momento, estamos desenvolvendo técnicas de otimização de código da máquina categórica abstrata com o intuito de reduzir o consumo de alocação de memória para execução de programas simples.

\section{Conclusões}

Neste trabalhos verificamos a viabilidade do uso de linguagens funcionais de alto nível para o desenvolvimento de sistemas embarcados. Obtivemos como resultado a formalização completa da linguagem
proposta e a implementação de um compilador protótipo para esta linguagem está em fase final. A principal dificuldade encontrada para a não conclusão deste projeto no prazo foram as dificuldades
encontradas para gerenciamento automático de memória para o Arduíno, pois a utilização de coletores de lixo não é viável devido ao consumo de memória e ciclos de processamento que estes causam
no mecanismo de execução dos programas.

Como possíveis trabalhos futuros pretendemos estudar a viabilidade do uso de um sistema de tipos baseado em lógica linear \cite{Girard95} para permitir um controle preciso sobre a alocação e
liberação de memória utilizada pelo mecanismo de execução da máquina categórica abstrata.

\bibliographystyle{plain}
\bibliography{bib}
\end{document}
