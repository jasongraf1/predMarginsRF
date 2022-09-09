#' American English genitive constructions
#'
#' Annotated dataset of 5098 tokens of English s-genitive (e.g. "the children's
#' voices") and of-genitive (e.g."the voices of the children") constructions
#' extracted from 5 components an of the Brown and Frown corpora of published
#' written American English.
#'
#' @format A data frame with 5098 rows and 26 variables:
#' \describe{
#'  \item{Token}{Unique ID made op of the corpus, text file, and line in the text where the token was found}
#'  \item{Corpus}{The corpus from which the token was taken}
#'  \item{Text}{The text from which the token was taken}
#'  \item{Line_Number}{The line (sentence) in the text file where the token was found}
#'  \item{Genre}{The genre from which the token was taken}
#'  \item{Type}{s-genitive or of-genitive}
#'  \item{Whole_Genitive}{The entire genitive construction}
#'  \item{Possessor}{The full possessor phrase, e.g. "the children" in "the children's voices"}
#'  \item{Possessor_Head}{The head of the possessor phrase, e.g. "children" in "the children's voices". Complex heads were included as multi-word heads, e.g. "Republican Party" for "the apparent intention of the Republican Party", or "Declaration of Independence" for "the principles of the Declaration of Independence"}
#'  \item{Possessor_NP_Type}{Whether the possessor is represented by a 'proper' or 'common' noun}
#'  \item{Final_Sibilant}{Does the possessor end in a sibilant sound (Y/N)?}
  #'  \item{Possessor_Animacy5}{5-level animacy classification of the possessor}{'animate', 'collective', 'locative', 'temporal', 'inanimate'}
#'  \item{Possessor_Animacy3}{3-level animacy classification of the possessor}{'animate', 'collective', 'inanimate' (collapsing 'locative', 'temporal', 'inanimate')}
#'  \item{Possessor_Animacy2}{2-level animacy classification of the possessor}{'animate', 'inanimate' (collapsing 'collective', 'locative', 'temporal', 'inanimate')}
#'  \item{Possessor_Givenness}{Has the possessor been referred to in the 100 words preceding the token. If yes, it is coded as 'given', otherwise 'new'}
#'  \item{Possessum}{The full possessum phrase, e.g. "voices" in "the children's voices" or "the apparent intention" in "the apparent intention of the Republican Party"}
#'  \item{Possessum_Head}{The head of the possessum phrase}
#'  \item{Possessum_Animacy2}{2-level animacy classification of the possessum}{'animate', 'inanimate'}
#'  \item{Semantic_Relation}{5-level classification of the relation between the possessor and possessum}{body-part ('BOD'), kinship ('KIN'), ownership ('OWN'), part-whole ('PRT'), other ('OTH')}
#'  \item{Prototypical_Semantics}{Is the semantic relation a prototypical one (BOD, KIN, OWN, PRT) or not (OTH)? See Rosenbach (2002:58-65).}
#'  \item{Preceding_s_genitive}{Is the genitive construction immediately preceding this }
#'  \item{Possessor_Thematicity}{The frequency of the head noun in the text, divided by the total number of words in the text and log10 transformed}
#'  \item{Possessor_Length}{The number of orthographic words in the possessor phrase}
#'  \item{Possessum_Length}{The number of orthographic words in the possessum phrase}
#'  \item{Type_Token_Ratio}{The local type-token ratio of the closest 100 words surrounding the token (Hinrish & Szmrecsanyi 2007)}
#'  \item{Time}{The time at which the corpus data was collected (1960s, 1990s)}
#'   ...
#' }
#' @source \url{https://doi.org/10.18710/R7HM8J}
"written_genitives"
