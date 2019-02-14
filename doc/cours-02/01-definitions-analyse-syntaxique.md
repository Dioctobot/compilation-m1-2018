# Analyse syntaxique, définitions

## Grammaires hors-contextes

N : ensemble des symboles non terminaux
T : ensemble de symboles terminaux
R : ensemble de règles (de paires) formées d'un symbole non terminal et
    d'un mot potentiellement vide de symboles terminaux et non terminaux
S : symbole d'entrée qui est un non terminal

Exemple (la grammaire de Marthe)
N = { phrase, expression, term, factor }
T = { SUM, INT(x), VAR(x), PLUS, STAR, LPAREN, RPAREN, COMMA }

phrase ::= expression EOF

expression ::= term PLUS expression
    | term

    term ::=
      factor STAR term
    | factor

    factor ::=
      INT(x)
    | VAR(x)
    | SUM LPAREN VAR COMMA expression COMMA expression COMMA expression RPAREN
    | LPAREN expression RPAREN

## Dérivation

phrase                       ->
expression               EOF ->
    |
    | par la première règle des règles d'expression
    v
term PLUS expression     EOF ->
    |
   ...
    |
    v
INT(1) PLUS INT(2) EOF

## Arbre de production

Voir le dessin au tableau

## Grammaire ambigue

Une grammaire est ambigue si elle associe plusieurs arbres de
production à un unique mot.

## Arbre de syntaxe concrète

C'est un arbre de production dont on a oublié les numéros
des règles utilisées.

## Arbre de syntaxe abtraite

C'est un arbre de production où on a fusionné les non terminaux utiles
uniquement à la suppression des ambiguités de la grammaire.



