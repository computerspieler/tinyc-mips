# Compilateur
## Fonctionnement
Le compilateur prend un argument obligatoire, l'architecture,
et deux arguments optionnels, le fichier d'entrée et de sortie,
comme suit :
`compiler [Architecture] [Input file] [Output file]`

Si le fichier d'entrée n'est pas spécifié, alors stdin servira
d'entrée.
Si le fichier de sortie n'est pas spécifié, alors stdout servira
de sortie.
A noter que pour envoyer la sortie dans un fichier, il faut que
l'entrée soit aussi un ficher.

## Varargs
Je gère les varargs de manière différentes que les standards C.
Pour cela, il existe un identifiant spécial `__varargs_start` de type
`void*` qui contient l'addresse de la première valeur après les
arguments obligatoires.

## Bugs connu
- Le code suivant n'est pas censé être compilable, mais l'est ici :
```C
if(1)
	int x;
```
Cependant, x n'existe que dans le if, donc ça n'est pas un grand problème

- Les chaines de caractères fonctionnent comme des `char*` en mémoire
(i.e. un octet par caractère) mais sont considérés de type `void*`
