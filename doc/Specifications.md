Cahier des charges pour l'outil de modification des codes
=========================================================

Document rédigé par Sébastien Riette (sebastien.riette at meteo.fr) le 8
décembre 2022.
Mise à jour du 28 mars 2023: utilisation d'un parser xml externe autorisé,
ajout de fonctionnalités pour permettre la création automatique d'enrobages
autour de routines existantes (cas des drivers compatibles python).

I - Contexte et contraintes
===========================

I.1 Contexte
------------

Dans le cadre de l'adaptation des codes aux architectures accélérées
(GPU), il est nécessaire d'adapter le code source (modification de
l'organisation du code, réécriture de certaines parties) et de
l'instrumenter (c'est-à-dire que des directives de compilation sont
ajoutées au code).

Pour Méso-NH, des modifications sont introduites manuellement dans le
code pour le modifier et l'instrumenter.

Pour ARPEGE/AROME, ce travail est fait en deux temps : le code est tout
d'abord modifié manuellement pour le standardiser (étape 1) ; puis un
outil de transformation automatique du code (modifications dites
source-to-source) est appliqué pour produire un code compilable (étape
2) sur des machines scalaires (CPU), vectorielles ou accélérées (GPU).
Les outils de transformation sont développés par CNRM/GMAP et par le
CEP.

Concernant la physique du modèle AROME, les codes sont communs entre
Méso-Nh et AROME et sont déposés sur github
(<https://github.com/UMR-CNRM/PHYEX>). Ce code doit pouvoir répondre aux
besoins pour AROME (c'est-à-dire être compatibles avec les attentes de
l'étape 2) et aux besoins pour Méso-NH (code modifié et instrumenté).

Il est donc nécessaire de développer un outil de transformation du code
pour pouvoir produire la version AROME ou la version Méso-NH à partir du
code commun contenu dans PHYEX.

De plus, il est prévu de pouvoir appeler les différentes paramétrisations
physiques depuis un code python. Pour cela, des enrobages doivent être
écrits pour rendre compatibles avec python les arguments des routines
en fortran. Il serait souhaitable de pouvoir créer automatiquement ces
enrobages.

I.2 Existant
------------

Aujourd'hui des outils ont été développés (répertoire tools du dépôt
PHYEX) mais sont incomplets (toutes les fonctionnalités attendues ne
sont pas codées), éclatés (plusieurs outils dans des langages
différents), partiellement déficients (un script a été codé pour
vérifier le travail d'un autre) et difficilement maintenables (plusieurs
langages, utilisation d'un outil de bas niveau trop complet pour nos
besoins).

Ce premier développement (dans l'urgence) nous permet tout de même
d'identifier assez clairement ce que doit pouvoir faire l'outil et les
contraintes à respecter.

I.3 Contraintes
---------------

Voici une liste de contraintes :

-   Développement en python pur mis à part la possibilité d'utiliser une
    librairie externe pour transformer le code fortran en fichier xml.
    En particulier, il est possible d'utiliser la librairie
    [fxtran](https://github.com/pmarguinaud/fxtran).
-   Dans la mesure du possible, pas d'utilisation de modules non
    standards
-   Si possible un développement « standalone », c'est-à-dire un seul
    fichier, sans installation. Cet objectif initial sera certainement
    non tenable avec l'ajout de fonctionnalités à l'outil.
-   Utilisation soit en python (importation de l'outil et appel d'une
    fonction ou utilisation d'une classe) ou en ligne de commande
-   Licence : l'outil sera intégré au dépôt PHYEX qui est sous licence
    CeCILL-C

II - Fonctionnalités
====================

La fonctionnalité [II.7](#ii7-expansion-de-boucles) est actuellement réalisée par nos
outils. Nous souhaitons réécrire ces outils pour les raisons évoquées
plus haut mais cette réécriture est moins urgente que le développement
de la fonctionnalité [II.6](#ii6-allocateur) qui n'est actuellement pas
couverte par nos outils.

Certaines fonctionnalités marquées avec \*\*\* sont à traiter dans un deuxième
temps. Elles nous sont nécessaires mais sont soit moins urgentes, soit
réalisables assez facilement en dehors de l'outil.

Les options écrites en **gras** sont des propositions de noms d'options pour
l'outil appelable en ligne de commandes. Ces options (nom et argument)
peuvent être adaptées par le développeur.

De même, les exemples donnent le résultat attendu mais le résultat
réellement obtenu peut différer tant que les codes sont équivalents pour
le compilateur.

II.1 Généralités
----------------

Le code source à manipuler est écrit en FORTRAN. Les exemples fournis
essaient de prendre en compte tous les cas qui peuvent être rencontrés
dans nos modèles. Il ne devrait donc pas être nécessaire de connaître
les spécifications FORTRAN. Attention tout de même au fait que
minuscules et majuscules ne sont pas différenciées en FORTRAN.

De plus, les exemples fournis ne sont pas trop compliqués mais les codes
réels peuvent l'être davantage. En particulier, en FORTRAN, une ligne
peut être coupée n'importe où en introduisant un '&' et continuer à la
ligne suivante. Il faut que le code fourni soit robuste par rapport à
cela.

L'outil sera appelé sur un fichier unique et le transforme sur place
(l'ancienne version est effacée et remplacée par la nouvelle).

Si possible\*\*\*, l'outil pourra prendre en entrée un répertoire et
intégrera, dans ce cas, une boucle pour parcourir tous les fichiers
présents dans le répertoire et ses sous-répertoires.

Des exemples de code sont donnés. Tous ces exemples sont compilables
(sauf les différentes versions de mode\_compute\_updraft qui ont des
dépendances). La compilation peut au besoin être vérifiée avec la
commande gfortran -c \<nom du fichier\>.

Le code doit être commenté en anglais et le guide d'utilisation
doit également être rédigé en anglais.

Certaines des fonctionnalités décrites ci-dessous peuvent être
difficieles à interprêter dans le cas où plusieurs routines
seraient présentes dans le code source. Il sera peut-être
nécessaire d'introduire un argument général du type **\--only=nom**
pour restreindre l'application à la routine portant ce nom.

II.2 Changement de nom\*\*\*
----------------------------

Si l'option **\--renameFf** est passée, l'extension du nom du fichier est
mis en minuscules (.F90 devient .f90, .F devient .f). Avec l'option
**\--renamefF**, l'inverse est fait.

Explications : Méso-NH ne respecte pas le standard qui veut que les
fichiers avec une extension en majuscules doivent être traités par le
préprocesseur alors que ceux avec une extension en minuscules ne doivent
pas l'être.

II.3 Vérification de la présence de « IMPLICIT NONE »\*\*\*
-----------------------------------------------------------

Toutes les SUBROUTINEs devraient comporter l'instruction « IMPLICIT
NONE ». Ce n'est pas une obligation de la norme FORTRAN mais on souhaite
que l'outil le vérifie.

Le contrôle peut être activé par l'option
**\--checkIMPLICIT=None\|Warn\|Err** avec None comme valeur par défaut.
L'outil émet un warning (logging.warning) avec l'option Warn et émet une
erreur (logging.error & raise) avec l'option Err si l'instruction
« IMPLICIT NONE » n'est pas trouvée.

L'instruction est commentée dans exemple\_check\_KO.F90 et est correcte
dans exemple\_check\_OK.F90.

Il n'est pas attendu une correction automatique, juste le contrôle.

II.4 Vérification de la présence de l'attribut INTENT\*\*\*
-----------------------------------------------------------

Tous les arguments reçus par la routine devraient être déclarés avec
l'attribut INTENT (ce n'est pas une obligation dans les spécifications
FORTRAN mais on souhaite que l'outil le vérifie).

Le contrôle peut être activé par l'option **\--checkINTENT=None\|Warn\|Err**
avec None comme valeur par défaut. L'outil émet un warning
(logging.warning) avec l'option Warn et émet une erreur (logging.error &
raise) avec l'option Err si un attribut INTENT est manquant.

L'argument ARG2 est incorrect dans [exemple\_check\_KO.F90](../examples/exemple_check_KO.F90) et est correct
dans [exemple\_check\_OK.F90](../examples/exemple_check_OK.F90). À noter que les arguments non présents dans
la déclaration de l'interface n'ont pas d'INTENT (exemple de l'argument
Z1).

Il n'est pas attendu une correction automatique (qui n'est pas
possible), juste le contrôle.

II-5 Ajout d'un ou plusieurs arguments ou module
------------------------------------------------

L'option **\--addARG=nom\#declaration** ajoute l'argument « nom » à
l'interface de la SUBROUTINE et le déclare à l'aide de la chaîne
« declaration ». L'option est ajoutée autant de fois que l'on veut
ajouter d'arguments.

L'option **\--addUSE=statement** ajoute « statement » juste après
l'interface de la SUBROUTINE.

Le fichier [exemple\_add\_apres.F90](../examples/exemple_add_apres.F90) serait généré à partir du fichier
[exemple\_add\_avant.F90](../examples/exemple_add_avant.F90) en utilisant les options
--addARG=\"PBUF\#TYPE(BUF\_t), INTENT(IN) :: PBUF\" --addUSE=\"USE
MODD\_BUF, ONLY : BUF\_t\".

II.6 Allocateur
---------------

Les tableaux locaux sont déclarés de manière statique (tableaux dit
automatiques) dans le code. L'option
**\--allocARRAY=declaration\#allocation** doit modifier ces déclarations
pour utiliser une allocation dite dynamique. La déclaration de la
variable est modifiée en utilisant le modèle « declaration » et une
instruction est ajoutée après les déclarations suivant le modèle
« allocation ». L'exemple fourni doit éclaircir la transformation
attendue ; cet exemple contient également un certain nombre de pièges à
éviter.

Pour plus de simplicité et de lisibilité, les déclarations obtenues sont
écrites sur des lignes séparées même si elles sont regroupées en une
seule instruction dans le fichier de départ.

Les modèles contiennent des éléments à remplacer pour obtenir
l'instruction exacte. Les éléments possibles sont :

-   {shape} à remplacer par les dimensions (ex : « NI, NJ »)
-   {doubledotshape} à remplacer par des « : » (ex : « :, : »)
-   {name} à remplacer par le nom du tableau
-   {kind} à remplacer par le type (ex : « REAL »)

Le fichier [exemple\_alloc\_apres.F90](../examples/exemple_alloc_apres.F90) serait généré à partir du fichier
[exemple\_alloc\_avant.F90](../examples/exemple_alloc_avant.F90) à l'aide de l'option\--alloc=\"{kind},
DIMENSION({doubledotshape}), ALLOCATABLE ::
{name}\#ALLOCATE({name}({shape}))\"

Note : l'allocateur ne doit être utilisé que pour les tableaux locaux.
Lorsque la fonctionnalité [II.4](#ii4-vérification-de-la-présence-de-lattribut-intent) sera développée,
l'utilisation de l'option \--allocARRAY pourra soit reposer sur le
développement de la fonctionnalité [II.4](#ii4-vérification-de-la-présence-de-lattribut-intent) pour écarter les
arguments d'appel, soit imposer \--checkINTENT=Err et écarter les
tableaux déclarés avec l'option INTENT.

II.7 Expansion de boucles
-------------------------

Le code source est instrumenté, c'est-à-dire qu'il contient des
instructions qui prennent la forme de commentaires (elles commencent par
« ! ») et qui sont interprétables par un outil pour transformer le code
avant qu'il ne soit fourni au compilateur.

Le code de la physique contient des instructions permettant de convertir
des notations en « array-syntax » en boucles explicites (DO ou DO
CONCURRENT). Ces instructions sont composées d'une balise de début et
d'une de fin encadrant le bloc de code à transformer.

Ces transformations seront contrôlées par l'option
**\--expand=None\|Do\|DoConcurrent**. Avec None (valeur par défaut) aucune
conversion n'est faite. Avec Do, les blocs sont convertis en boucles
« DO ». Avec DoConcurrent, les blocs sont convertis en boucles « DO
CONCURRENT ». Les directives sont supprimées après transformation.

Pour des raisons historiques, ces balises peuvent prendre le nom
« expand\_array » ou « expand\_where ». Il n'y a pas de distinction à
faire entre ces deux noms. Si la balise de début est
« !\$mnh\_expand\_where(\...) », la balise de fin est
« !\$mnh\_end\_expand\_where(\...) ». Si la balise de début est
« !\$mnh\_expand\_array(\...) », la balise de fin est
« !\$mnh\_end\_expand\_array(\...) ».

Les balises prennent en argument la variable et les bornes à utiliser
pour effectuer les boucles DO.

La syntaxe exacte et le code généré sont décrits à travers l'exemple du
fichier [exemple\_expand\_avant.F90](../examples/exemple_expand_avant.F90) qui doit donner
[exemple\_expand\_apres\_DO.F90](../examples/exemple_expand_apres_DO.F90) ou
[exemple\_expand\_apres\_DOCONCURRENT.F90](../examples/exemple_expand_apres_DOCONCURRENT.F90)
suivant l'option choisie. Le
fichier [mode\_compute\_updraft.F90](../examples/mode_compute_updraft.F90)
est un fichier réellement utilisé dans AROME, il doit être transformé en
[mode\_compute\_updraft\_DO.F90](../examples/mode_compute_updraft_DO.F90) ou
[mode\_compute\_updraft\_DOCONCURRENT.F90](../examples/mode_compute_updraft_DOCONCURRENT.F90).

L'instrumentation a été réalisée à la main et de nombreux développeurs
vont dans l'avenir modifier le code source. Pour ces raisons, il n'est
pas exclu que des directives soient incorrectes. Dans les cas qui
suivent, l'outil doit refuser de faire la transformation (logging.error
& raise) :

-   la balise de début et la balise de fin doivent porter des noms
    compatibles (soit toutes les deux « expand\_array », soit toutes les
    deux « expand\_where »). L'outil doit refuser de transformer
    [exemple\_expand\_KO1.F90](../examples/exemple_expand_KO1.F90).
-   les variables et indices pour les boucles doivent être identiques
    pour la balise de début et la balise de fin. L'outil doit refuser de
    transformer [exemple\_expand\_KO2.F90](../examples/exemple_expand_KO2.F90).
-   le code entre les deux balises ne peut être constitué que
    d'affectations sur des tableaux, d'instructions
    « WHERE/ELSEWHERE/ENDWHERE » ou de tests « IF/ELSEIF/ELSE/ENDIF ».
    Attention on peut également ajouter un espace après les ELSE dans
    ELSEWHERE et ELSEIF et après les END dans ENDWHERE et ENDIF. L'outil
    doit refuser de transformer
    [exemple\_expand\_KO3.F90](../examples/exemple_expand_KO3.F90).
-   le nombre de dimensions des tableaux doit correspondre au nombre
    d'indice déclarés dans la directive. L'outil doit refuser de
    transformer [exemple\_expand\_KO4.F90](../examples/exemple_expand_KO4.F90).
-   lorsque les indices sont définis explicitement dans les boucles, ils
    doivent correspondre aux bornes de la directives. L'outil doit
    refuser de transformer [exemple\_expand\_KO5.F90](../examples/exemple_expand_KO5.F90).

Note : pour vérifier que les affectations sont réalisées sur des
tableaux, on part du principe que le code avant transformation donne les
résultats attendus (on affecte un tableau à un tableau, un scalaire à un
tableau ou un scalaire à un scalaire). Le seul cas à exclure est
l'affectation d'un scalaire à un scalaire. Pour détecter ce cas il n'est
nécessaire que de vérifier le membre de gauche car l'affectation d'un
tableau à un scalaire serait rejetée par le compilateur.

Pour faciliter l'implémentation, on peut considérer que tous les
tableaux sont écrits avec des parenthèses à la suite. Ainsi « A=B+A »
serait reconnu comme une opération scalaire et serait interdit alors que
« A(:)=B(:)+A(:) » serait reconnu comme une opération en
« syntax-array » et serait alors acceptée et transformée.

Optionnel : si les variables utilisées comme indices de boucles dans les
directives ne sont pas déclarées, l'outil pourrait les déclarer avec le
type INTEGER.

II.8 Suppression des directives openACC\*\*\*
---------------------------------------------

Si l'option **\--noACC** est positionnée, toutes les directives openACC
seront supprimées. Ces directives commencent par «!\$ » sur les deux
premiers caractères de la ligne et ces caractères sont suivis de la
chaîne « acc ». Entre les deux parties peuvent se trouver des espaces.

II.6 Suppression d'arguments\*\*\*
----------------------------------

L'option **\--rmARG=nom** supprime l'argument « nom » de l'interface d'appel
et supprime également la déclaration de cet argument.

II.7 Renommage\*\*\*
--------------------

L'option **\--rename=avant\#apres** renomme l'identificateur « avant »
en « après ». L'identificateur peut être le nom d'un module, d'un arguement
de l'interface, d'une routine ou fonction, d'une variable locale ou d'une
variable de module.

II.8 Ajout de variable locale\*\*\*
-----------------------------------

L'option **\--addVAR=declaration** ajoute la ligne « declaration »
après les déclarations des arguments de l'interface et avant la
première ligne exécutable.

II.9 Ajout de code\*\*\*
------------------------

L'option **\--addSRCbegin=source** ajoute le code « source »
juste après les déclarations.

L'option **\--addSRCend=source** ajoute le code « source »
à la fin.

II.10 Évaluation de conditions\*\*\*
------------------------------------

L'option **\--evalIF=nom\#valeur** supprime les portions de code
qui ne seront par parcourrues connaissant la valeur « valeur »
de la variable « nom ». L'argument peut être répété plusieurs
fois pour fournir les valeurs de plusieurs variables.

