# 4.4.2 $B<ALd%7%9%F%`$O$I$&F0$/$+(B

$B<ALd%7%9%F%`$O%Q%?!<%s%^%C%A%s%0$H%f%K%U%#%1!<%7%g%s$H$$$&Fs$D$NCf?4E*1i;;$N<~0O$K9=C[$9$k!#(B


## $B%Q%?!<%s%^%C%A%s%0(B

$B%G!<%?%j%9%H(B ((a b) c (a b))
$B%Q%?!<%sJQ?t(B?x$B$r(B(a b)$B$KB+G{(B
$B%Q%?!<%s(B(?x c ?x)$B$H%^%C%A(B


$B%G!<%?%j%9%H(B ((a b) c (a b))
$BB+G{(B
    - ?x$B$r(B(a b)
    - ?y$B$r(Bc
    - ?z$B$r(B(a b)
(?x ?y ?z)$B$K%^%C%A(B



$B%G!<%?%j%9%H(B ((a b) c (a b))
$BB+G{(B
    - ?x$B$r(Ba
    - ?y$B$r(Bb
((?x ?y) c (?x ?y))$B$K%^%C%A(B


### $BMQ8l(B

$B%G!<%?(B
(job (Bitddidle Ben) (computer wizard))

$B%Q%?!<%s(B
(job ?x (Computer Geek))

$B%U%l!<%`(B
- $B6u%U%l!<%`(B
- $B%Q%?!<%sJQ?t$NB+G{5,Dj$K$h$C$F3HD%$5$l$?%U%l!<%`(B
- $B%^%C%A<:GT(B


## $B%U%l!<%`$N%9%H%j!<%`(B

$BF~NO(B $B"*(B AND Match $B"*(B $B=PNO(B
$BF~NO(B $B"*(B OR Match $B"*(B $B=PNO(B


## $B9g@.<ALd(B

$B<BAu$N??$NH~$7$5$H$O!#(B

$B<ALdKh$K%U%l!<%`$,:n$j=P$5$l$k$N$G!"(Band$B$N>l9g$O:G0-<ALd$N?t$N;X?tE*$J?t$N%^%C%A$,I,MW$K$J$k(B

```
frame:
    $B6u(B
pattern:
    (and (can-do-job ?x (computer programmer trainee))
         (job ?person ?x))

# 1$B$D$a$N%Q%?!<%s$K%^%C%A$9$k%U%l!<%`$r:n@.(B

frame:
    (?x -> computer programmer)
    (?x -> computer trainee)
    (?x -> computer wizard)
pattern:
    (job ?person ?x)

# 2$B$D$a$N%Q%?!<%s$K%^%C%A$9$k%U%l!<%`$r:n@.(B

frame:
   (job (Bitdiddle Ben) (computer wizard))
   (job (Fact Cy D) (computer programmer))
   (job (Hacker Alyssa P) (computer programmer))
   (job (Reasoner Louis) (computer trainee))
```

lives-near$B$NI=L@$,;X?tE*$J?t$N%^%C%A$K$J$C$F$$$k5$$,$9$k(B 100^2

```
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

### not

```
(not (job ?x (computer programmer)))
```

$BF~NO%9%H%j!<%`$N3F%U%l!<%`$+$i(B(job ?x (computer programmer))$B$rK~$?$9J*$r3HD%%U%l!<%`$r:n@.!"F~NO%9%H%j!<%`$+$i$=$N3HD%$,B8:_$9$k%U%l!<%`$r=|5n$9$k!#(B


## $B%f%K%U%#%1!<%7%g%s(B


(?x a ?y)$B$H(B(?y ?z a)$B$N%f%K%U%!%$7k2L$O(B ?x => a, ?y => a, ?z => a


$B?dO@$9$k(B


# $BO@M}7?%W%m%0%i%_%s%0$O?t3XE*O@M}$+(B

## $BL58B%k!<%W(B

$BHs8zN($N6KC<$JNc(B

```
(assert! (married Minnie Mickey))

(assert! (rule (married ?x ?y)
               (married ?y ?x)))

(married Mickey ?who)
```

1. (married ?x ?y)$B$H(B(married Mickey ?who)$B$,%f%K%U%!%$$5$l$k(B
2. (married Mickey ?who)
3. (married ?who Mickey)
4. (married Minnie Mickey)$B$,=PNO(B
5. (married ?who Mickey)$B$,(Brule$B$KE83+$5$l$k(B $B"*(B $BL58B%k!<%W(B

## not

not$B$O%U%l!<%`$N%U%#%k%?$H$7$FF/$/(B

```
(and (supervisor ?x ?y)
     (not (job ?x (computer programmer))))
```
$B$H(B
```
(and (not (job ?x (computer programmer)))
     (supervisor ?x ?y))
```
$B$O0[$J$k7k2L$H$J$k!#(B

$B?tM}O@M}$N(Bnot$B$H$O0[$J$k(B


; $BLdBj(B4.64

outranked-by$B$rL58B$KI>2A$7$F$7$^$&$?$a(B


; $BLdBj(B4.65

$B4FFD<T$N4FFD<T$r=PNO$9$k(B

Louis -> Alyssa -> Ben
Alyssa -> Ben -> Oliver
Cy D -> Ben -> Oliver
Lem -> Ben -> Oliver
Robert -> Eben -> Oliver



