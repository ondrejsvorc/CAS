## Time Series

Seminar project for the course *Time Series (Časové řady)*.

Dataset: https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset

```
Zadání práce je "Hledání optimálního modelu pro zvolenou časovou řadu". 
Zpracovávaná data:
	i) data budou obsahovat více potenciálně závislých řad 
	ii) zvolíte si jednu řadu jako hlavní
	iii) hlavní řada by měla mít evidentní sezónní složku a optimálně i trendu
	
Řešení by mělo obsahovat:
	i) grafické zobrazení řady + stručný komentář
	ii) dekompozice + identifikace trendu pomocí vyhlazení řady (klouzavé průměry či exponenciální vyrovnání)
	iii) hledání optimálního modelu pro samotnou řadu (funkční zápis trendu + sezónnost)
	iv) hledání optimálního modelu typu SARIMA
	v) hledání závislostí na jiných řadách (identifikace řad, které mohou mít vliv pomoci kroskorelační funkce + nalezení "zpoždění závislosti")
	vi) hledání optimálního modelu kombinujícího závislost na ostatních řadách a řešení autokorelace
	vi) u nalezených modelů v krocích iii), iv) a vi) zkontrolujte, zda jsou splněny předpoklady regresních modelů, tj. především nezávislost residuí (stačí přes autokorelační funkci) 
	vii) u vhodných modelů predikujte 10 budoucích pozorování, predikce i s intervaly spolehlivosti vykreslete a výsledky komentujte (který model Vám přijde pro predikci nejlepší)
	viii) porovnání jednotlivých modelů a závěr
U každého kroku by mělo být uvedeno, jakou metodu používáte a proč, co Vám z ní vyšlo a jak vypadá "optimální model" v daném kroku.
```
