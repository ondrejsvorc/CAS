## Seminární projekt (Časové řady)

```
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

### Datová sada
- https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset
- byl použit pouze soubor `hour.csv` a přejmenován na `bike_sharing.csv`

#### Sloupce
- **instant** – ID záznamu (1, 2, 3, …)
- **dteday** – datum (YYYY-MM-DD)
- **season** – roční období (1 jaro, 2 léto, 3 podzim, 4 zima)
- **yr** – rok (0 a 1, kde 0 symbolizuje rok 2011 a 1 rok 2012)
- **mnth** – měsíc (1–12)
- **hr** – hodina (0–23)
- **holiday** – svátek (1 ano, 0 ne)
- **weekday** – den v týdnu (0 neděle … 6 sobota)
- **workingday** – pracovní den (1 ano, 0 ne)
- **weathersit** – počasí (1 jasno, 2 mlha/oblačno, 3 déšť/sníh lehký, 4 déšť/sníh silný)
- **temp** – teplota (0–1, normalizovaná škála)
- **atemp** – pocitová teplota (0–1, normalizovaná škála)
- **hum** – vlhkost (0–1, procenta)
- **windspeed** – vítr (0–1, normalizovaná škála)
- **casual** – počet neregistrovaných uživatelů
- **registered** – počet registrovaných uživatelů
- **cnt** – celkový počet jízd

#### Interpretace

Každý záznam symbolizuje souhrn všech jízd uskutečněných v dané hodině spolu s kontextem (datum, čas, den v týdnu, pracovní den či svátek, počasí, teplota, vlhkost, vítr, …).

První záznam:
```
1, 2011-01-01, 1, 0, 1, 0, 0, 6, 0, 1, 0.24, 0.288, 0.81, 0.000, 3, 13, 16
```

- **instant = 1** → první záznam
- **dteday = 2011-01-01** → datum 1. 1. 2011
- **season = 1** → zima
- **yr = 0** → 2011
- **mnth = 1** → leden
- **hr = 0** → hodina 00:00–00:59
- **holiday = 0** → není svátek
- **weekday = 6** → sobota
- **workingday = 0** → nepracovní den
- **weathersit = 1** → jasno
- **temp = 0.24** → normalizovaná teplota
- **atemp = 0.288** → normalizovaná pocitová teplota
- **hum = 0.81** → vlhkost 81 %
- **windspeed = 0.000** → bezvětří
- **casual = 3** → 3 neregistrovaní uživatelé
- **registered = 13** → 13 registrovaných uživatelů
- **cnt = 16** → celkem 16 jízd