********************************************************************************
************ Paper BRT Brasília 
************ Synthetic Difference-in-Differences Estimator

cd "D:\Artigos\Paper BRT Brasília\Code and Data"
use "dados_agg_reg_completa.dta"

*********************************
* Create a numeric ID for the RAs
encode reg, gen(reg_id)
label list
/*
           1 Brasília
           2 Brazlândia
           3 Candangolândia
           4 Ceilândia
           5 Cruzeiro
           6 Gama
           7 Guará
           8 Lago Norte
           9 Lago Sul
          10 Núcleo Bandeirante
          11 Paranoá
          12 Planaltina
          13 Plano Piloto
          14 Recanto Das Emas
          15 Riacho Fundo
          16 Samambaia
          17 Santa Maria
          18 Sobradinho
          19 São Sebastião
          20 Taguatinga
*/
xtset reg_id aamm 

*********************************
* Keep a balanced panel 
keep if aamm >= 200901 & aamm <= 201712
bysort reg_id: gen n_obs = _N

tab	n_obs
/*
      n_obs |      Freq.     Percent        Cum.
------------+-----------------------------------
         24 |         24        1.28        1.28
         39 |         39        2.09        3.37
         70 |         70        3.74        7.11
         91 |         91        4.87       11.98
         96 |        288       15.40       27.38
         97 |      1,358       72.62      100.00
------------+-----------------------------------
      Total |      1,870      100.00

*/
keep if n_obs == 97

xtset reg_id aamm 
tab reg
/*
                reg |      Freq.     Percent        Cum.
--------------------+-----------------------------------
         Brazlândia |         97        7.14        7.14
          Ceilândia |         97        7.14       14.29
           Cruzeiro |         97        7.14       21.43
               Gama |         97        7.14       28.57
              Guará |         97        7.14       35.71
 Núcleo Bandeirante |         97        7.14       42.86
            Paranoá |         97        7.14       50.00
         Planaltina |         97        7.14       57.14
   Recanto Das Emas |         97        7.14       64.29
          Samambaia |         97        7.14       71.43
        Santa Maria |         97        7.14       78.57
         Sobradinho |         97        7.14       85.71
      São Sebastião |         97        7.14       92.86
         Taguatinga |         97        7.14      100.00
--------------------+-----------------------------------
              Total |      1,358      100.00

sort aamm
egen time_id = group(aamm)
xtset reg_id time_id
order reg_id time_id
*/

gen mes = mod(aamm,100)

*********************************
* Create dependent variables

/*
rend_bruto é a média da RA? ok
informal é o emprego? ok
Não achei a agregação da variável ocupado
*/

gen y_1 = ln(rend_bruto/horas_trab)

* BRT Effect 
gen BRT = 1 if aamm > 201406 & reg_id == 6 
replace BRT = 1 if aamm > 201406 & reg_id == 17 
replace BRT = 0 if BRT ==.
tab BRT
tab BRT reg_id

* Deixando apenas o mês 09
keep if mes ==9

xtset reg_id ano

*********************************
* SDID - Gama
sdid y_1 reg_id ano BRT if reg_id != 17, vce(placebo) seed(1213) reps(100)

sdid y_1 reg_id ano BRT if reg_id != 17, vce(placebo) seed(1213) reps(100) ///
graph g1on g2_opt(ylabel(3(0.5)4.5) ytitle("log (Hourly Wage)") scheme(stsj)) ///
g1_opt(xtitle("") scheme(stsj)) graph_export(sdid_gama, .png) 

{
sdid_event y_1 reg_id ano BRT if reg_id != 17, vce(placebo) brep(1000) placebo(all)

	mat res = e(H)
	svmat res
	mat list res

	drop in 1
	gen id = .

	* efeitos (0 a 3)
	replace id = _n - 1 if _n <= 4

	* placebos (-5 a -1)
	replace id = _n - 10 if _n >= 5
	keep res* id
	drop if res1 == .
	sort id
	
twoway(rarea res3 res4 id, lc(gs10) fc(gs11%50)) ///
(scatter res1 id, mc(blue) ms(d)) ///
, legend(off) ///
xlabel(-5(1)3) xscale(range(-5 3)) xtitle("Relative time to treatment change") ///
ytitle("log (Hourly Wage)") ///
yline(0, lc(red) lp(dash)) ///
xline(0, lc(black) lp(solid)) 

graph export "sdid_event_gama.png", replace
}


* SDID - Santa Maria
sdid y_1 reg_id ano BRT if reg_id != 6, vce(placebo) seed(1213) reps(100)

/*
sdid y_1 reg_id ano BRT if reg_id != 6 & reg_id != 13, vce(placebo) seed(1213) reps(100)

Se retirarmos também o plano piloto o resultado não muda
*/

sdid y_1 reg_id ano BRT if reg_id != 6, vce(placebo) seed(1213) reps(100) ///
graph g1on g2_opt(ylabel(3(0.5)4.5) ytitle("log (Hourly Wage)") scheme(stsj)) ///
g1_opt(xtitle("") scheme(stsj)) graph_export(sdid_sm, .png) 


{
sdid_event y_1 reg_id ano BRT if reg_id != 6, vce(placebo) brep(1000) placebo(all)

*sdid_event y_1 reg_id ano BRT if reg_id != 6 & reg_id != 13, vce(placebo) brep(1000) placebo(all)

	mat res = e(H)
	svmat res
	mat list res

	drop in 1
	gen id = .

	* efeitos (0 a 3)
	replace id = _n - 1 if _n <= 4

	* placebos (-5 a -1)
	replace id = _n - 10 if _n >= 5
	keep res* id
	drop if res1 == .
	sort id

gen ano = 2008 + _n
	
twoway(rarea res3 res4 ano, lc(gs10) fc(gs11%60) lwidth(none) legend(label(1 "95% CI"))) ///
(line res1 ano, lc(black) lp(dash) lw(vvthin) legend(label(2 ""))) ///
(scatter res1 ano, mcolor(black) msymbol(O) msize(small) legend(label(3 "Point estimate"))) ///
, legend(order(3 1) position(2009 1.5) ring(0) col(1) size(small)) ///
xlabel(2009(1)2017, labsize(vsmall)) xscale(range(2009 2017)) xtitle("year") ///
ytitle("log (Hourly Wage)") ///
yline(0, lc(black) lp(solid) lw(vthin)) ///
xline(2014, lc(black) lp(solid) lw(vthin)) ///
ylabel(, labsize(vsmall) glcolor(white) glwidth(thick)) ///
graphregion(fcolor(white) margin(0 1 0 1) lcolor(white)) ///
plotregion(margin(0 2 0 1)) xsize(4) ysize(4)
 

graph export "sdid_event_sm.png", replace 

* Gráfico

* Criar variáveis para pré e pós tratamento
rename id periodo
gen coef_pre_1 = res1 if periodo < 0
gen lim_inf_pre_1 = res3 if periodo < 0
gen lim_sup_pre_1 = res4 if periodo < 0

gen coef_pos_1 = res1 if periodo >= 0
gen lim_inf_pos_1 = res3 if periodo >= 0
gen lim_sup_pos_1 = res4 if periodo >= 0

twoway ///
    (rarea lim_sup_pre_1 lim_inf_pre_1 periodo if periodo < 0, color(eltblue) lcolor(eltblue) lwidth(none) legend(label(1 "Pré-tratamento"))) ///
    (scatter coef_pre_1 periodo if periodo < 0, mcolor(blue) msymbol(O) msize(small) legend(label(2 ""))) ///
    (rarea lim_sup_pos_1 lim_inf_pos_1 periodo if periodo >= 0, color(red%30) lcolor(red%30) lwidth(none) legend(label(3 "Pós-tratamento"))) ///
    (scatter coef_pos_1 periodo if periodo >= 0, mcolor(red) msymbol(O) msize(small) legend(label(4 ""))) ///
	(line coef_pre_1 periodo if periodo < 0, lcolor(blue) lpattern(dash) lwidth(vthin) legend(label(5 ""))) ///
	(line coef_pos_1 periodo if periodo >= 0, lcolor(red%30) lpattern(dash) lwidth(vthin) legend(label(6 ""))), ///
	xline(-0.5, lpattern(dash) lcolor(black)) ///
    yline(0, lpattern(dash) lcolor(black)) ///
    xscale(range(-5 (1) 3)) ///
    yscale(range(-1 2)) ///
    xtitle("year", height(5)) ///
    xlabel(-5(1)3, labsize(small)) ///
    ylabel(, labsize(small) glcolor(white) glwidth(thick)) ///
    ytitle("log (Hourly Wage)", height(5)) ///
    legend(order(1 3) ring(0) position(11) col(1)) ///
    graphregion(fcolor(white) margin(0 1 0 1) lcolor(white)) ///
    plotregion(margin(0 1 0 1))



}

*********************************

