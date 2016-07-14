clear
set more off

cd ~/Data/forex/dta

use short-adopts

gen ntalt0 = ntotal_alt == 0
gen ntalt = ntotal_alt
gen ntalt2 = ntalt ^ 2
gen npalt = npos_alt
gen npalt2 = npalt ^ 2


