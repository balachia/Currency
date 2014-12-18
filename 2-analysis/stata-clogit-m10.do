clear
set more off

capture: log close
log using "~/2YP/writing/smcl/clogits-m10.smcl", replace

cd /archive/gsb/vashevko/Currensee/dta

use "adopts-m10.dta"

describe

su

gen ntotal_alt_0 = ntotal_alt == 0

levelsof cp, local(cps)

* clogit badopt ntotal_alt ntotal_alt_0 if cp == "EURUSD", group(dlapse) difficult showstep

* clogit badopt ntotal_alt ntotal_alt_0 if cp == "USDCZK", group(dlapse) difficult showstep

* clogit badopt ntotal_alt ntotal_alt_0, group(dlapse) difficult showstep
* clogit badopt ntotal_alt npos_alt ntotal_alt_0, group(dlapse) difficult showstep

foreach ccp in `cps' {
    di "`ccp'"
    capture noisily: clogit badopt ntotal_alt ntotal_alt_0 if cp == "`ccp'", group(dlapse) difficult showstep
    capture noisily: clogit badopt ntotal_alt npos_alt ntotal_alt_0 if cp == "`ccp'", group(dlapse) difficult showstep
}

log close

