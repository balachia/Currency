clear
set more off

capture: log close
log using ~/2YP/writing/smcl/clogits, replace

cd /archive/gsb/vashevko/Currensee/dta

use adopts

describe

su



clogit badopt ntotal_alt if cp == "EURUSD", group(dlapse) difficult showstep

clogit badopt ntotal_alt if cp == "USDCZK", group(dlapse) difficult showstep

clogit badopt ntotal_alt, group(dlapse) difficult showstep

log close

