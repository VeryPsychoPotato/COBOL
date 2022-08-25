Input: PGMID and PGM_KEY
Output: price for of each ISIN (use OCCURS), ISIN with max and min prices. 

Use CRUD module FIM044P to read PGM_PARAM value from FI.TFI_PGM_PARAM where PGMID = 'TESTPGM' and PGM_KEY = 'ISIN-LIST'.
When calling FIM044P set version in FIM044I to '01' and function 'V' for view/read.

PGM_PARAM consists of 4 ISINs that have separator '05'.
Loop thru each of them and fetch price (KRS_HDL) from VD.TVD_KURS_STAM_REAL by using direct SQL.

Same hint: you can always browse other programs to see how one or other module is being called (RTC Full Text Search). 
