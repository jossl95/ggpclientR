DATA LIST FILE='C:\ISSA\SURVEY\USA\SPS\USA2.DAT' records=2
/1
COUNTRY  15-16
HHNUM    17-21(A)
WEIGHT   22-27
/2
V004$01  15-16
V005$01  17-17
.
VARIABLE LABELS
 COUNTRY  "ISO country code"
/HHNUM    "Household identification no."
/WEIGHT   "Caseweight in US$"
/V004$01  "Born in Nieder"sterreich"
.
VALUE LABELS
 COUNTRY
    23 "United States"
/V004$01
 V005$01
     1 "One"
    98 "Missing value"
.
MISSING VALUE
 V004$01  (98)
.
EXECUTE
.
