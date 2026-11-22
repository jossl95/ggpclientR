`%+%` <- function(a, b) paste0(a, b)

# an FFS dataset as the portal ships it, with the quirks of the real files: CRLF lines, a text column (A), a stray quote
# where an umlaut was, * for overflow, $ in names, labels shared by two variables
FFS_SPS <- gsub("\n", "\r\n", r"---(DATA LIST FILE='C:\ISSA\SURVEY\USA\SPS\USA2.DAT' records=2
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
)---", fixed = TRUE)
FFS_DAT <- paste(c("23      000101" %+% "23" %+% "A0001" %+% "0.7035", "23      000102" %+% " 1" %+% "1",
                   "23      000201" %+% "23" %+% "A0002" %+% "1.2000", "23      000202" %+% "98" %+% "*"),
                 collapse = "\r\n")
