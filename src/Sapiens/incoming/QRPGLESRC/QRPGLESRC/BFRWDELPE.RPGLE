     H OPTION(*NODEBUGIO: *SRCSTMT)
     H DFTACTGRP(*NO) ACTGRP(*caller)
     hdatedit(*ymd) datfmt(*ymd)

     fWDELP     if   f  494        disk                                          Input file
     fAFTWDELPEOo    e             printer oflind(*in71)                         Query report

      ** Constants
      *************************************************
     dcnTITLE          c                   const('Query Conversion for')         Title
     dcnTYPE           c                   const('Old File')                     File (new/old)
     dcnSUBT           c                   const(' - Estimated')                 Sub type

      ** Initialization work area
      *************************************************
     dWork_Inz         ds                  inz                                   Initialization
     d  xxCCYYMMDD                    8s 0                                         Date is CCYYMMDD
     d    xxCCYY                      4a   overlay(xxCCYYMMDD:1)                     Century, year
     d    xxMM                        2a   overlay(xxCCYYMMDD:5)                     Month
     d    xxDD                        2a   overlay(xxCCYYMMDD:7)                     Day
     d  xxEDATE                      10a                                           Edited date
     d  xxHRMNSC                      6s 0                                         Time is HHMMSS
     d    xxHR                        2a   overlay(xxHRMNSC:1)                       Hour
     d    xxMN                        2a   overlay(xxHRMNSC:3)                       Minute
     d    xxSC                        2a   overlay(xxHRMNSC:5)                       Second
     d  xxETIME                       8a                                           Edited time

      ** Work/Storage area
      *************************************************
     dWork_Stor        ds                  inz                                   Work
     d  IX                            5p 0                                         Index
     d  xxCOUNT                       9p 0                                         Record counter
     d  xxPRINT                       9p 0                                         Print counter

      ** EMPLOYER DETAIL FILE
      *************************************************
     iWDELP     ns
     i                             P    1    2 0ELCO#                            COMPANY NUMBER
     i                             P    3    4 0ELFND                            FUND NUMBER
     i                             P    5    6 0ELFYR                            FUND CENTURY AN
     i                             P    7    9 0ELEMP#                           EMPLOYER NUMBER
     i                             P   10   11 0ELDIV                            DIVISION
     i                             P   12   17 2ELPRTD                           EXPENSE CONSTAN
     i                             P   18   22 2ELOUTS                           OUTSTANDING RES
     i                             P   23   28 2ELTCLP                           TOTAL CLAIMS PA
     i                             P   29   32 0ELT#CL                           TOTAL # CLAIMS
     i                             P   33   36 0ELTCLC                           TOTAL # OF CLOS
     i                             P   37   42 2ELANPR                           EST ANNUAL PREM
     i                             P   43   48 2ELMAPR                           EST MANUAL PREM
     i                             P   49   54 2ELSTPR                           EST STANDARD PR
     i                             P   55   56 0ELBSCD                           BILLING SCHEDUL
     i                             P   57   59 2ELASP1                           CONTRACTORS ADJ
     i                             P   60   62 2ELASP2                           DEVIATION
     i                             P   63   65 2ELASP3                           DISCOUNT
     i                             P   66   67 2ELEXM1                           EXPERIENCE MODI
     i                                 68   73 0ELEMD1                           EXP MOD EFFECT
     i                             P   74   75 2ELEXM2                           EXPERIENCE MODI
     i                                 76   81 0ELEMD2                           EXP MOD EFFECT
     i                             P   82   83 2ELEXM3                           EXPERIENCE MODI
     i                                 84   89 0ELEMD3                           EXP MOD EFFECT
     i                                 90   95 0ELPFDT                           POLICY FROM DAT
     i                                 96  101 0ELPTDT                           POLICY TO DATE
     i                                102  102  ELSTAT                           STATUS
     i                                103  112  ELUSER                           USER
     i                                113  122  ELPGM                            PROGRAM
     i                                123  128 0ELTIME                           TIME
     i                                129  134 0ELDT                             DATE MAINTAINED
     i                             P  135  139 0ELBUL1                           BATCH UPDATE LO
     i                             P  140  145 2ELEDC1                           EST DISCOUNT #
     i                             P  146  151 2ELEDC2                           EST DISCOUNT #
     i                             P  152  157 2ELEDC3                           EST DISCOUNT #
     i                             P  158  163 2ELADC1                           AUD DISCOUNT #
     i                             P  164  169 2ELADC2                           AUD DISCOUNT #
     i                             P  170  175 2ELADC3                           AUD DISCOUNT #
     i                             P  176  181 2ELAAPR                           AUDIT ANNUAL PR
     i                             P  182  187 2ELAMPR                           AUDIT MANUAL PR
     i                             P  188  193 2ELASPR                           AUDIT STANDARD
     i                             P  194  199 2ELEXP1                           EST EXP PREM #1
     i                             P  200  205 2ELEXP2                           EST EXP PREM #2
     i                             P  206  211 2ELEXP3                           EST EXP PREM #3
     i                             P  212  217 2ELAXP1                           AUD EXP PREM #1
     i                             P  218  223 2ELAXP2                           AUD EXP PREM #2
     i                             P  224  229 2ELAXP3                           AUD EXP PREM #3
     i                             P  230  232 3ELPROR                           PRORATA FACTOR
     i                                233  234  ELILCD                           INCREASED LIMIT
     i                                235  235  ELEMNP                           EST MINIMUM PRE
     i                                236  236  ELAMNP                           AUD MINIMUM PRE
     i                                237  237  ELDISC                           DISC TYPE S,G
     i                                238  238  ELFAF                            FINAL AUD. FLAG
     i                                239  244 0ELILEF                           INCREASED LIM.
     i                                245  250 0ELANDT                           ANNIVERSARY RAT
     i                             P  251  256 2ELENMP                           ESTIMATED NORMA
     i                             P  257  262 2ELANMP                           AUDITED NORMAL
     i                             P  263  268 2ELEIN1                           EST INCR LMT PR
     i                             P  269  274 2ELAIN1                           AUD INCR LMT PR
     i                             P  275  277 2ELAVCC                           ANNIV CONTRACTO
     i                                278  279  ELMBCD                           MEMBERSHIP TYPE
     i                             P  280  283 2ELMBDU                           MEMBERSHIP DUES
     i                                284  289 0ELDVEF                           DEVIATION EFF D
     i                                290  290  ELEMOD                           ELIGIBLE FOR EX
     i                                291  292  ELQTYP                           QUOTE TYPE CODE
     i                                293  293  ELGMIF                           GENERATE MONTHL
     i                                294  294  ELCDIF                           CALC DISCOUNT O
     i                                295  297  ELRTCD                           RATE SET CODE
     i                                298  298  ELCXBL                           DEDUCTIBLE BILL
     i                             P  299  301 2ELDDCT                           DEDUCTIBLE DISC
     i                             P  302  307 2ELEDCT                           ESTIMATED DEDUC
     i                             P  308  313 2ELADCT                           AUDITED DEDUCTI
     i                                314  316  ELDCDE                           DEDUCTIBLE CODE
     i                             P  317  319 2ELASPM                           MANAGED CARE CR
     i                             P  320  325 2ELEDCM                           ESTIMATED MANAG
     i                             P  326  331 2ELADCM                           AUDITED MANAGED
     i                             P  332  334 2ELASPS                           SAFETY CREDIT P
     i                             P  335  340 2ELEDCS                           ESTIMATED SAFET
     i                             P  341  346 2ELADCS                           AUDITED SAFETY
     i                                347  352 0ELMCEF                           MANAGED CARE EF
     i                                353  358 0ELMCEX                           MANAGED CARE EX
     i                                359  364 0ELSCEF                           SAFETY CREDIT D
     i                                365  366  ELCVC1                           COVERAGE CODE
     i                                367  368  ELCVC2                           COVERAGE CODE
     i                                369  370  ELCVC3                           COVERAGE CODE
     i                                371  372  ELCVC4                           COVERAGE CODE
     i                                373  374  ELCVC5                           COVERAGE CODE
     i                                375  376  ELCVC6                           COVERAGE CODE
     i                                377  378  ELCVC7                           COVERAGE CODE
     i                                379  380  ELCVC8                           COVERAGE CODE
     i                                381  382  ELCVC9                           COVERAGE CODE
     i                                383  384  ELCVC0                           COVERAGE CODE
     i                                385  385  ELSORT                           SORT SEQUENCE
     i                                386  386  ELFREQ                           FREQUENCY
     i                                387  388  ELILC2                           2ND INCREASED L
     i                             P  389  394 2ELEIN2                           EST INCR LMT PR
     i                             P  395  400 2ELAIN2                           AUD INCR LMT PR
     i                                401  406 0ELDFXD                           DRUG FREE EXP.
     i                                407  407  ELCFAF                           CASH FLOW ACCOU
     i                                408  408  ELLFEF                           LATE FEE EXMPT
     i                             P  409  410 2ELDBCR                           SCHEDULE DB/CR
     i                             P  411  416 2ELEDCR                           EST SCHED DB/CR
     i                             P  417  422 2ELADCR                           AUD SCHED DB/CR
     i                                423  428 0ELDCEF                           DEDUCTIBLE EFF.
     i                                429  434 0ELDCEX                           DEDUCTIBLE EXP.
     i                                435  440 0ELSCEX                           SUBJ PREM 1B EX
     i                                441  446 0ELCREF                           CONTRACTORS EFF
     i                                447  452 0ELCREX                           CONTRACTORS EXP
     i                                453  458 0ELSDEF                           SCHED DB/CR EFF
     i                                459  464 0ELSDEX                           SCHED DB/CR EXP
     i                                465  470 0ELILEX                           INCREASED LIM.
     i                             P  471  476 2ELEDCP                           EST SCHED DB/CR
     i                             P  477  482 2ELADCP                           AUD SCHED DB/CR
     i                                483  488 0ELBSDT                           BILLING START D
     i                             P  489  494 2ELINMP                           CURRENT INFORCE

      ** Program entry
      *************************************************
     c     *entry        plist
     c                   parm                    pmFILE           10             File name

      ** Mainline processing
      *************************************************
     c                   read      wdelp                                  LR
     c                   if        *inLR=*on                                     >-+
     c                   exsr      $Finish                                         :
     c                   else                                                    >-+
     c                   exsr      $Process                                        :
     c                   endif                                                   <-+

      ** Program initialization routine
      *************************************************
     csr   *inzsr        begsr                                                   >-+
     csr                 move      *date         xxCCYYMMDD                        :
     csr                 time                    xxHRMNSC                          :
     csr                 eval      xxEDATE=xxMM+'/'+xxDD+'/'+xxCCYY                :
     csr                 eval      xxETIME=xxHR+':'+xxMN+':'+xxSC                  :
     csr                 eval      *in71=*on                                       :
     csr                 eval      *in61=*off                                      :
     csr                 eval      ptPAGE=*zero                                    :
     csr                 eval      xxCOUNT=*zero                                   :
     csr                 eval      xxPRINT=*zero                                   :
     csr                 eval      ptRDAT=xxEDATE                                  :
     csr                 eval      ptRTIM=xxETIME                                  :
     csr                 eval      ptTITL=cnTITLE+' '+cnTYPE+' '+                  :
     c                             %trim(pmFILE)+cnSUBT                            :
     csr   ' '           checkr    ptTITL        IX                                :
     csr                 eval      IX=(%size(ptTITL)-IX)/2                         :
     csr   ' '           cat       ptTITL:IX     ptTITL                            :
     csr                 eval      ptQFIL=%trim('*LIBL  ')+'/'+pmFILE              :
     csr                 endsr                                                   <-+

      ** Finish up routine
      *************************************************
     csr   $Finish       begsr                                                   >-+
     csr                 eval      elCO#=*loval                                    :
     csr                 eval      elFND=*loval                                    :
     csr                 exsr      $Write_Hd                                       :
     csr                 exsr      $Write_Bk1                                      :
     csr                 exsr      $Write_Tl                                       :
     csr                 endsr                                                   <-+

      ** Process a record routine
      *************************************************
     csr   $Process      begsr                                                   >---+
     csr                 eval      xxCOUNT=xxCOUNT+1                                 :
     csr                 eval      xxMAPR=xxMAPR+elMAPR                              :
     csr                 eval      xxSTPR=xxSTPR+elSTPR                              :
     csr                 eval      xxENMP=xxENMP+elENMP                              :
     csr                 eval      xxPRTD=xxPRTD+elPRTD                              :
     csr                 eval      xxANPR=xxANPR+elANPR                              :
     csr                 if        xxCOUNT=1                                     >-+ :
     csr                 eval      xxCO#=elCO#                                     : :
     csr                 eval      xxFND=elFND                                     : :
     csr                 eval      xxFYR=elFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 if        xxCO#<>elCO# or xxFND<>elFND or xxFYR<>elFYR  >-+ :
     csr                 exsr      $Write_Hd                                       : :
     csr                 exsr      $Write_Bk1                                      : :
     csr                 eval      xxCO#=elCO#                                     : :
     csr                 eval      xxFND=elFND                                     : :
     csr                 eval      xxFYR=elFYR                                     : :
     csr                 endif                                                   <-+ :
     csr                 eval      xxCOUNT1=xxCOUNT1+1                               :
     csr                 eval      xxMAPR1=xxMAPR1+elMAPR                            :
     csr                 eval      xxSTPR1=xxSTPR1+elSTPR                            :
     csr                 eval      xxENMP1=xxENMP1+elENMP                            :
     csr                 eval      xxPRTD1=xxPRTD1+elPRTD                            :
     csr                 eval      xxANPR1=xxANPR1+elANPR                            :
     csr                 eval      xxCOUNT2=xxCOUNT2+1                               :
     csr                 eval      xxMAPR2=xxMAPR2+elMAPR                            :
     csr                 eval      xxSTPR2=xxSTPR2+elSTPR                            :
     csr                 eval      xxENMP2=xxENMP2+elENMP                            :
     csr                 eval      xxPRTD2=xxPRTD2+elPRTD                            :
     csr                 eval      xxANPR2=xxANPR2+elANPR                            :
     csr                 endsr                                                   <---+

      ** Write break 1 line
      *************************************************
     csr   $Write_Bk1    begsr                                                   >---+
     csr                 if        xxCO#<>elCO# or xxFND<>elFND                  >-+ :
     csr                 eval      *in61=*on                                       : :
     csr                 eval      xxPRINT=*zero                                   : :
     csr                 endif                                                   <-+ :
     csr                 write     PRTBK1                                            :
     csr                 eval      xxPRINT=xxPRINT+1                                 :
     csr                 eval      xxMAPR1=*zero                                     :
     csr                 eval      xxSTPR1=*zero                                     :
     csr                 eval      xxENMP1=*zero                                     :
     csr                 eval      xxPRTD1=*zero                                     :
     csr                 eval      xxANPR1=*zero                                     :
     csr                 eval      xxCOUNT1=*zero                                    :
     csr                 if        *in61=*on                                     >-+ :
     csr                 write     PRTBK2                                          : :
     csr                 eval      *in61=*off                                      : :
     csr                 eval      xxMAPR2=*zero                                   : :
     csr                 eval      xxSTPR2=*zero                                   : :
     csr                 eval      xxENMP2=*zero                                   : :
     csr                 eval      xxPRTD2=*zero                                   : :
     csr                 eval      xxANPR2=*zero                                   : :
     csr                 eval      xxCOUNT2=*zero                                  : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write a print header
      *************************************************
     csr   $Write_Hd     begsr                                                   >---+
     csr                 if        *in71=*on                                     >-+ :
     csr                 eval      *in71=*off                                      : :
     csr                 eval      ptPAGE=ptPAGE+1                                 : :
     csr                 write     PrtHdr                                          : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      ** Write total line
      *************************************************
     csr   $Write_Tl     begsr                                                   >-+
     csr                 write     PRTTOT                                          :
     csr                 endsr                                                   <-+
