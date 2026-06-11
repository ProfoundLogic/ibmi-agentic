      *  (C) Copyright FISERV INSURANCE SOLUTIONS, INC. 2008
      *
      *****************************************************************
      *  PROGRAM    : F8600R
      *  DESCRIPTION: IMPLEMENT PPO INTERFACE(COVENTRY/MCPS)
      *               LOAD FILE TO SEND TO COVENTRY
      *
      *  CREATED BY : C:47   4/10/08  PROJECT: 129MPN
      *
      *  CALLED FROM: F8600C
      *  CALLS TO   : NONE
      *
      *****************************************************************
      *  REVISIONS:
      *
      *  02/12/09  C: 62  Project: 129MRH
      *                   Back out 12/15 change and populate 020-003.
      *
      *  12/15/08  C: 79  Project: 129MRH
      *                   Change how EOB# and Claim Number are loaded
      *
      *  10/28/08  C: 79  Support: 65786
      *                   Add logic to handle all hospital bill types
      *
      *****************************************************************
    Hdebug(*yes) option(*srcstmt:*nodebugio)
      *
     F*  This FAEAP is in QTEMP
     FFAEAP     IF   E             DISK
     F                                     RENAME(PEA01:PEA0P)
     F*
     F* LF OVER PDSFIL/FAEAP BY: CO#/SUB#/FND/FYR/CASE/EOB#/RCN
     FFAEAL1    UF   E           K DISK
     F*
     FFAEOL4    IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDCAL5    IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFDDHL     IF   E           K DISK
     FFMEML     IF   E           K DISK
     FFIY8P     O    E             DISK
     F*
     F* File Transmission History
     FFiy7p     o    E             DISK
     D*****************************************************************
     D/COPY SCOPYWW01I
     D*
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D FCC             C                   CONST('First Cardinal Corp')
     D xcReas          C                   CONST('Sent to Coventry')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D* Record 000
     D TRNHD           DS                  INZ
     D  THRTYP                 1      3  0
     D  THTTYP                 4      5
     D  THREFN                 6     25
     D  THTRDT                26     45
     D  THTRID                46     65
     D  THSID                 66     85
     D  THSCNM                86    120
     D  THSFNM               121    145
     D  THSLNM               146    180
     D  THSNPR               181    190
     D  THSNSU               191    200
     D  THSADR               201    270
     D  THSCTY               271    300
     D  THSST                301    302
     D  THSZIP               303    313
     D  THSCNY               314    316
     D  THSCNA               317    351
     D  THTORP               352    352
     D  THF1                 353    372  0
     D  THF2                 373    392
     D* Record 002
     D CLMHD           DS                  INZ
     D  CHRTYP                 1      3  0
     D  CHBNUM                 4     23
     D  CHTTYP                24     27
     D  CHSSGN                28     47
     D  CHCCNT                48     67
     D  CHCCNS                68     87
     D  CHCCNE                88    107
     D  CHCCNC               108    127
     D  CHCCNO               128    147
     D  CHRID                148    167
     D  CHFTYP               168    171
     D  CHFVER               172    175  0
     D  CHBTYP               176    178
     D  CHCCNA               179    201
     D  CHDCHG               202    224
     D  CHCCN1               225    247
     D  CHDATE               248    255  0
     D  CHTIME               256    261  0
     D  CHCIDM               262    266
     D  CHCIDC               267    271
     D  CHCIDS               272    276
     D  CHCISG               277    281
     D  CH1                  282    301  0
     D  CH2                  302    341
     D* Record 010
     D HPPROV          DS                  INZ
     D  PRRTYP                 1      3  0
     D  PRBNUM                 4     23
     D  PRFEIN                24     38
     D  PRSEI                 39     39
     D  PRCPRN                40     46
     D  PRPTYP                47     48
     D  PRMRPN                49     61
     D  PRMIPN                62     74
     D  PRBBPN                75     87
     D  PRCHPN                88    100
     D  PROLN1               101    113
     D  PROLN2               114    126
     D  PROLN3               127    139
     D  PRPCM                140    140
     D  PRPSGI               141    153
     D  PRPSGB               154    157
     D  PRPSGP               158    167
     D  PRPSGN               168    227
     D  PRPSGA               228    245
     D  PRPSGC               246    260
     D  PRPSGS               261    262
     D  PRPSGZ               263    273
     D  PRPSGO               274    298
     D  PRPLNM               299    318
     D  PRPFNM               319    333
     D  PRPMI                334    334
     D  PRPDGR               335    337
     D  PRPSI                338    338
     D  PR1                  339    358
     D  PR2                  359    378  0
     D  PR3                  379    398
     D* Record 020
     D HPPAT           DS                  INZ
     D  PARTYP                 1      3  0
     D  PABNUM                 4     23
     D  PAPCN                 24     43
     D  PALSN                 44     63
     D  PAFSN                 64     78
     D  PAMI                  79     79
     D  PASS                  80     88
     D  PASEX                 89     89
     D  PADTOB                90     97
     D  PAMSTS                98     98
     D  PAADR1                99    116
     D  PAADR2               117    134
     D  PACTY                135    164
     D  PAST                 165    166
     D  PAZIP                167    177
     D  PACNY                178    202
     D  PAPHN                203    226
     D  PAES                 227    227
     D  PASTAT               228    229
     D  PAOII                230    230
     D  PA1                  231    250
     D  PAMRSI               251    251
     D  PA2                  252    276
     D  PA3                  277    296
     D  PAMRN                297    313
     D  PA4                  314    338
     D  PA5                  339    349
     D  PA6                  350    369  0
     D  PA7                  370    389
     D* Record 03A
     D HCLHDA          DS                  INZ
     D  HCRTYP                 1      3
     D  HCBNUM                 4     23
     D  HCDIA1                24     29
     D  HCSDC2                30     35
     D  HCTDC3                36     41
     D  HCDIA4                42     47
     D  HCDIA5                48     53
     D  HCDIA6                54     59
     D  HCDIA7                60     65
     D  HCDIA8                66     71
     D  HCDIA9                72     77
     D  HCADDT                78     85
     D  HCATYP                86     86
     D  HCADHR                87     88
     D  HCASRC                89     89
     D  HCACHR                90     91
     D  HCSCFD                92     99
     D  HCSCTD               100    107
     D  HCDSHR               108    109
     D  HCBTYP               110    112
     D  HC1                  113    119
     D  HCOCR1               120    121
     D  HCOCR2               122    123
     D  HCOCR3               124    125
     D  HCOCR4               126    127
     D  HCOCR5               128    129
     D  HCOCR6               130    131
     D  HCOCR7               132    133
     D  HCOCR8               134    135
     D  HCOCD1               136    143
     D  HCOCD2               144    151
     D  HCOCD3               152    159
     D  HCOCD4               160    167
     D  HCOCD5               168    175
     D  HCOCD6               176    183
     D  HCOCD7               184    191
     D  HCOCD8               192    199
     D  HCOSC                200    201
     D  HCOSFD               202    209
     D  HCOSTD               210    217
     D  HCCC1                218    219
     D  HCCC2                220    221
     D  HCCC3                222    223
     D  HCCC4                224    225
     D  HCCC5                226    227
     D  HCCC6                228    229
     D  HCCC7                230    231
     D  HCPOBF               232    233  0
     D  HCPOBR               234    235  0
     D  HCPOBN               236    237  0
     D  HCPOBD               238    238  0
     D  HCVC1                239    240
     D  HCVC2                241    242
     D  HCVC3                243    244
     D  HCVC4                245    246
     D  HCVC5                247    248
     D  HCVC6                249    250
     D  HCVC7                251    252
     D  HCVC8                253    254
     D  HCVC9                255    256
     D  HCVC10               257    258
     D  HCVC11               259    260
     D  HCVC12               261    262
     D  HCVA1                263    271  2
     D  HCVA2                272    280  2
     D  HCVA3                281    289  2
     D  HCVA4                290    298  2
     D  HCVA5                299    307  2
     D  HCVA6                308    316  2
     D  HCVA7                317    325  2
     D  HCVA8                326    334  2
     D  HCVA9                335    343  2
     D  HCVA10               344    352  2
     D  HCVA11               353    361  2
     D  HCVA12               362    370  2
     D  HCPPCD               371    377
     D  HCPPDT               378    385
     D  HCOPC1               386    392
     D  HCOPD1               393    400
     D  HCOPC2               401    407
     D  HCOPD2               408    415
     D  HCOPC3               416    422
     D  HCOPD3               423    430
     D  HCOPC4               431    437
     D  HCOPD4               438    445
     D  HCOPC5               446    452
     D  HCOPD5               453    460
     D  HCNOGD               461    461
     D  HC2                  462    481
     D  HC3                  482    501  0
     D* Record 03B
     D HCLHDB          DS                  INZ
     D  HCRTYB                 1      3
     D  HCBNUB                 4     23
     D  HCTLRD                24     26  0
     D  HCADC                 27     32
     D  HCTHD                 33     43  0
     D  HCAC1                 44     47
     D  HCAC2                 48     51
     D  HCAC3                 52     55
     D  HCTCD                 56     58  0
     D  HCTNCD                59     62  0
     D  HCTCID                63     65  0
     D  HCTDC                 66     69  0
     D  HCTC                  70     80  2
     D  HCTWSA                81     91  2
     D  HCCFD                 92     99
     D  HCCTD                100    107
     D  HCMD                 108    118  2
     D  HCPC                 119    123
     D  HCTAC1               124    141
     D  HCTAC2               142    159
     D  HCTAC3               160    177
     D  HCLTRA               178    188  2
     D  HCNCC                189    199  2
     D  HCDRG                200    202  0
     D  HCEC                 203    208
     D  HCRFSA               209    213
     D  HC4                  214    233  0
     D  HC5                  234    253
     D* Record 031
     D HINPAT          DS                  INZ
     D  HIRTYP                 1      3  0
     D  HIBNUM                 4     23
     D  HISEQ                 24     26  0
     D  HIRC                  27     30  0
     D  HIHPC                 31     41
     D  HIMOD1                42     43
     D  HIMOD2                44     45
     D  HIMOD3                46     47
     D  HIDOS                 48     55
     D  HIAR                  56     64  2
     D  HIU                   65     71  0
     D  HIC                   72     80  2
     D  HINCRC                81     82
     D  HIWCSA                83     91  2
     D  HICSI                 92     93
     D  HICD                  94     98
     D  HIAC                  99    102
     D  HINCC                103    111  2
     D  HINR                 112    120  2
     D  HIPTC                121    125
     D  HISA                 126    134  2
     D  HICF                 135    143  2
     D  HIRFNA               144    147
     D  HI1                  148    167  0
     D  HI2                  168    187
     D* Record 033
     D HOUTPA          DS                  INZ
     D  HORTYP                 1      3  0
     D  HOBNUM                 4     23
     D  HOSEQ                 24     26  0
     D  HORVCD                27     30  0
     D  HOHPC                 31     41
     D  HOMOD1                42     43
     D  HOMOD2                44     45
     D  HOMOD3                46     47
     D  HOU                   48     54  0
     D  HODOS                 55     62
     D  HOC                   63     71  2
     D  HONCRC                72     73
     D  HOWCSA                74     82  2
     D  HOCSI                 83     84
     D  HO1                   85     89
     D  HOAC                  90     93
     D  HONCC                 94    102  2
     D  HONR                 103    111  2
     D  HOSA                 112    120  2
     D  HOCF                 121    129  2
     D  HOPTC                130    134
     D  HORFNA               135    138
     D  HO2                  139    158  0
     D  HO3                  159    178
     D* Record 034
     D HRPHY           DS                  INZ
     D  HPRTYP                 1      3  0
     D  HPBNUM                 4     23
     D  HPAPC                 24     48
     D  HPAPLN                49     68
     D  HPAPFN                69     83
     D  HPAPMI                84     84
     D  HPAPD                 85     87
     D  HPOPID                88    112
     D  HPOPLN               113    132
     D  HPOPFN               133    147
     D  HPOPMI               148    148
     D  HPOPD                149    151
     D  HPOPI1               152    176
     D  HPOPL1               177    196
     D  HPOPF1               197    211
     D  HPOPM1               212    212
     D  HPOPD1               213    215
     D  HPOPI2               216    240
     D  HPOPL2               241    260
     D  HPOPF2               261    275
     D  HPOPM2               276    276
     D  HPOPD2               277    279
     D  HP1                  280    299
     D  HP2                  300    319  0
     D  HP3                  320    339
     D* Record 035
     D HCLMTR          DS                  INZ
     D  HTRTYP                 1      3  0
     D  HTBNUM                 4     23
     D  HTCONT                24     27  0
     D  HTRC31                28     31  0
     D  HT1                   32     35  0
     D  HTRC33                36     39  0
     D  HTRC34                40     43  0
     D  HTTS                  44     54  2
     D  HTTU                  55     63  0
     D  HTTWSA                64     74  2
     D  HTTCC                 75     85  2
     D  HTTNCC                86     96  2
     D  HTTN                  97    107  2
     D  HTTCF                108    118  2
     D  HT2                  119    138
     D  HT3                  139    158  0
     D  HT4                  159    178
     D* Record 050
     D PCLMHD          DS                  INZ
     D  PCRTYP                 1      3  0
     D  PCBNUM                 4     23
     D  PCPDC1                24     29
     D  PCSDC2                30     35
     D  PCTDC3                36     41
     D  PCODC4                42     47
     D  PCPAN                 48     65
     D  PCAFD                 66     73  0
     D  PCATD                 74     81  0
     D  PCSSCD                82     89
     D  PCCI                  90     90
     D  PCSD                  91     98
     D  PCAI                  99     99
     D  PCASC                100    101
     D  PCERI                102    102
     D  PCUI                 103    103
     D  PCHFPN               104    118
     D  PCHFN                119    178
     D  PCHFA                179    196
     D  PCHFC                197    211
     D  PCHFS                212    213
     D  PCHFZ                214    224
     D  PCHFY                225    249
     D  PCCFD                250    257
     D  PCCTD                258    265
     D  PCDFD                266    273
     D  PCDTD                274    281
     D  PCTDC                282    285  0
     D  PCIDC                286    286
     D  PCRTWD               287    294
     D  PCRPLN               295    314
     D  PCRPFN               315    329
     D  PCRPMI               330    330
     D  PC1                  331    341
     D  PCAC1                342    345
     D  PCAC2                346    349
     D  PCAC3                350    353
     D  PCTSL                354    357  0
     D  PCTCC                358    368  2
     D  PCTWSA               369    379  2
     D  PCOLC                380    390  2
     D  PCLI                 391    391
     D  PCAP                 392    402  2
     D  PCAA                 403    403
     D  PCRPID               404    423
     D  PCMRC                424    438
     D  PCMORN               439    458
     D  PCPC                 459    463
     D  PC2                  464    483  0
     D  PC3                  484    498
     D* Record 056
     D PSVC            DS                  INZ
     D  PSRTYP                 1      3  0
     D  PSBNUM                 4     23
     D  PSSEQ                 24     26  0
     D  PSSFD                 27     34
     D  PSSTD                 35     42
     D  PSPC                  43     47
     D  PSPCM1                48     49
     D  PSPCM2                50     51
     D  PSPCM3                52     53
     D  PSUS                  54     57  0
     D  PSPOS                 58     60
     D  PSTOS                 61     63
     D  PSDCP                 64     64  0
     D  PSSLC                 65     73  2
     D  PSCSC                 74     75
     D  PSS                   76     84  0
     D  PSNR                  85     93  2
     D  PS1                   94     98
     D  PSAC1                 99    102
     D  PSWCSA               103    111  2
     D  PSCF                 112    120  2
     D  PSPTC                121    125
     D  PSEFP                126    126
     D  PSEMG                127    128
     D  PS2                  129    148  0
     D  PS3                  149    175
     D* Record 059
     D PCLMTR          DS                  INZ
     D  PTRTYP                 1      3  0
     D  PTBNUM                 4     23
     D  PTCONT                24     26  0
     D  PT1                   27     29  0
     D  PTRTC                 30     32  0
     D  PTTCC                 33     43  2
     D  PTTN                  44     54  2
     D  PTTCF                 55     65  2
     D  PTTS                  66     76  2
     D  PT2                   77     96
     D  PTTWSA                97    107  2
     D  PTTUS                108    111  0
     D  PT3                  112    131  0
     D  PT4                  132    149
     D* Record 062
     D HPPAYR          DS                  INZ
     D  PYRTYP                 1      3  0
     D  PYBNUM                 4     23
     D  PYSEQ                 24     25
     D  PYSOPC                26     26
     D  PYPID                 27     31  0
     D  PYPSID                32     35
     D  PYPN                  36     53
     D  PYPA1                 54     88
     D  PYPA2                 89    123
     D  PYPC                 124    153
     D  PYPS                 154    155
     D  PYPZ                 156    166
     D  PYPY                 167    191
     D  PYPIA                192    202  2
     D  PYER                 203    213  2
     D  PYEAD                214    224  2
     D  PYPCD                225    227
     D  PYPPA                228    236  2
     D  PYMPA                237    245  2
     D  PYPDA                246    254  2
     D  PY1                  255    274
     D  PYCOBA               275    283  2
     D  PYMPBC               284    294  2
     D  PYRFSA               295    308
     D  PYRFNA               309    335
     D  PY2                  336    355  0
     D  PY3                  356    375
     D* Record 063
     D HPINSR          DS                  INZ
     D  INRTYP                 1      3  0
     D  INBNUM                 4     23
     D  INSEQ                 24     25  0
     D  INRPLN                26     45
     D  INRPFN                46     60
     D  INRPMI                61     61
     D  INRPA                 62     79
     D  INRPA1                80     97
     D  INRPA2                98    178
     D  INRPC                179    193
     D  INRPS                194    195
     D  INRPZ                196    206
     D  INRPN                207    231
     D  INIGPI               232    251
     D  INIGN                252    265
     D  INIECI               266    266
     D  INISS                267    285
     D  INILN                286    305
     D  INIFL                306    320
     D  INIMI                321    321
     D  INISEX               322    322
     D  INIDOB               323    330
     D  INIP                 331    340
     D  INEDIC               341    348
     D  INTDIC               349    356
     D  INRICI               357    357
     D  INABCI               358    358
     D  INPIR                359    360  0
     D  INPSI                361    361
     D  INPSD                362    369
     D  INEN                 370    393
     D  INEA                 394    411
     D  INES                 412    426
     D  INEC                 427    428
     D  INEZ                 429    439
     D  INEY                 440    464
     D  INEID                465    476
     D  INSPI                477    478
     D  INEIC                479    479
     D  INESC                480    480  0
     D  IN1                  481    500
     D  IN2                  501    520  0
     D  IN3                  521    538
     D* Record 065
     D REMARK          DS                  INZ
     D  RERTYP                 1      3  0
     D  REBNUM                 4     23
     D  RER1                  24     66
     D  RER2                  67    114
     D  RER3                 115    162
     D  RER4                 163    210
     D  RE1                  211    230
     D* Record 095
     D MESSAG          DS                  INZ
     D  MSRTYP                 1      3  0
     D  MSBNUM                 4     23
     D  MSSEQ                 24     24  0
     D  MST                   25     64
     D  MSC                   65     69
     D  MS1                   70     89  0
     D  MS2                   90    129
     D* Record 097
     D HPCLMT          DS                  INZ
     D  CTRTYP                 1      3  0
     D  CTBNUM                 4     23
     D  CTTCC                 24     34  2
     D  CTTWSA                35     45  2
     D  CTTNRC                46     50  0
     D  CTRC30                51     54  0
     D  CTRC50                55     58  0
     D  CTRC62                59     62  0
     D  CTRC63                63     66  0
     D  CTRC64                67     70  0
     D  CTRC65                71     74  0
     D  CTRC71                75     78  0
     D  CT1                   79     82  0
     D  CTRC75                83     86  0
     D  CTRC77                87     90  0
     D  CTRC81                91     94  0
     D  CTRC83                95     98  0
     D  CTRC95                99    102  0
     D  CT2                  103    122  0
     D  CT3                  123    162
     D* Record 099
     D TRNTR           DS                  INZ
     D  TTRTYP                 1      3  0
     D  TTTCOF                 4     14  2
     D  TTTC                  15     19  0
     D  TTTR                  20     24  0
     D  TTTB                  25     29  0
     D  TT1                   30     49  0
     D  TT2                   50     89
     D                 DS
     D  XXOTH2                 1      9  2
     D  OTH2A                  1      7  0
     D  OTH2B                  8      9  2
     D                 DS
     D  ZZCHRG                 1     10
     D  CHGA                   1      7
     D  PNT1                   8      8    INZ('.')
     D  CHGB                   9     10
     D                 DS
     D  XXSFFC                 1     20
     D  EASUB#                 1      5  0
     D  EAFND                  6      8  0
     D  EAFYR                  9     11  0
     D  EACASE                12     18  0
     D* FAEUP TYPE OF BILL
     D                 DS
     D  EUBLTY                 1      3
     D  EUBLT2                 2      2
     F*
      *****************************************************************
     IPEA01
     I              EARCN                       E2RCN
     I              EANSNT                      E2NSNT
     I              EANPDT                      E2NPDT
     I              EAUSER                      E2USER
     I              EAPGM                       E2PGM
     I              EADT                        E2DT
     I              EATIME                      E2TIME
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *entry        PLIST
     C                   PARM                    @@filn           25
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTYYMD2       PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     CDKY          KLIST
     C                   KFLD                    CMSS#
     C*
     C     CMKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C     DHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DUKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     E2KY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C                   KFLD                    EARCN
     C*
     C     EHKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EMKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMSUB#
     C                   KFLD                    CMFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C*
     C     EOKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     EOKY2         KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C*
     C     EUKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*
     C     PDKY          KLIST
     C                   KFLD                    EAFEIN
     C                   KFLD                    EAMOD
     C*
     C*================================================================
     C*                  M A I N L I N E
     C*================================================================
     C                   EXSR      PGMSTR
     C                   EXSR      LDTRHD
     C*
     C*  This FAEAP is in QTEMP
     C*
     C     *IN96         DOUEQ     *ON
     C                   READ      PEA0P                                  96
     C     *IN96         IFEQ      *OFF
     C*
     C                   EXSR      LDCLHD
     C                   EXSR      LDPROV
     C                   EXSR      LDPAT
     C                   EXSR      RESET
     C*
     C* HOSPITAL BILL
     C     EOBTYP        IFEQ      'H'
     C                   EXSR      LDHCHA
     C                   EXSR      LDHCHB
     C*
     C* INPATIENT HOSPITAL BILL
     C     CHBTYP        IFEQ      '001'
     C                   EXSR      LDHIPA
     C                   END
     C*
     C* OUTPATIENT HOSPITAL BILL
     C     CHBTYP        IFEQ      '002'
     C                   EXSR      LDHOPA
     C                   END
     C*
     C                   EXSR      LDHPHY
     C                   EXSR      LDHCTR
     C                   END
     C*
     C* PHYSICAN BILL
     C     EOBTYP        IFEQ      'N'
     C                   EXSR      LDPCHD
     C                   EXSR      LDPSVC
     C                   EXSR      LDPCTR
     C                   END
     C*
     C                   EXSR      LDPAYR
     C                   EXSR      LDINSR
     C                   EXSR      LDRMK
     C                   EXSR      LDMSG
     C                   EXSR      LDCLTR
     C*
     C                   EXSR      UPDATR
     C                   END
     C                   ENDDO
     C*
     C                   EXSR      LDTRTR
     C                   EXSR      WrFIY7P
     C                   EXSR      PGMEND
     C*
     C*
     C*================================================================
     C* PGMSTR -
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   CALL      COMAPI        COMPL                  99
     C                   Z-ADD     *ZERO         BNUM             20 0
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CYMD              7 0
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         YYMD              8 0
     C*
     C                   ENDSR
     C*================================================================
     C* LDTRHD - Load transmission header record.
     C*================================================================
     C     LDTRHD        BEGSR
     C*
     C                   CLEAR                   TRNHD
     C                   MOVEL(P)  '000'         THRTYP
     C                   MOVEL(P)  'IC'          THTTYP
     C                   MOVEL(P)  YYMD          THTRDT
     C                   MOVEL     *BLANK        THTRID
     C* 000-06 SUBMITTER ID
     C                   MOVEL(P)  'MF9100'      THSID
     C                   MOVEL(P)  FCC           THSCNM
     C*
     C*****  WILL HAVE TO CHANGE FROM T TO P WHEN TESTING IS COMPLETE.
     C                   MOVEL     'P'           THTORP
     C*
     C                   Z-ADD     *ZERO         THF1
     C                   MOVE      *BLANK        THF2
     C                   MOVEL(P)  TRNHD         Y8DATA
     C                   WRITE     PY801
     C                   Z-ADD     1             TOTREC            5 0
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* LDCLHD - Create Hosp/Phy Claim Header Record.
     C*        - Record 002
     C*================================================================
     C     LDCLHD        BEGSR
     C*
     C                   CLEAR                   CLMHD
     C                   MOVEL(P)  '002'         CHRTYP
     C*
     C                   ADD       1             BNUM
     C                   MOVE      *ZERO         CHBNUM
     C                   MOVE      BNUM          CHBNUM
     C* 002-03
     C     EARCN         IFEQ      *ZERO
     C                   MOVEL(P)  '01  '        CHTTYP
     C                   ELSE
     C                   MOVEL(P)  '02  '        CHTTYP
     C                   ENDIF
     C* 002-04 & 05
     C                   MOVEL(P)  'MF9'         CHSSGN
     C                   MOVEL(P)  YYMD          CHCCNT
     C*
     C* 002-06 PUT EOB# & RECON# IN 20A FIELD, LEFT JUSTIFIED
     C*
     C                   MOVE(P)   EAEOB#        EOB               9
     C                   MOVE(P)   EARCN         RCN               3
     C                   MOVEL(P)  EOB           ER               12
     C                   MOVE      RCN           ER
     C                   MOVEL(P)  ER            CHCCNS
     C* 002-08
     C                   MOVEL(P)  XXSFFC        CHCCNC
     C                   MOVEL(P)  'F2E'         CHFTYP
     C*
     C     EOKY          CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   add       eochrg        totEOCHRG        11 2          for TTTCOF 099-02
     C                   add       1             totEO_Cnt         5 0          for TTTC   099-03
     C* ---
     C* 002-13 TYPE OF Claim
     C*
     C     EOBTYP        IFEQ      'H'
     C*
     C     EUKY          CHAIN     PEU01                              98
     C     *IN98         IFEQ      *OFF
     C*
     C                   SELECT
     C*** BEG DEL *** 10/28/08 *********************************************
     C**** EUBLT2        WHENEQ    '1'
     C****               MOVEL(P)  '001'         CHBTYP
     C**** EUBLT2        WHENEQ    '2'
     C**** EUBLT2        OREQ      '3'
     C*** END DEL *** 10/28/08 *********************************************
     C*** BEG ADD *** 10/28/08 *********************************************
     C     EUBLT2        WHENEQ    '1'
     C     EUBLT2        OREQ      '2'
     C                   MOVEL(P)  '001'         CHBTYP
     C     EUBLT2        WHENEQ    '3'
     C*** END ADD *** 10/28/08 *********************************************
     C                   MOVEL(P)  '002'         CHBTYP
     C*** BEG ADD *** 10/28/08 *********************************************
     C                   OTHER
     C*
     C* If you cant determine Type from 3 digit bill type use service dates
     C* or type of servce
     C*
     C     EUICN         IFNE      *BLANKS
     C*
     C     EUICN         IFGE      'IM'
     C     EUICN         ANDLE     'IS'
     C                   MOVEL(P)  '001'         CHBTYP
     C                   ELSE
     C                   MOVEL(P)  '002'         CHBTYP
     C                   ENDIF
     C*
     C                   ELSE
     C     EOFRDT        IFNE      EOTODT
     C                   MOVEL(P)  '001'         CHBTYP
     C                   ELSE
     C                   MOVEL(P)  '002'         CHBTYP
     C                   ENDIF
     C*
     C                   ENDIF                                                  EUICN <> blankss
     C*
     C*** END ADD *** 10/28/08 *********************************************
     C                   ENDSL
     C                   END                                                    PEU01
     C*
     C                   END                                                    EOBTYP = H
     C*
     C     EOBTYP        IFEQ      'N'
     C                   MOVEL(P)  '003'         CHBTYP
     C                   END
     C* ---
     C                   END
     C*
     C                   MOVEL     *BLANK        CHCCNA
     C* 002-15
     C***                MOVEL(P)  'DCHG'        CHDCHG
     C                   MOVE      *BLANK        CHDCHG
     C* --------------------
     C* 002-16 ORIGINAL BILL
     C* FAEAP/EAEOB#,EARCN DETERMINED BY FINDING THE FIRST BILL
     C* THAT WAS SENT.  GET THE EARLIEST EANSNT=P RECORD.
     C* EX: IF RCN#=0, BUT EANSNT=BLANK, CHECK RCN#1.
     C* IF THE RECORD FOR RCN# 1 IS EANSNT=P, THAT WOULD BE THE
     C* ORIGINAL BILL TO SEND.  IF NOT, USE THE NEXT ONE.
     C*
     C                   MOVEL(P)  ER            CHCCN1
     C*
     C     E2KY          SETLL     PEA01
     C     *IN98         DOUEQ     *ON
     C     E2KY          READE     PEA01                                  98
     C     *IN98         IFEQ      *OFF
     C*
     C     E2NSNT        IFEQ      'P'
     C                   Z-ADD     E2RCN         PREC              3 0
     C                   LEAVE
     C                   END
     C*
     C                   END
     C                   ENDDO
     C*
     C     PREC          IFGT      *ZERO
     C                   MOVE(P)   EAEOB#        EOBO              9
     C                   MOVE(P)   PREC          RCNO              3
     C                   MOVEL(P)  EOBO          ERO              12
     C                   MOVE      RCNO          ERO
     C                   MOVEL(P)  ERO           CHCCN1
     C                   END
     C*
     C                   if        prec = *zero
     C                   MOVE      *blank        CHCCN1
     C                   END
     C* --------------------
     C* 002-17 & 18
     C                   Z-ADD     YYMD          CHDATE
     C                   Z-ADD     DSTIME        CHTIME
     C*
     C                   Z-ADD     *ZERO         CH1
     C                   MOVE      *BLANK        CH2
     C                   MOVEL(P)  CLMHD         Y8DATA
     C                   WRITE     PY801
     C                   z-add     1             tnrc              5 0
     C                   ADD       1             TOTREC
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* LDPROV - Create Hosp/Phy Provider Record.
     C*        - Record 010
     C*================================================================
     C     LDPROV        BEGSR
     C*
     C                   CLEAR                   HPPROV
     C                   MOVEL(P)  '010'         PRRTYP
     C*
     C                   MOVE      *ZERO         PRBNUM
     C                   MOVE      BNUM          PRBNUM
     C* 010-03
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PDFEIN        PRFE12           12
     C                   MOVE      PDMOD         PRFE12
     C                   MOVEL(P)  PRFE12        PRFEIN
     C* 010-24 & 25
     C                   MOVEL(P)  PDNAME        PRPLNM
     C                   MOVEL(P)  PDFSN         PRPFNM
     C                   END
     C* 010-04
     C                   MOVEL(P)  'E'           PRSEI
     C* 010-18-22
     C                   MOVEL(P)  EAPNAM        PRPSGN
     C                   MOVEL(P)  EAADR1        PRPSGA
     C                   MOVEL(P)  EACTY         PRPSGC
     C                   MOVEL(P)  EAST          PRPSGS
     C                   MOVEL(P)  EAZIP         PRPSGZ
     C*
     C                   Z-ADD     *ZERO         PR2
     C                   MOVE      *BLANK        PR3
     C                   MOVEL(P)  HPPROV        Y8DATA
     C                   WRITE     PY801
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*
     C*================================================================
     C* LDPAT  - Create Hosp/Phy Patient Record.
     C*        - Record 020
     C*================================================================
     C     LDPAT         BEGSR
     C*
     C                   CLEAR                   HPPAT
     C                   MOVEL(P)  '020'         PARTYP
     C*** BEGIN ADD *** 02/12/09 *******************************************
     C* 020-03
     C                   MOVEL(P)  XXSFFC        PAPCN
     C*** END   ADD *** 02/12/09 *******************************************
     C*
     C                   MOVE      BNUM          PABNUM
     C* 020-07
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE(P)   CMSS#         PASS
     C                   END
     C* 020-04-15
     C     CDKY          CHAIN     PCD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  CDLSN         PALSN
     C                   MOVEL(P)  CDFSN         PAFSN
     C                   MOVEL(P)  CDMID         PAMI
     C                   MOVEL(P)  CDSEX         PASEX
     C***                MOVEL(P)  CDDTOB        PADTOB
     C                   Z-ADD     cddtob        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         padtob
     C                   MOVEL(P)  CDADR1        PAADR1
     C                   MOVEL(P)  CDCTY         PACTY
     C                   MOVEL(P)  CDST          PAST
     C                   MOVEL(P)  CDZIP         PAZIP
     C                   END
     C*
     C                   MOVE      *BLANK        PA1
     C                   MOVE      *BLANK        PA2
     C*
     C* 020-24 ACCIDENT DATE
     C***                MOVE      EAACDT        PA3
     C                   Z-ADD     EAACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         pa3
     C*
     C                   Z-ADD     *ZERO         PA6
     C                   MOVE      *BLANK        PA7
     C                   MOVEL(P)  HPPAT         Y8DATA
     C                   WRITE     PY801
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* LDHCHA - Create Hosp Claim Header A Record.
     C*        - Record 03A
     C*================================================================
     C     LDHCHA        BEGSR
     C*
     C                   CLEAR                   HCLHDA
     C                   MOVEL(P)  '03A'         HCRTYP
     C*
     C                   MOVE      BNUM          HCBNUM
     C* 03A-03
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EUDIA1        HCDIA1
     C* 03A-12
     C                   Z-ADD     EUADDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HCADDT
     C* 03A-13
     C                   MOVEL(P)  EUADTY        HCATYP
     C                   MOVEL(P)  EUADHR        HCADHR
     C* 03A-15
     C                   MOVEL(P)  EUADSC        HCASRC
     C     EUADSC        IFEQ      *BLANK
     C                   MOVEL(P)  '9'           HCASRC
     C                   END
     C* 03A-17 STATEMENT-COVERS-FROM-DATE
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HCSCFD
     C* 03A-18
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HCSCTD
     C* 03A-19
     C                   MOVEL(P)  EUDCHR        HCDSHR
     C* 03A-20
     C                   MOVEL(P)  EUBLTY        HCBTYP
     C* 03A-52
     C                   MOVEL(P)  EUVC01        HCVC1
     C* 03A-76
     C                   MOVEL(P)  EUPRC1        HCPPCD
     C* 03A-77
     C                   Z-ADD     EUPRD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HCPPDT
     C*
     C                   END
     C*
     C                   Z-ADD     *ZERO         HC3
     C                   MOVEL(P)  HCLHDA        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC30
     C                   ADD       1             CLMCNT
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C*
     C                   ENDSR
     C*================================================================
     C* LDHCHB - Create Hosp Claim Header B Record.
     C*        - Record 03B
     C*================================================================
     C     LDHCHB        BEGSR
     C*
     C                   CLEAR                   HCLHDB
     C                   MOVEL(P)  '03B'         HCRTYB
     C                   MOVE      BNUM          HCBNUB
     C* 03B-0
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EUDIA1        HCADC
     C                   Z-ADD     EUDYCV        HCTDC
     C                   Z-ADD     EUDRG#        HCDRG
     C                   MOVE(P)   *BLANK        HCEC
     C                   END
     C* 03B-13 TOTAL CHARGES
     C                   Z-ADD     EOCHRG        HCTC
     C*** BEG DEL *** 10/28/08 ****************************************
     C****************** z-add     EOPAID        HCTWSA
     C*** END DEL *** 10/28/08 ****************************************
     C*** BEG ADD *** 10/28/08 ****************************************
     C*
     C* Calculate total paid omitting negative payments like APORT
     C*
     C     CAKY          SETLL     PCA01
     C     *IN98         DOUEQ     *ON
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C*
     C* Skip lines with payment < 0, e.g. APORT, etc.
     C*
     C     CAPAID        IFLT      0
     C                   ITER
     C                   ENDIF
     C* 03B-14 TOTAL-W/C-SCHEDULE-AMOUNT
     C                   ADD       CAPAID        HCTWSA
     C*
     C                   ENDIF
     C*
     C                   ENDDO
     C*** END ADD *** 10/28/08 ****************************************
     C*
     C                   Z-ADD     *ZERO         HC4
     C                   MOVE      *BLANK        HC5
     C                   MOVEL(P)  HCLHDB        Y8DATA
     C                   WRITE     PY801
     C***                ADD       1             CLMCNT
     C***                ADD       1             TOTREC
     C*
     C                   ENDSR
     C*================================================================
     C* RESET  - Reset counters
     C*================================================================
     C     RESET         BEGSR
     C                   Z-ADD     *ZERO         CLMCNT            5 0
     C                   Z-ADD     *ZERO         CLMC30            5 0
     C                   Z-ADD     *ZERO         CLMC31            5 0
     C                   Z-ADD     *ZERO         CLMC33            5 0
     C                   Z-ADD     *ZERO         CLMC34            5 0
     C                   Z-ADD     *ZERO         UNITSC            7 0
     C                   Z-ADD     *ZERO         MRAC             11 2
     C                   Z-ADD     *ZERO         CHG              11 2
     C                   Z-ADD     *ZERO         CLMC50            5 0
     C                   Z-ADD     *ZERO         CLMC56            5 0
     C                   Z-ADD     *ZERO         CLMC59            5 0
     C                   Z-ADD     *ZERO         CLMC62            5 0
     C                   Z-ADD     *ZERO         CLMC63            5 0
     C                   Z-ADD     *ZERO         CLMC65            5 0
     C                   Z-ADD     *ZERO         CLMC95            5 0
     C                   ENDSR
     C*================================================================
     C* LDHIPA - Load all Hospital Inpatient records.
     C*        - Record 031
     C*================================================================
     C     LDHIPA        BEGSR
     C                   CLEAR                   HINPAT
     C                   MOVEL(P)  '031'         HIRTYP
     C                   MOVE      BNUM          HIBNUM
     C*
     C     CAKY          SETLL     PCA01
     C     *IN98         DOUEQ     *ON
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HIDOS
     C* 031-12
     C                   Z-ADD     CACHRG        HIC
     C                   ADD       CACHRG        CHG
     C* 031-14
     C                   Z-ADD     capaid        HIWCSA
     C                   ADD       capaid        MRAC
     C*
     C     DUKY          CHAIN     PDU01                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     DUDSEQ        HISEQ
     C* 031-04
     C                   Z-ADD     DURVCD        HIRC
     C                   MOVEL(P)  DURATE        HIHPC
     C*
     C* 031-11
     C                   Z-ADD     DUUNTS        HIU
     C                   ADD       DUUNTS        UNITSC
     C                   END
     C*
     C                   Z-ADD     *ZERO         HI1
     C                   MOVE      *BLANK        HI2
     C                   MOVEL(P)  HINPAT        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMCNT
     C                   ADD       1             CLMC31
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C*
     C                   END
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* LDHOPA - Load Hospital Outpatient record.
     C*        - Record 033
     C*================================================================
     C     LDHOPA        BEGSR
     C                   CLEAR                   HOUTPA
     C                   MOVEL(P)  '033'         HORTYP
     C                   MOVE      BNUM          HOBNUM
     C*
     C     CAKY          SETLL     PCA01
     C     *IN98         DOUEQ     *ON
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C* 033-10
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         HODOS
     C* 033-11
     C                   Z-ADD     CACHRG        HOC
     C                   ADD       CACHRG        CHG
     C* 033-13
     C                   Z-ADD     capaid        HOWCSA
     C                   ADD       capaid        MRAC
     C*
     C     DUKY          CHAIN     PDU01                              97
     C     *IN97         IFEQ      *OFF
     C                   Z-ADD     DUDSEQ        HOSEQ
     C* 033-04
     C                   Z-ADD     DURVCD        HORVCD
     C                   MOVEL(P)  DURATE        HOHPC
     C* 033-09
     C                   Z-ADD     DUUNTS        HOU
     C                   ADD       DUUNTS        UNITSC
     C                   END
     C*
     C                   Z-ADD     *ZERO         HO2
     C                   MOVE      *BLANK        HO3
     C                   MOVEL(P)  HOUTPA        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMCNT
     C                   ADD       1             CLMC33
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   END
     C                   ENDDO
     C*
     C                   ENDSR
     C*================================================================
     C* LDHPHY - Load Hosp Physician record.
     C*        - Record 034
     C*================================================================
     C     LDHPHY        BEGSR
     C                   CLEAR                   HRPHY
     C                   MOVEL(P)  '034'         HPRTYP
     C                   MOVE      BNUM          HPBNUM
     C                   Z-ADD     *ZERO         HP2
     C                   MOVE      *BLANK        HP3
     C                   MOVEL(P)  HRPHY         Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMCNT
     C                   ADD       1             CLMC34
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDHCTR - Load Hosp Claim trailer record.
     C*        - Record 035
     C*================================================================
     C     LDHCTR        BEGSR
     C                   CLEAR                   HCLMTR
     C                   MOVEL(P)  '035'         HTRTYP
     C                   MOVE      BNUM          HTBNUM
     C                   ADD       1             CLMCNT
     C* 035-03
     C                   Z-ADD     CLMCNT        HTCONT
     C* 035-04
     C                   Z-ADD     CLMC31        HTRC31
     C* 035-06
     C                   Z-ADD     CLMC33        HTRC33
     C* 035-07
     C                   Z-ADD     CLMC34        HTRC34
     C* 035-09
     C                   Z-ADD     UNITSC        HTTU
     C* 035-10
     C                   Z-ADD     MRAC          HTTWSA
     C* 035-11
     C                   Z-ADD     CHG           HTTCC
     C* 035-12
     C                   Z-ADD     *ZERO         HTTNCC
     C*
     C* 035-13: OutBound FROM COVENTRY
     C*
     C* 035-14
     C                   Z-ADD     *zero         HTTCF
     C*
     C                   Z-ADD     *ZERO         HT3
     C                   MOVE      *BLANK        HT4
     C                   MOVEL(P)  HCLMTR        Y8DATA
     C                   WRITE     PY801
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDPCHD - Load Physician Claim header record.
     C*        - Record 050
     C*================================================================
     C     LDPCHD        BEGSR
     C                   CLEAR                   PCLMHD
     C                   MOVEL(P)  '050'         PCRTYP
     C                   MOVE(P)   BNUM          PCBNUM
     C*
     C     EHKY          CHAIN     PEH01                              99
     C     *IN99         IFEQ      *OFF
     C* 050-03
     C                   MOVEL(P)  EHDIA1        PCPDC1
     C                   END
     C* 050-18-22
     C                   MOVEL(P)  EAPNAM        PCHFN
     C                   MOVEL(P)  EAADR1        PCHFA
     C                   MOVEL(P)  EACTY         PCHFC
     C                   MOVEL(P)  EAST          PCHFS
     C                   MOVEL(P)  EAZIP         PCHFZ
     C                   Z-ADD     *ZERO         PCTDC
     C*
     C     CAKY          SETLL     PCA01
     C     *IN98         DOUEQ     *ON
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C*** BEG ADD *** 10/28/08 ****************************************
     C*
     C* Skip lines with payment < 0, e.g. APORT, etc.
     C*
     C     CAPAID        IFLT      0
     C                   ITER
     C                   ENDIF
     C*** END ADD *** 10/28/08 ****************************************
     C*
     C                   Z-ADD     *ZERO         PCTSL
     C     DHKY          CHAIN     PDH01                              97
     C     *IN97         IFEQ      *OFF
     C* 050-38
     C                   ADD       1             PCTSL
     C                   END
     C*
     C* 050-39
     C                   ADD       CACHRG        PCTCC
     C* 050-40
     C                   ADD       capaid        PCTWSA
     C                   END
     C                   ENDDO
     C*
     C                   Z-ADD     *ZERO         PC2
     C                   MOVE      *BLANK        PC3
     C                   MOVEL(P)  PCLMHD        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC50
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDPSVC - Load Physician Service record.
     C*        - Record 056
     C*================================================================
     C     LDPSVC        BEGSR
     C                   CLEAR                   PSVC
     C                   MOVEL(P)  '056'         PSRTYP
     C                   MOVE      BNUM          PSBNUM
     C                   Z-ADD     *ZERO         TMRAA            11 2
     C                   Z-ADD     *ZERO         TPSUS             4 0
     C*
     C     CAKY          SETLL     PCA01
     C     *IN98         DOUEQ     *ON
     C     CAKY          READE     PCA01                                  98
     C     *IN98         IFEQ      *OFF
     C*** BEG ADD *** 10/28/08 ****************************************
     C*
     C* Skip lines with payment < 0, e.g. APORT, etc.
     C*
     C     CAPAID        IFLT      0
     C                   ITER
     C                   ENDIF
     C*** END ADD *** 10/28/08 ****************************************
     C* 056-04
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         PSSFD
     C*
     C* 056-05
     C                   Z-ADD     CATDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD2
     C                   MOVEL(P)  SRDTO         PSSTD
     C*
     C     DHKY          CHAIN     PDH01                              97
     C     *IN97         IFEQ      *OFF
     C* 056-03
     C                   Z-ADD     DHDSEQ        PSSEQ
     C*
     C* -----------------
     C* 056-10 UNITS
     C                   Z-ADD     caunts        PSUS
     C*
     C* 056-10 ANESTHESIA UNITS
     C     CAUNTS        IFGT      *ZERO
     C     DHPRCD        IFGE      '00100'
     C     DHPRCD        ANDLE     '01999'
     C     DHPRCD        ORGE      '99100'
     C     DHPRCD        ANDLE     '99140'
     C*
     C                   SELECT
     C     CAUNTS        WHENLT    15
     C                   Z-ADD     1             PSUS
     C*
     C     CAUNTS        WHENLE    240
     C     CAUNTS        DIV       15            PSUS
     C                   MVR                     AUNTS             4 0
     C     AUNTS         IFGT      *ZERO
     C                   ADD       1             PSUS
     C                   END
     C*
     C                   OTHER
     C                   Z-ADD     16            PSUS
     C     CAUNTS        SUB       240           AUNTS
     C     AUNTS         DIV       10            BUNTS             4 0
     C                   MVR                     CUNTS             4 0
     C                   ADD       BUNTS         PSUS
     C     CUNTS         IFGT      *ZERO
     C     CUNTS         ANDLE     5
     C                   ADD       1             PSUS
     C                   END
     C     CUNTS         IFGT      5
     C     CUNTS         ANDLT     9
     C                   ADD       2             PSUS
     C                   END
     C*
     C                   ENDSL
     C                   END
     C                   END
     C*
     C                   if        psus = *zero
     C                   z-add     1             PSUS
     C                   end
     C*
     C                   ADD       PSUS          TPSUS
     C* -----------------
     C* 056-14
     C                   Z-ADD     CACHRG        PSSLC
     C* 056-20
     C                   Z-ADD     capaid        PSWCSA
     C                   ADD       capaid        TMRAA
     C*
     C* 056-06
     C                   MOVEL(P)  DHPRCD        PSPC
     C* 056-07
     C                   MOVEL(P)  DHMOD1        PSPCM1
     C* 056-11
     C                   MOVEL(P)  DHPOS         PSPOS
     C* 056-12
     C                   MOVEL(P)  DHTOS         PSTOS
     C     DHTOS         IFEQ      *BLANK
     C                   MOVEL(P)  '9  '         PSTOS
     C                   END
     C*
     C                   Z-ADD     *ZERO         PS2
     C                   MOVE      *BLANK        PS3
     C                   MOVEL(P)  PSVC          Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC56
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C*
     C                   END                                                    PDH01
     C*
     C                   END
     C                   ENDDO                                                  PCA01
     C*
     C                   ENDSR
     C*================================================================
     C* LDPCTR - Load Physician Claim Trailer record.
     C*        - Record 059
     C*================================================================
     C     LDPCTR        BEGSR
     C                   CLEAR                   PCLMTR
     C                   MOVEL(P)  '059'         PTRTYP
     C                   MOVE      BNUM          PTBNUM
     C*
     C* 059-03 PTCONT IS A COUNT OF RECORDS 50, 56, & 59.
     C*   SO COUNT ALL THE 56 RECORDS AND
     C*   ADD 1 EACH FOR HEADER AND TRAILER.
     C*
     C     CLMC56        ADD       2             PTCONT
     C*
     C                   Z-ADD     *ZERO         PT1
     C* 059-05
     C                   Z-ADD     CLMC56        PTRTC
     C*
     C* 059-06
     C                   Z-ADD     EOCHRG        PTTCC
     C* 059-11
     C                   Z-ADD     TMRAA         PTTWSA
     C* 059-12
     C                   Z-ADD     TPSUS         PTTUS
     C*
     C                   Z-ADD     *ZERO         PT3
     C                   MOVE      *BLANK        PT4
     C                   MOVEL(P)  PCLMTR        Y8DATA
     C                   WRITE     PY801
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDPAYR - Load Hosp/Phy Payor record.
     C*        - Record 062
     C*================================================================
     C     LDPAYR        BEGSR
     C                   CLEAR                   HPPAYR
     C                   MOVEL(P)  '062'         PYRTYP
     C                   MOVE      BNUM          PYBNUM
     C* 062-03
     C                   MOVEL(P)  '01'          PYseq
     C*
     C     CMKY          CHAIN     PCM01                              99
     C     *IN99         IFEQ      *OFF
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EMEMP#        PYPID
     C                   MOVE      EMEMP#        PYPSID
     C                   MOVEL(P)  EMNAME        PYPN
     C*
     C* PUT EMP DIVISION IN ADDRESS IN CASE WE NEED IT.
     C*
     C                   MOVEL(P)  EMDIV         XXDIV             5
     C     'DIV:'        CAT(P)    XXDIV:1       PYPA1
     C                   END
     C                   END
     C*
     C                   Z-ADD     *ZERO         PY2
     C                   MOVE      *BLANK        PY3
     C                   MOVEL(P)  HPPAYR        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC62
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDINSR - Load Hosp/Phy Insured record.
     C*        - Record 063
     C*================================================================
     C     LDINSR        BEGSR
     C                   CLEAR                   HPINSR
     C                   MOVEL(P)  '063'         INRTYP
     C                   MOVE      BNUM          INBNUM
     C*
     C                   Z-ADD     1             INSEQ
     C*
     C                   Z-ADD     *ZERO         IN2
     C                   MOVE      *BLANK        IN3
     C                   MOVEL(P)  HPINSR        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC63
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDRMK  - Load Remarks record.
     C*        - Record 065
     C*================================================================
     C     LDRMK         BEGSR
     C                   CLEAR                   REMARK
     C                   MOVEL(P)  '065'         RERTYP
     C                   MOVE      BNUM          REBNUM
     C*
     C     RER1          IFNE      *BLANK
     C                   MOVEL(P)  REMARK        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             CLMC65
     C                   add       1             tnrc
     C                   ADD       1             TOTREC
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* LDMSG  - Load Message record.
     C*        - Record 095
     C*================================================================
     C     LDMSG         BEGSR
     C                   CLEAR                   MESSAG
     C                   MOVEL(P)  '095'         MSRTYP
     C                   ENDSR
     C*================================================================
     C* LDCLTR - Load Claim trailer record.
     C*        - Record 097
     C*================================================================
     C     LDCLTR        BEGSR
     C                   CLEAR                   HPCLMT
     C                   MOVEL(P)  '097'         CTRTYP
     C                   MOVE      BNUM          CTBNUM
     C*
     C* 097-03
     C                   Z-ADD     EOCHRG        CTTCC
     C* 097-04
     C*   PHYSICAN BILL - total W/C Scheduled Amount
     C     EOBTYP        IFEQ      'N'
     C*   (from 059-11)
     C                   Z-ADD     pttwsa        CTtwsa
     C                   END
     C*
     C*   Hospital BILL - total of non-covered charges
     C     EOBTYP        IFEQ      'H'
     C*   (from 035-12)
     C                   Z-ADD     httncc        CTtwsa
     C                   END
     C*
     C* 097-05
     C***  HTCONT        ADD       PTCONT        CTTNRC
     C     tnrc          ADD       1             CTTNRC
     C                   Z-ADD     CLMC30        CTRC30
     C                   Z-ADD     CLMC50        CTRC50
     C                   Z-ADD     CLMC62        CTRC62
     C                   Z-ADD     CLMC63        CTRC63
     C                   Z-ADD     *ZERO         CTRC64
     C                   Z-ADD     CLMC65        CTRC65
     C                   Z-ADD     *ZERO         CTRC71
     C                   Z-ADD     *ZERO         CT1
     C                   Z-ADD     *ZERO         CTRC75
     C                   Z-ADD     *ZERO         CTRC77
     C                   Z-ADD     *ZERO         CTRC81
     C                   Z-ADD     *ZERO         CTRC83
     C                   Z-ADD     CLMC95        CTRC95
     C*
     C                   ADD       1             CTRC95
     C*
     C                   Z-ADD     *ZERO         CT2
     C                   MOVE      *BLANK        CT3
     C                   MOVEL(P)  HPCLMT        Y8DATA
     C                   WRITE     PY801
     C                   ADD       1             TOTREC
     C                   ENDSR
     C*================================================================
     C* LDTRTR - Load transmission trailer record.
     C*        - Record 099
     C*================================================================
     C     LDTRTR        BEGSR
     C*
     C                   CLEAR                   TRNTR
     C                   MOVEL(P)  '099'         TTRTYP
     C*
     C* ADD CHARGES (099-02) & COUNT # OF CLAIMS (099-03) IN FAEOP
     C*
     C                   Z-ADD     totEOCHRG     TTTCOF
     C                   Z-ADD     totEO_Cnt     TTTC
     C*
     C* TOTAL # OF RECORDS IN THIS FILE
     C*
     C     TOTREC        ADD       1             TTTR
     C*
     C                   Z-ADD     *zero         TTTB
     C*
     C                   Z-ADD     *ZERO         TT1
     C                   MOVE      *BLANK        TT2
     C*
     C                   MOVEL(P)  TRNTR         Y8DATA
     C                   WRITE     PY801
     C                   ENDSR
     C*================================================================
     C* UPDATR - UPDATE FAEAP: EANSNT FROM 'C' (PENDED AND READY TO SEN
     C*          SEND) TO 'S' (SENT TO COVENTRY)
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  'S'           E2NSNT
     C                   Z-ADD     YYMD          E2NPDT
     C                   MOVEL(P)  Q1PGM         E2PGM
     C                   Z-ADD     CYMD          E2DT
     C                   Z-ADD     DSTIME        E2TIME
     C                   UPDATE    PEA01
     C                   END
     C*
     C                   ENDSR
     C*================================================================
     C* WrFIY7P - Write FIY7P for Transmission to Coventry History
     C*================================================================
     C     WrFIY7P       BEGSR
     C*
     C                   movel(P)  @@filn        y7fnam
     C                   move      *zero         y7bnum
     C                   z-add     cymd          y7uldt
     C                   z-add     dstime        y7ultm
     C                   Z-ADD     totEO_Cnt     y7nbil
    C*
    C* load maintenance fields
     C                   movel     'A'           y7stat
     C                   movel     q1user        y7user
     C                   movel     q1pgm         y7pgm
     C                   movel     q1user        y7iusr
     C                   movel     q1pgm         y7ipgm
     C                   z-add     cymd          y7dt
     C                   z-add     cymd          y7idt
     C                   eval      y7time = dstime
     C                   eval      y7itim = dstime
    C*
     C                   write     py701
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
