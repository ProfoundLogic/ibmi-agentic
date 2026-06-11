     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      *
      ****************************************************************
      *  F8681R - Update Production Files
      *
      *  DESCRIPTION: This pgm will update the production files with
      *               Repriced data from Coventry.
      *               This will also create a check request.
      *
      *  CREATED    : 5/19/2008  PROGRAMMER: 47    PROJECT: 129MPN
      *
      *  CALLED BY  : F8681C
      *  CALLS TO   : FA700R  - Translate WC-GROUP into MC-SUB#/GROUP
      *               WA023R1 - API for determining Bank Code
      *
      ****************************************************************
      *  REVISIONS:
      *
      * 05/10/10 C:220 SUPPORT 72166
      *                Change program to be compitable with Coventry
      *                process
      *
      *
      ****************************************************************
     F*
     F* Coventry's Repriced Claim file
     FFIY0P     UF   E             DISK
     F* Transmission History record
     FFIY9L1    UF   E           k DISK
     F*
     F* faeop by co/sub/eob
     FFAEOL3    UF   E           K DISK
     F*
     F* faeap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN
     FFAEAL1    UF   E           K DISK
     F*
     F* fdcap by CO#/SUB#/FND/FYR/CASE/EOB#/RCN/DSEQ
     FFDCAL13   uf   E           k DISK
     F*
     F* detail data
     FFDDAL4    uf   E           k DISK
     F*
     F* holds Coventry's messages
     FFAEEL3    uf a E           k DISK
     F*
     F* EOB History
     FFAESP     o    E             DISK
     F* ---
     F* Check Request file
     FWARPLN    IF A E           K DISK
     FWARPL     IF   E           K DISK
     F                                     RENAME(PRP01:PRP02)
     FWMCIL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM03)
     FWMNRL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PNA01)
     F                                     IGNORE(PPA01)
     F                                     IGNORE(PCA01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTPSL     IF   E           K DISK
     FftxmL     IF   E           K DISK
     D*****************************************************************
     D/COPY SCOPYWW01I
     D*
     D*****************************************************************
     D* Program constants.
     D*****************************************************************
     D COMAPI          C                   CONST('FA007R')
     D xcReas          C                   CONST('Received from Coventry')
     D TRANS           C                   CONST('Transaction ID:')
     D USERID          C                   CONST('PDSPGMR   ')
     D complete        C                   CONST('Completed by Coventry upload')
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  DATA                   1     50
     D  QXTRDT                 1      7  0
     D  QXFRDT                 8     14  0
     D  QXTODT                15     21  0
     D  QXTRCD                22     23
     D  QXRECD                24     25
     D  QXPV1                 26     27
     D  QXPV2                 28     29
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3
     D  K1HCPC                 4      8
     D  K1UNTS                 9     11
     D*
     D $$APPL          s              2
     D $$CASE          s              7
     D $$CKTP          s             10
     D $$CO#           s              3
     D $$DATA          s             50
     D $$DATE          s              7
     D $$FND           s              3
     D $$FYR           s              3
     D $$GRP           s              3
     D $$MODE          s              1
     D $$PCDE          s              2
     D $$RCDE          s              2
     D $$RTCD          s              1
     D lines           s              3  0
     D sequence        s             20    varying
     D headerSeq#      s              3  0
     D productCode     s              5
     D lineDseq        s                   like(cadseq) dim(999)
     D linePaid        s                   like(capaid) dim(999)
     D line            s              3  0
     D skip            s               n
     D sprinkle        s               n
     D completed       s               n
     D nonNetwork      s               n
     D deleteNotes     s               n
     D noteDate        s             10
     D pos             s              3  0
     D* ------------------------
     D* Coventry's Repriced Data
     D*
     D                 DS                  INZ
     D  Y0DATA                 1    550
     D  Y0RTYP                 1      3
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
     D*
     D* Record 002
     D CLMHD           DS                  INZ
     D  CHRTYP                 1      3  0
     D  CHBNUM                 4     23
     D  CHTTYP                24     27
     D  CHSSGN                28     47
     D  CHCCNT                48     67
     D  CHCCNS                68     87
     D    Ceob1               68     76  0
     D    Crcn1               77     79  0
     D    CBLANK              80     85
     D    Csub1               68     72  0
     D    Cfnd1               73     75  0
     D    Cfyr1               76     78  0
     D    Ccase1              79     85  0
     D  CHCCNE                88    107
     D  CHCCNC               108    127
     D    Ceob2              108    116  0
     D    Crcn2              117    119  0
     D    Csub2              108    112  0
     D    Cfnd2              113    115  0
     D    Cfyr2              116    118  0
     D    Ccase2             119    125  0
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
     D*
     D* Record 010
     D HPPROV          DS                  INZ
     D  PRRTYP                 1      3  0
     D  PRBNUM                 4     23
     D  PRFEIN                24     38
     D    cfein               24     32  0
     D    cmod                33     35  0
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
     D*
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
     D* accident date
     D  PA3                  277    296
     D  PAMRN                297    313
     D  PA4                  314    338
     D  PA5                  339    349
     D  PA6                  350    369  0
     D  PA7                  370    389
     D*
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
     D*
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
     D*
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
     D*
     D* Record 035
     D HCLMTR          DS                  INZ
     D  HTRTYP                 1      3  0
     D  HTBNUM                 4     23
     D  HTCONT                24     27  0
     D  HTRC31                28     31  0
     D  HT1                   32     35  0
     D  HTRC33                36     39  0
     D  HTRC34                40     43  0
     D  HTTS                  44     54  0
     D  HTTU                  55     63  0
     D  HTTWSA                64     74  2
     D  HTTCC                 75     85  0
     D  HTTNCC                86     96  0
     D  HTTN                  97    107  2
     D  HTTCF                108    118  0
     D  HT2                  119    138
     D  HT3                  139    158  0
     D  HT4                  159    178
     D*
     D* Record050
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
     D*
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
     D  PSS                   76     84  2
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
     D*
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
     D*
     D* Record 095
     D MESSAG          DS                  INZ
     D  MSRTYP                 1      3  0
     D  MSBNUM                 4     23
     D  MSSEQ                 24     24  0
     D  MST                   25     64
     D  MSC                   65     69
     D  MS1                   70     89  0
     D  MS2                   90    129
     D*
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
     D* ------------------------
     IPCM03
     I              CMOSCL                      CWOSCL
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@dcid
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
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
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
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     C                   KFLD                    kkDSEQ
     C*
     C     CAKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     C*
     C     EAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkFND
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    kkEOB#
     C                   KFLD                    kkRCN
     C*
     C     EOKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    kkEOB#
     *
     C     RPKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKEOBS
     *
     C     RPKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     C*
     C     NRKY          KLIST
     C                   KFLD                    eoCO#
     C                   KFLD                    eofein
     C                   KFLD                    eomod
     *
     C     CMKY4         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    kkFYR
     C                   KFLD                    kkCASE
     *
     C     CIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKWCF#
     C                   KFLD                    KKPTYP
     C                   KFLD                    KKSYS
     *
     C     TBKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKKEY
     *
     C     XMKY          KLIST
     C                   KFLD                    KKCODE
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOsub#        KKsub#
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOEOB#        svEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C     *LIKE         DEFINE    EOfein        KKfein
     C     *LIKE         DEFINE    EOdt          xxDT
     C     *LIKE         DEFINE    EOtime        xxTime
     C     *LIKE         DEFINE    EOmod         KKmod
     C     *LIKE         DEFINE    EOuser        user
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        xxDSEQ
     C     *LIKE         DEFINE    CADSEQ        svDSEQ
     C     *LIKE         DEFINE    CAlin#        kklin#
     C     *LIKE         DEFINE    CAlin#        xxlin#
     C     *LIKE         DEFINE    easinv        xxsinv
     C     *LIKE         DEFINE    eondis        headerSavings
     C     *LIKE         DEFINE    eondis        xSprinkleAmt
     C     *LIKE         DEFINE    eondis        xSprinkleExtra
     C     *LIKE         DEFINE    eondis        headerPaid
     C     *LIKE         DEFINE    candis        detailPaid
     C     *LIKE         DEFINE    y9dcid        @@dcid
     C     *LIKE         DEFINE    y9dcid        xxdcid
     C     *LIKE         DEFINE    RPEOBS        KKEOBS
     C     *LIKE         DEFINE    PCKEY         KKKEY
     C     *LIKE         DEFINE    ciptyp        KKptyp
     C     *LIKE         DEFINE    cisys         KKsys
     C     *LIKE         DEFINE    cmfnd         KKwcf#
     C     *LIKE         DEFINE    rpseq#        xxseq#
     C     *LIKE         DEFINE    xmcode        kkcode
     C*================================================================
     C*                MAIN SUBROUTINE
     C*================================================================
     C*
     C                   EXSR      PGMSTR
     C*
     C                   dou       *in98 = *on
     C                   read      py001                                  98
     C                   if        *in98 = *off and y0prfl<>'Y'
     C*
     C* Determine the record format and load the data structure.
     C*
     C                   if        Y0RTYP = '002'
     C                   exsr      xclmhd
     C                   endif
     C*
     C                   if        Y0RTYP = '03B'
     C                   exsr      xhclhdb
     C                   endif
     C*
     C                   if        Y0RTYP = '050'
     C                   exsr      xpclmhd
     C                   endif
     C*
     C                   if        xxbtyp ='H' or xxbtyp='N'
     C                   select
     c                   when      xxbtyp = 'H'
     c                   exsr      processHosp
     c                   when      xxbtyp = 'N'
     c                   exsr      processPhys
     c                   endsl
     C                   endif
     C*
     C                   if        Y0RTYP = '095'
     C                   exsr      xMESSAG
     C                   endif
     C*
     C                   if        Y0RTYP = '097'
     C                   exsr      xHPCLMT
     C                   endif
     C*
     C                   EXSR      UPDAT0
     C                   endif
     C                   enddo
     C*
     C* Update Transmission file
     C*
     C                   EXSR      UPDAT9
     C                   EXSR      PGMEND
     C*
     C*================================================================
     C* processHosp - Process Hospital Bills
     C*================================================================
     C     processHosp   BEGSR
     C*
     C                   if        Y0RTYP = '031'
     C                   exsr      xhinpat
     C                   endif
     C*
     C                   if        Y0RTYP = '033'
     C                   exsr      xhoutpa
     C                   endif
     C*
     C                   if        Y0RTYP = '035'
     C                   exsr      xhclmtr
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* processPhys - Process Physician Bills
     C*================================================================
     C     processPhys   BEGSR
     C*
     C                   if        Y0RTYP = '056'
     C                   exsr      xpsvc
     C                   endif
     C*
     C                   if        Y0RTYP = '059'
     C                   exsr      xpclmtr
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* calcSprinkle - calculates the sprinkle amount
     C*================================================================
     C     calcSprinkle  BEGSR
     C*
     C*
     C* Get number of lines in eob that pay.
     C*
     C                   z-add     *zero         xxPGT0            5 0
     C     caky1         setll     pca01
     C                   dou       *in94 = *on
     C     caky1         reade     pca01                                  94
     C                   if        *in94 = *off
     C*
     C                   if        capaid > *zero
     C                   add       1             xxPGT0
     C                   end
     C*
     C                   end
     C                   enddo
     C*
     C* Calc the new paid amount; Sprinkle Amount.
     C*   Claim level negotiated rate (that has been re-priced)
     C*   divided by the number of lines in the eob that pay
     C*   giving the new paid amount per line, detailPaid.
     C*
     C                   z-add     *zero         xSprinkleAmt
     C                   z-add     *zero         xSprinkleExtra
     C*
     C                   if        headerPaid <> *zero
     C                             and xxPGT0 <> *zero
     C     headerPaid    div       xxPGT0        xSprinkleAmt
     C                   mvr                     xSprinkleExtra
     C                   end
     C*
     C                   z-add     xSprinkleAmt  detailPaid
     C*
     C                   endsr
     C*================================================================
     C* xLoadKeys - Use the fields from rcd 002 to load the key fields.
     C*================================================================
     C     xLoadKeys     BEGSR
     C*
     C                   if        cblank=*blanks
     C                   z-add     csub2         ##sub#
     C                   z-add     csub2         kksub#
     C                   z-add     cfnd2         kkfnd
     C                   z-add     cfyr2         kkfyr
     C                   z-add     ccase2        kkcase
     C                   z-add     ceob1         kkeob#
     C                   z-add     crcn1         kkrcn
     C                   else
     C                   z-add     csub1         ##sub#
     C                   z-add     csub1         kksub#
     C                   z-add     cfnd1         kkfnd
     C                   z-add     cfyr1         kkfyr
     C                   z-add     ccase1        kkcase
     C                   z-add     ceob2         kkeob#
     C                   z-add     crcn2         kkrcn
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C* xCLMHD - 002 Load data from claim header
     C*================================================================
     C     xCLMHD        BEGSR
     C*
     C                   movel(P)  Y0data        clmhd
     C                   exsr      xLoadKeys
     C                   MOVEL(P)  chccno        xxsinv
     C                   z-add     *zero         xSprinkleAmt
     C                   z-add     *zero         xSprinkleExtra
     C                   eval      sprinkle=*off
     C                   eval      nonNetwork=*off
     c                   eval      headerSeq#=0
     c                   clear                   lineDseq
     c                   clear                   linePaid
     c                   eval      line=0
     C*
     C* get the bill type
     C*
     C                   select
     C                   when         chbtyp = '001'
     C                             or chbtyp = '002'
     C                   movel(P)  'H'           xxbtyp            1
     C*
     C                   when         chbtyp = '003'
     C                   movel(P)  'N'           xxbtyp
     C                   endsl
     C*
     C                   ENDSR
     C*================================================================
     C* xHCLHDB - 03B Hospital claim header
     C*================================================================
     C     xHCLHDB       BEGSR
     C*
     C                   movel(P)  Y0data        hclhdb
     C                   eval      productCode=hcpc
     C*
     C                   ENDSR
     C*================================================================
     C* xHINPAT - 031 Load data
     C*================================================================
     C     xHINPAT       BEGSR
     C*
     C                   movel(P)  Y0data        hinpat
     C                   eval      line=line+1
     C                   z-add     hiseq         lineDseq(line)
     C                   z-add     hinr          linePaid(line)
     C*
     C                   ENDSR
     C*================================================================
     C* xHOUTPA - 033 Load data
     C*================================================================
     C     xHOUTPA       BEGSR
     C*
     C                   movel(P)  Y0data        houtpa
     C                   eval      line=line+1
     C                   z-add     hoseq         lineDseq(line)
     C                   z-add     honr          linePaid(line)
     C*
     C                   ENDSR
     C*================================================================
     C* xHCLMTR - 035 Load data
     C*================================================================
     C     xHCLMTR       BEGSR
     C*
     C                   movel(P)  Y0data        hclmtr
     C                   z-add     httn          headerPaid
     C                   z-add     htts          headerSavings
     C*
     C                   ENDSR
     C*================================================================
     C* xPCLMHD - 050 Physician claim header
     C*================================================================
     C     xPCLMHD       BEGSR
     C*
     C                   movel(P)  Y0data        pclmhd
     C                   eval      productCode=pcpc
     C*
     C                   ENDSR
     C*================================================================
     C* xPSVC  - 056 Load data
     C*================================================================
     C     xPSVC         BEGSR
     C*
     C                   movel(P)  Y0data        psvc
     C                   eval      line=line+1
     C                   z-add     psseq         lineDseq(line)
     C                   z-add     psnr          linePaid(line)
     C*
     C                   ENDSR
     C*================================================================
     C* xPCLMTR - 059 Load data from Physician Claim Trailer
     C*================================================================
     C     xPCLMTR       BEGSR
     C*
     C                   movel(P)  Y0data        pclmtr
     C                   z-add     pttn          headerPaid
     C                   z-add     ptts          headerSavings
     C*
     C                   ENDSR
     C*================================================================
     C* xMESSAG - 095 Load messages from Coventry
     C*================================================================
     C     xMESSAG       BEGSR
     C*
     C                   movel(P)  Y0data        messag
     C* ---
     C* write FAEEP
     C                   clear                   pee01
     C                   move      ##co#         eeco#
     C                   move      kksub#        eesub#
     C                   move      kkfnd         eefnd
     C                   move      kkfyr         eefyr
     C                   move      kkcase        eecase
     C                   move      kkeob#        eeeob#
     C                   eval      eercn = kkrcn
     C                   move      eoss#         eess#
     C                   z-add     eoacdt        eeacdt
     C*
     C                   if        msc='CCMP'
     C                   eval      pos=%scan('/':mst:1)
     C                   if        pos<>0
     C                   eval      noteDate=%subst(mst:pos-2:10)
     C                   endif
     C                   endif
     C
    C*
    C* Load maintenance fields
     C                   movel     'A'           eestat
     C                   movel     q1user        eeuser
     C                   movel     q1pgm         eepgm
     C                   movel     q1user        eeiusr
     C                   movel     q1pgm         eeipgm
    C*
     C                   z-add     xxdt          eedt
     C                   z-add     xxdt          eeidt
     C                   eval      eetime = xxtime
     C                   eval      eeitim = xxtime
    C*
     C                   eval      kkcode=msc
     C     xmky          chain     pxm01                              99
     c                   dow       not *in99
     C                   movel(P)  xmdesc        eenot1
     c*
     C                   if        msc='CCMP'
     C                   eval      pos=%scan('*V':eenot1:1)
     C                   if        pos<>0
     C                   eval      %subst(eenot1:pos:10)=noteDate
     C                   endif
     C                   endif
     c*
     C                   add       1             headerSeq#
     C                   z-add     headerSeq#    eeseq#
     C                   write     pee01
     C     xmky          reade     pxm01                                  99
     C                   enddo
     C*
     C*  Is this a sprinkle bill?
     C*
     C                   if        (productCode='WCPPO' and (msc='EPFH' or
     C                              msc='EPHI' or msc='EPTN')) or
     C                             (productCode='WCCMI' and msc='CCMP') or
     C                             (productCode='WCCMO' and msc='CCMP')
     C                   eval      sprinkle=*on
     C                   endif
     C*
     C*  Is this a non-network bill>
     C*
     C                   if        msc='FCAN' or msc='FI*S' or msc='FNCP' or
     C                             msc='FNMP' or msc='FNOC' or msc='H004' or
     C                             msc='OAEE' or msc='PMIS' or msc='G702'
     C                   eval      nonNetwork=*on
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C* xHPCLMT - 097 Load data from Hosp/Phys trailer
     C*================================================================
     C     xHPCLMT       BEGSR
     C*
     C                   exsr      upFAEAP
     C                   if        completed
     C                   exsr      WrFAESP
     C                   exsr      WrtRPP
     C                   endif
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        xxTIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         xxDT
     C*
 001 C                   MOVEL(P)  USERID        USER
     C                   z-add     777           ##CO#
     C                   CALL      COMAPI        COMPL                  99
     C*
     C                   MOVE      *blank        xUpdat9           1
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT0 - Update Repriced Claim File
     C*================================================================
     C     UPDAT0        BEGSR
     C*
     C                   MOVEL(P)  'Y'           y0PRFL
     C
     C                   UPDATE    Py001
     C*
     C                   MOVEL(P)  'Y'           xUpdat9
     C*
     C                   ENDSR
     C*================================================================
     C* UPDAT9 - Update Transmission History File (FIY9P).
     C*================================================================
     C     UPDAT9        BEGSR
     C*
     C                   if        xUpdat9 = 'Y'
     C*
     C                   MOVEL(P)  @@dcid        xxdcid
     C     xxdcid        CHAIN     PY901                              95
     C     *IN95         IFEQ      *OFF
     C*
     C                   MOVEL(P)  xUpdat9       Y9PRFL
     C                   MOVEL(P)  Q1USER        Y9USER
     C                   MOVEL(P)  Q1PGM         Y9PGM
     C                   Z-ADD     xxTIME        Y9TIME
     C                   z-add     xxDT          Y9DT
     C*
     C                   UPDATE    PY901
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* UpFAEAP - Update FAEAP & FAEOP
     C*================================================================
     C     UpFAEAP       BEGSR
     C*
     C                   eval      skip=*on
     C                   eval      completed=*off
     C                   eval      deleteNotes=*off
     C     eaky          chain     pea01                              99
     C                   if        *in99 = *off and eaepfl='E'
     C*
     C                   if        (not nonNetwork or
     C                             headerPaid<>0 or headerSavings<>0)
     C                             and eapaid < headerPaid
     C                   eval      deleteNotes=*on
     c                   endif
     C*
     C                   if        (not nonNetwork or
     C                             headerPaid<>0 or headerSavings<>0)
     C                             and eapaid > headerPaid
     C                   eval      skip=*off
     C                   z-add     headerPaid    eapaid
     C     eachrg        sub       eamra         eandis
     C                   sub       eaothr        eandis
     C                   sub       eaoth2        eandis
     C                   sub       eapaid        eandis
     C                   endif
     C*
     C                   eval      eaepfl=' '
     C                   eval      completed=*on
     C                   MOVEL(P)  xxsinv        easinv
     C                   MOVEL(P)  'P'           eansnt
    C*
     C                   movel(P)  q1user        eauser
     C                   movel(P)  q1pgm         eapgm
     C                   z-add     xxdt          eadt
     C                   z-add     xxtime        eatime
     C                   update    pea01
     C                   endif
     C*
     C* update FAEOP
     C*
     C                   if        completed
     C*
     C     eoky          chain     peo01                              99
     C                   if        *in99 = *off
     C*
     C                   if        not skip
     C                   z-add     headerPaid    eopaid
     C     eochrg        sub       eomra         eondis
     C                   sub       eoothr        eondis
     C                   sub       eooth2        eondis
     C                   sub       eopaid        eondis
     C                   endif
     C*
     C                   MOVEL(P)  xxsinv        eosinv
     C                   MOVEL(P)  'C'           eoblst
     C                   z-add     eoeob#        eochk#
    C*
     C                   eval      eontwk='CVT'
    C*
     C                   movel(P)  q1pgm         eopgm
     C                   z-add     xxdt          eodt
     C                   z-add     xxtime        eotime
     C                   update    peo01
     C                   endif
     C*
     C                   if        deleteNotes
     C     eeky          chain     pee01                              99
     C                   dow       not  *in99
     C                   delete    pee01
     C     eeky          reade     pee01                                  99
     c                   enddo
     c                   endif
     C*
     C                   if        sprinkle
     C                   exsr      calcSprinkle
     C                   exsr      UpFDCAPS
     C                   exsr      UpFDDAPS
     C                   else
     C                   exsr      UpFDCAPN
     C                   exsr      UpFDDAPN
     C                   endif
     C*
     C                   endif
     C*
     C                   endsr
     C*================================================================
     C* WrFASEP - Write FAESP for EOB History
     C*================================================================
     C     WrFAESP       BEGSR
     C*
     c                   clear                   pes01
     C                   move      ##co#         esco#
     C                   move      kksub#        essub#
     C                   move      kkfnd         esfnd
     C                   move      kkfyr         esfyr
     C                   move      kkcase        escase
     C                   move      kkeob#        eseob#
     C                   move      kkrcn         esrcn
     C                   move      eoss#         esss#
     C                   z-add     eoacdt        esacdt
     C                   movel(P)  'C'           esblst
     C                   movel(P)  complete      esreas
    C*
    C* load maintenance fields
     C                   movel     'A'           esstat
     C                   movel     q1user        esuser
     C                   movel     q1pgm         espgm
     C                   movel     q1user        esiusr
     C                   movel     q1pgm         esipgm
     C                   z-add     xxdt          esdt
     C                   z-add     xxdt          esidt
     C                   eval      estime = xxtime
     C                   eval      esitim = xxtime
    C*
     C                   write     pes01
     C*
     C                   ENDSR
     C*================================================================
     C*                ======Non-Sprinkle========
     C* UpFDCAPN - Update FDCAP
     C*================================================================
     C     UpFDCAPN      BEGSR
     C*
     C     1             do        999           x                 3 0
     C                   if        lineDseq(x)=0
     C                   leave
     C                   else
     C                   eval      xxdseq=lineDseq(x)
     C                   eval      kkdseq=lineDseq(x)
     C                   endif
     C     caky          chain     pca01                              99
     C                   if        *in99 = *off
     C*
     C* determine network discount
     C*
     C                   if        not skip
     C                   z-add     linePaid(x)   capaid
     C     cachrg        sub       camra         candis
     C                   sub       caothr        candis
     C                   sub       caoth2        candis
     C                   sub       capaid        candis
     C                   endif
     C*
     C                   z-add     caeob#        cachk#
     C******* BEG ADD ***** 05/10/10 **********************************
     C                   MOVE      *BLANK        CAFLAG
     C******* END ADD ***** 05/10/10 **********************************
     C*
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   Z-ADD     xxTIME        CATIME
     C                   Z-ADD     xxDT          CADT
     C*
     C                   update    PCA01
     C                   endif
     C                   enddo
     C*
     C                   ENDSR
     C*================================================================
     C*                ======Non-Sprinkle========
     C* UpFDDAPN  - Update FDDAP for the repriced data
     C*================================================================
     C     UpFDDAPN      BEGSR
     C*
     C     1             do        999           x                 3 0
     C                   if        lineDseq(x)=0
     C                   leave
     C                   else
     C                   eval      xxdseq=lineDseq(x)
     C                   eval      kkdseq=lineDseq(x)
     C                   endif
     C     caky          chain     pda01                              99
     C                   if        *in99 = *off
     C*
     C                   if        not skip
     C                   z-add     linePaid(x)   dapaid
     C     dachrg        sub       damra         dandis
     C                   sub       daothr        dandis
     C                   sub       daoth2        dandis
     C                   sub       dapaid        dandis
     C                   endif
    C*
    C* load maintenance fields
    C*
     C                   movel     'A'           dastat
     C                   movel     q1user        dauser
     C                   movel     q1pgm         dapgm
     C                   movel     q1user        daiusr
     C                   movel     q1pgm         daipgm
    C*
     C                   z-add     xxdt          dadt
     C                   z-add     xxdt          daidt
     C                   eval      datime = xxtime
     C                   eval      daitim = xxtime
    C*
     C                   update    pda01
     C                   endif
     C                   enddo
     C*
     C                   ENDSR
     C*================================================================
     C*                ======Sprinkle========
     C* UpFDCAPS  - Update FDCAP
     C*================================================================
     C     UpFDCAPS      BEGSR
     C*
     C                   eval      lines=0
     C                   setoff                                       99
     C     caky1         setll     pca01
     C                   dou       *in99 = *on
     C     caky1         reade     pca01                                  99
     C                   if        *in99 = *off
     C*
     C                   if        capaid > *zero and not skip
     C                   z-add     detailPaid    capaid
     C                   add       1             lines
     C*
     C* add remainder portion of division to the first line of the bill
     C*
     C                   if        lines=xxPGT0
     C                   add       xSprinkleExtracapaid
     C                   endif
     C                   endif
     C*
     C* determine network discount
     C*
     C     cachrg        sub       camra         candis
     C                   sub       caothr        candis
     C                   sub       caoth2        candis
     C                   sub       capaid        candis
     C*
     C                   z-add     caeob#        cachk#
     C******* BEG ADD ***** 05/10/10 **********************************
     C                   MOVE      *BLANK        CAFLAG
     C******* END ADD ***** 05/10/10 **********************************
     C*
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   Z-ADD     xxTIME        CATIME
     C                   Z-ADD     xxDT          CADT
     C*
     C                   update    PCA01
     C                   endif
     C                   enddo
     C*
     C                   ENDSR
     C*================================================================
     C*                ======Sprinkle========
     C* UpFDDAPS  - Update FDDAP for the repriced data
     C*================================================================
     C     UpFDDAPS      BEGSR
     C*
     C                   eval      lines=0
     c                   setoff                                       99
     C     caky1         setll     pda01
     C                   dou       *in99 = *on
     C     caky1         reade     pda01                                  99
     C                   if        *in99 = *off
     C*
     C                   if        dapaid > *zero  and not skip
     C                   Z-ADD     detailPaid    dapaid
     C                   add       1             lines
     C*
     C* add remainder portion of division to the first line of the bill
     C*
     C                   if        lines=xxPGT0
     C                   add       xSprinkleExtradapaid
     C                   endif
     C                   endif
     C*
     C     dachrg        sub       damra         dandis
     C                   sub       daothr        dandis
     C                   sub       daoth2        dandis
     C                   sub       dapaid        dandis
    C*
    C* load maintenance fields
     C                   movel     'A'           dastat
     C                   movel     q1user        dauser
     C                   movel     q1pgm         dapgm
     C                   movel     q1user        daiusr
     C                   movel     q1pgm         daipgm
    C*
     C                   z-add     xxdt          dadt
     C                   z-add     xxdt          daidt
     C                   eval      datime = xxtime
     C                   eval      daitim = xxtime
    C*
     C                   update    pda01
     C                   endif
     C                   enddo
     C*
     C                   ENDSR
     *================================================================
     * WRTRPP - Write WARPP record
     *================================================================
     C     WRTRPP        BEGSR
     *
 003 * Let's get the WCMS group number
     *
 003 C                   MOVE      ##CO#         #1CO#
 003 C                   MOVE      eaSUB#        #1SUB#
 003 C                   MOVE      eaFND         #1FND
 003 *
 003 C                   CALL      'FA700R'
 003 C                   PARM      *BLANKS       ##RTCD            1
 003 C                   PARM                    #1CO#             3
 003 C                   PARM      *BLANKS       #1WCF#            3
 003 C                   PARM                    #1SUB#            5
 003 C                   PARM                    #1FND             3
 003 *
B004 C                   if        ##RTCD = *BLANKS
 004 C                   MOVE      #1WCF#        KKWCF#
X004 C                   ELSE
 004 C                   eval      KKWCF# = *ZEROS
E004 C                   ENDIF
     *
     *
     C                   eval      KKEOB# = EOEOB#
     C                   eval      KKEOBS = EORCN
     C     RPKY1         CHAIN     PRP01                              99
     C                   if        *IN99 = *ON
     *
     C     RPKY2         SETGT     PRP02
     C     RPKY2         READPE    PRP02                                  99
     C                   if        *IN99 = *OFF
     C                   eval      XXSEQ# = RPSEQ# + 1
     C                   ELSE
     C                   eval      XXSEQ# = 1
     C                   ENDIF
     *
     C                   CLEAR                   PRP01
     C                   eval      RPCO# = EOCO#
     C                   eval      RPFND = KKWCF#
     C                   eval      RPFYR = EOFYR
     C                   eval      RPCASE = EOCASE
     C                   eval      RPSEQ# = XXSEQ#
     C                   eval      RPTRDT = EOTRDT
     C                   eval      RPFEIN = EOFEIN
     C                   eval      RPMOD = EOMOD
     C                   eval      RPAMT = EAPAID
     *
     C     NRKY          CHAIN     PNR01                              99
     C*
     C                   if        *in99 = *OFF
     C                   MOVEL(P)  'G'           RPPGTP
     C                   end
     C*
     C     CMKY4         CHAIN     PCM03                              99
     C                   MOVEL     'P'           KKPTYP
     C                   MOVEL     EOSYS         KKSYS
     C     CIKY          CHAIN     PCI01                              99
     C                   if        *IN99 = *OFF
     *
     C                   if        CMCVFL = 'Y'
     C                   MOVE(P)   CIP1CV        RPBR2
     C                   ELSE
     C                   if        EOBTYP = 'X'
     C                   MOVE(P)   CIXBR2        RPBR2
     C                   ELSE
     C                   MOVE(P)   CIBR2         RPBR2
     C                   ENDIF
     C                   ENDIF
     *
     C                   if        CMLTFL = 'I'
     C                   MOVE(P)   CIBR7L        RPBR7
     C                   ELSE
     C                   MOVE(P)   CIBR7N        RPBR7
     C                   ENDIF
     *
     C                   MOVEL(P)  CISTS         RPSTS
     *
     C                   ELSE
     *
     C                   if        EOSYS = 'P'
     C                   MOVEL(P)  'MD'          RPBR2
     C                   ELSE
     C                   MOVEL(P)  'HP'          RPBR2
     C                   ENDIF
     *
     C                   if        CMLTFL = 'I'
     C                   MOVEL(P)  'LT'          RPBR7
     C                   ELSE
     C                   MOVEL(P)  'NL'          RPBR7
     C                   ENDIF
     *
     C                   MOVEL(P)  'PD'          RPSTS
     *
     C                   ENDIF
     *
     C                   MOVEL(P)  CMNCCI        RPNCCI
     C                   eval      RPFRDT = EOFRDT
     C                   eval      RPTODT = EOTODT
     C                   MOVEL(P)  'MP'          RPTRCD
     C                   MOVEL(P)  'P'           RPTYPE
     C                   MOVEL(P)  'M'           RPRETP
     C                   MOVEL(P)  'ME'          RPRECD
     *
     C                   MOVEL(P)  RPTRCD        KKKEY
     C     TBKY          CHAIN     PPC01                              99
     C                   if        *IN99 = *OFF
     C                   MOVEL(P)  PCDESC        RPDESC
     C                   ENDIF
     *
     C                   MOVEL(P)  EOPNAM        RPPNAM
     C                   MOVEL(P)  EOADR1        RPADR1
     C                   MOVEL(P)  EOADR2        RPADR2
     C                   MOVEL(P)  EOCTY         RPCTY
     C                   MOVEL(P)  EOST          RPST
     C                   eval      RPZIP = EOZIP
     C                   MOVEL     'P'           RPSTAT
     C                   MOVEL(P)  EOUSER        RPUSER
     C                   MOVEL(P)  EOPGM         RPPGM
     C                   eval      RPTIME = EOTIME
     C                   eval      RPDT = EODT
     C                   eval      RPEOB# = EOEOB#
     C                   eval      RPEOBS = EORCN
     C                   eval      RPRCVD = EODT
     *
     C     TRANS         CAT(P)    xxsinv:1      RPRMDS
     *
     C                   MOVEL(P)  RPSTS         KKKEY
     C     TBKY          CHAIN     PPS01                              99
     C                   if        *IN99 = *OFF
     C                   MOVE(P)   PSSRT         RPSORT
     C                   ENDIF
     *
     C                   MOVEL(P)  'N'           RPSENT
     C                   MOVEL(P)  USER          RPRQST
     *
     *  Call API to determine the Bank "Code".
     *
     C                   MOVE      RPCO#         $$CO#
     C                   MOVE      RPFND         $$FND
     C                   MOVE      RPFYR         $$FYR
     C                   MOVE      RPCASE        $$CASE
     C                   eval      QXTRDT = RPTRDT
     C                   eval      QXFRDT = RPFRDT
     C                   eval      QXTODT = RPTODT
     C                   MOVEL     RPTRCD        QXTRCD
     C                   MOVEL     RPRECD        QXRECD
     C                   MOVEL     RPBR2         QXPV1
     C                   MOVEL     RPBR7         QXPV2
     *
     C                   CALL      'WA023R1'
     C                   PARM      '2'           $$MODE                         Mode
     C                   PARM                    $$CO#                          Company
     C                   PARM                    $$FND                          Group #
     C                   PARM                    $$FYR                          Year
     C                   PARM                    $$CASE                         Case
     C                   PARM      'WC'          $$APPL                         Applic Code
     C                   PARM      DATA          $$DATA                         Selection criteria
     C                   PARM      *BLANKS       $$CKTP                         Bank Acct Code
     C                   PARM      *BLANKS       $$RTCD                         Return Code
     *
     C                   MOVEL(P)  $$CKTP        RPCKTP                         Bank Acct# Code
     *
     C                   MOVEL(P)  EAIUSR        RPIUSR
     C                   MOVEL(P)  EAPGM         RPIPGM
     C                   eval      RPITM = EAITIM
     C                   eval      RPIDT = EAIDT
     *
     C                   if        RPAMT > *ZEROS
     C                   WRITE     PRP01
     C                   ENDIF
     *
     C                   ENDIF
     *
     C                   ENDSR
