     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h** ======================================================================================== **
    h/TITLE W8562R - Zurich Interface Process.
     h option(*nodebugio:*srcstmt)
     H DFTACTGRP(*NO) ACTGRP(*caller)
    h* ========================================================================================= **
    h*  DESCRIPTION:  This program reads the import file Group Auxiliary File(WDF2P).             *
    h*                It also reads the extract files and based on the TPA code it compares       *
    h*                with history files. It will also generate a summary file WXZUP.             *
    h*                                                                                            *
    h*                This program can process up to five record formats for each claim.          *
    h*                                                                                            *
     h*  Called from:  W8562C                                                                      *
     h*                                                                                            *
     h*  Created: 06/25/09 by C:220 for project: 114WTR                                            *
    h** ======================================================================================== **
    h* Revisions:                                                                                 *
     h*
     h* 10/12/17   c:062 Project: 114WIZ
     h*                  Use the edi part code.
     h*
     h* 10/27/10   c:220 Support: 74627
     h*                 - Fix the total sign for all Amounts and Totals.
     h*
     h* 09/29/2010 c:62 Project: 74347
     h*                 Update the last run date data area with month-end dt
     h*
    h** ======================================================================================== **
      *                                                                                            *
     f* Zurich Extract files.
      *                                                                                            *
     fWZZ0P     uf a e             disk
     fWZZ0l     uf a e           k disk    rename(PZ001:PZ002)
     fWZZ1P     uf a e             disk
     fWZZ1l     uf a e           k disk    rename(PZ101:PZ102)
     fWZZ2P     uf a e             disk
     fWZZ2l     uf a e           k disk    rename(PZ201:PZ202)
     fWZZ3P     uf a e             disk
     fWZZ3l     uf a e           k disk    rename(PZ301:PZ302)
     fWZZ4P     uf a e             disk
     fWZZ4l     uf a e           k disk    rename(PZ401:PZ402)
     fWZZ5P     uf a e             disk
     fWZZ5l     if   e           k disk    rename(PZ501:PZ502)                   tpa,clm#
      *
     f* Zurich Extract files - HISTORY
      *
     fWZZ0PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd00)
     fWZZ1PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd10)
     fWZZ2PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd20)
     fWZZ3PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd30)
     fWZZ4PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd40)
     fWZZ5PH    if   e             disk    prefix(H_)
     f                                     infds(dsrcd50)
      *
      * Group Auxiliary File (co#/fnd)
     fwdf2l     if   e           k disk
     Fwxgsl     if   e           k disk
      *
      * Company master file CO#
     fsmcol     if   e           k disk
      *
      * Claim Master file (co#/fnd/fyr/case)
     fwmcml1    if   e           k disk
      *
      * TPA Contact Master Information
     fwmtcp     if   e           k disk
      *
      * Claim Payment Detail file  (co#/fnd/fyr/case/seq#)
     fwdpal     if   e           k disk
      *
      * Claim Check View file  (co#/fnd/fyr/case/seq#)
     fwdrpl3    if   e           k disk
      *
      * Claimnant Demographic file (SS#)
     fwmcdl     if   e           k disk
      *
      * Claim Status History file  (co#/fnd/fyr/case/efdt/time)
     fwmchl     if a e           k disk
      *
      * Employer Master file (co#/fnd/emp#/div)
     fwmeml     if   e           k disk
      *
      * Employer Detail file (co#/fnd/emp#/div/fyr)
     fwdell     if   e           k disk
     fwmepl     if   e           k disk
      *
      * Claim Legal Master file  (co#/fnd/fyr/case)
     fWMLLL     if   e           k disk
     F*   Users by staf code.
     FWMUPL     if   e           k disk
      *
     F* co/fnd/fyr/case/frdt
     F*****WDPALK    if   e           k disk    rename(ppa01:ppa55)
      *
     F* CO/EOB#/SS#/ACDT/EOB#/PNAM
     FFAEOL19   if   e           k disk
      *
     F* COMPANY CONT INFORMATION
     Fsmccl     if   e           k disk
      *
     F* GROUP   CONT INFORMATION
     Fwmfcl     if   e           k disk
      *
     F* Zurich clamins export file
     Ffxzup     uf a e             disk
     F* Fund master file
     Fwmfnl     if   e           k disk
     F* CO#/SUB#/FND/FYR/CASE/EOB#
     Ffaeul     if   e           k disk
     F* co/fnd/fyr/case/trdt/seq
     Fwdpal5    if   e           k disk    rename(ppa01:ppa05)
      *
      *
      * Table files (co#/key)
     fwtabl     if   e           k disk    rename(ppa01:ppa02)
     f                                     ignore(pat01)
     f                                     ignore(pba01)
     f                                     ignore(pbc01)
     f                                     ignore(pbs01)
     f                                     ignore(pb701)
     f                                     ignore(pcc01)
     f                                     ignore(pcn01)
     f                                     ignore(pcs01)
     f                                     ignore(phy01)
     f                                     ignore(pid01)
     f                                     ignore(pis01)
     f                                     ignore(plr01)
     f                                     ignore(pnc01)
     f                                     ignore(prc01)
     f                                     ignore(prs01)
     f                                     ignore(psp01)
     f                                     ignore(psy01)
     f                                     ignore(ptr01)
     f                                     ignore(ptt01)
     f                                     ignore(pvs01)
      *=============================================================================================
      * Stand alone fields
      *=============================================================================================
     d xceam01         s             16  2 inz(*zeros)
     d xceam02         s             16  2 inz(*zeros)
     d xceam03         s             16  2 inz(*zeros)
     d xceam04         s             16  2 inz(*zeros)
     d xceam05         s             16  2 inz(*zeros)
     d xceam06         s             16  2 inz(*zeros)
     d xceam07         s             16  2 inz(*zeros)
     d xceam08         s             16  2 inz(*zeros)
     d xceam09         s             16  2 inz(*zeros)
     d xceam10         s             16  2 inz(*zeros)
     d xctam01         s             16  2 inz(*zeros)
     d xctam02         s             16  2 inz(*zeros)
     d xctam03         s             16  2 inz(*zeros)
     d xctam04         s             16  2 inz(*zeros)
     d xctam05         s             16  2 inz(*zeros)
     d xctam06         s             16  2 inz(*zeros)
     d xctam07         s             16  2 inz(*zeros)
     d xctam08         s             16  2 inz(*zeros)
     d xctam09         s             16  2 inz(*zeros)
     d xctam10         s             16  2 inz(*zeros)
     d xxcnm1          s             30
     d xxcnm2          s             30
     d xxcnm3          s             30
     d xxttl1          s             30
     d xxttl2          s             30
     d xxttl3          s             30
     d xxcph1          s             10  0 inz(*zeros)
     d xxcph2          s             10  0 inz(*zeros)
     d xxcph3          s             10  0 inz(*zeros)
     d xxtrdt          s              8
     d xxmco           s              2
     d xxjrst          s              2
     d x4pkcd          s              5
     d x4fein          s             12  0 inz(*zeros)
     d xxage           s              3  0 inz(*zeros)
     d xxcin           s              1
     d xxcsts          s              1
     d x0ind           s              1    inz('A')
     d x1ind           s              1    inz('A')
     d x2ind           s              1    inz('A')
     d x3ind           s              1    inz('A')
     d x4ind           s              1    inz('A')
     d x5ind           s              1    inz('A')
     d xxcntr          s              9  0 inz(*zeros)
     d xxco#           s              3  0 inz(*zeros)
     d xxfnd           s              3  0 inz(*zeros)
     d xxceam          s             16  2 inz(*zeros)
     d xxcea           s             16  2 inz(*zeros)
     d xxceam1         s             16  2 inz(*zeros)
     d xxceam2         s             16  2 inz(*zeros)
     d xxceam3         s             16  2 inz(*zeros)
     d xxceam4         s             16  2 inz(*zeros)
     d xxceam5         s             16  2 inz(*zeros)
     d xxceam6         s             16  2 inz(*zeros)
     d xxceam7         s             16  2 inz(*zeros)
     d xxceam8         s             16  2 inz(*zeros)
     d xxceam9         s             16  2 inz(*zeros)
     d xxceam10        s             16  2 inz(*zeros)
     d x3cpda          s             16  2 inz(*zeros)
     d x3ctra          s             16  2 inz(*zeros)
     d svfnd           s              3  0 inz(*zeros)
     d*
     d xxseq10         s              9  0 inz(*zeros)
     d xxseq20         s              9  0 inz(*zeros)
     d xxseq30         s              9  0 inz(*zeros)
     d xxseq40         s              9  0 inz(*zeros)
     d xxseq50         s              9  0 inz(*zeros)
     d xxzeros         s             19    inz(*blanks)
     d xxtpac          s              5s 0 inz(00204)
     d kkefdt          s                   like(chefdt) inz(9999999)
     d xxlrdt          s                   like(chefdt)
     d xxmedt          s                   like(chefdt)
     d*
     ddsrcd00          ds
     d dsrcd0                156    159i 0
     d*
     ddsrcd10          ds
     d dsrcd1                156    159i 0
     d*
     ddsrcd20          ds
     d dsrcd2                156    159i 0
     d*
     ddsrcd30          ds
     d dsrcd3                156    159i 0
     d*
     ddsrcd40          ds
     d dsrcd4                156    159i 0
     d*
     ddsrcd50          ds
     d dsrcd5                156    159i 0
     d*
     d rcdCount        s              7  0 inz(*zero)
     d recType         s              2    inz
      *
     d userName        s             50
     d UPUSNM          s                   like(userName)
     d xxclm           s             13
     d srdti           s              8  0
     d srdti1          s              8
     d srdto           s             10
     d srerr           s              1
     d srffmt          s              8
     d srtfmt          s              8
     d write2030       s               n
      *
     d Up              c                   'ABCDEFGHIJKLMNOPQRS-
     d                                     TUVWXYZ'
     d Lo              c                   'abcdefghijklmnopqrs-
     d                                     tuvwxyz'
      *
      *=============================================================================================
      * constants
      *=============================================================================================
     d digits          c                   '0123456789'
      *=============================================================================================
      * Arrays
      *=============================================================================================
      *=============================================================================================
      * Data structures
      *=============================================================================================
     d w8562c         uds
     d  x8562c                 1      8S 0

     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)


      * Incoming Record format Data Structures
     d                 ds
     d  Z0DATA                     1500                                         Record 00
     d  Z0TPA                  1      5S 0
     d  Z0SFL1                 6     47
     d  Z0TYPE                48     49S 0
     d  Z0AIND                50     50
     d  Z0EDT                 51     58S 0
     d  Z0ETIM                59     64S 0
     d  Z0SFL2                65     80
     d  Z0TPAN                81    120
     d  Z0PATT               121    150
     d  Z0ADR1               151    180
     d  Z0ADR2               181    210
     d  Z0CTY                211    229
     d  Z0ST                 230    231
     d  Z0ZIP                232    240
     d  Z0SATT               241    270
     d  Z0SAD1               271    300
     d  Z0SAD2               301    330
     d  Z0SCTY               331    349
     d  Z0SST                350    351
     d  Z0SZIP               352    360
     d  Z0CNM1               361    390
     d  Z0CTI1               391    420
     d  Z0CPH1               421    430S 0
     d  Z0CNM2               431    460
     d  Z0CTI2               461    490
     d  Z0CPH2               491    500S 0
     d  Z0CNM3               501    530
     d  Z0CTI3               531    560
     d  Z0CPH3               561    570S 0
     d  Z0ACCT               571    579S 0
     d  Z0#CLM               580    588S 0
     d  Z0#FIN               589    597S 0
     d  Z0#PMT               598    606S 0
     d  Z0#REC               607    615S 0
     d  Z0FILL               616   1500
     d*
     d                 ds
     d  Z1Data                     1500                                         Record 10
     d  Z1TPA                  1      5S 0
     d  Z1ACCT                 6     13
     d  Z1SFL1                14     47
     d  Z1TYPE                48     49S 0
     d  Z1AIND                50     50
     d  Z1ADT                 51     58S 0
     d  Z1ATIM                59     64S 0
     d  Z1SFL2                65     80
     d  Z1ANAM                81    120
     d  Z1PATT               121    150
     d  Z1PAD1               151    180
     d  Z1PAD2               181    210
     d  Z1PCTY               211    229
     d  Z1PST                230    231
     d  Z1PZIP               232    240
     d  Z1SATT               241    270
     d  Z1SAD1               271    300
     d  Z1SAD2               301    330
     d  Z1SCTY               331    349
     d  Z1SST                350    351
     d  Z1SZIP               352    360
     d  Z1CNM1               361    390
     d  Z1CTI1               391    420
     d  Z1CPH1               421    430S 0
     d  Z1CNM2               431    460
     d  Z1CTI2               461    490
     d  Z1CPH2               491    500S 0
     d  Z1CNM3               501    530
     d  Z1CTI3               531    560
     d  Z1CPH3               561    570S 0
     d  Z1#CLM               571    579S 0
     d  Z1#FIN               580    588S 0
     d  Z1CEAM               589    702
     d    Z1C1AM             589    607
     d    Z1C2AM             608    626
     d    Z1C3AM             627    645
     d    Z1C4AM             646    664
     d    Z1C5AM             665    683
     d    Z1C6AM             684    702
     d  Z1CEAT               703    721
     d  Z1PEAM               722    835
     d    Z1P1AM             722    740
     d    Z1P2AM             741    759
     d    Z1P3AM             760    778
     d    Z1P4AM             779    797
     d    Z1P5AM             798    816
     d    Z1P6AM             816    835
     d  Z1PEAT               836    854
     d  Z1PDAM               855    968
     d    Z1PD1M             855    873
     d    Z1PD2M             874    892
     d    Z1PD3M             893    911
     d    Z1PD4M             912    930
     d    Z1PD5M             931    949
     d    Z1PD6M             950    968
     d  Z1PDAT               969    987
     d  Z1PPDA               988   1101
     d    Z1PP1A             988   1006
     d    Z1PP2A            1007   1025
     d    Z1PP3A            1026   1044
     d    Z1PP4A            1045   1063
     d    Z1PP5A            1064   1082
     d    Z1PP6A            1083   1101
     d  Z1PPDT              1102   1120
     d  Z1CTRA              1121   1234
     d    Z1C1RA            1121   1139
     d    Z1C2RA            1140   1158
     d    Z1C3RA            1159   1177
     d    Z1C4RA            1178   1196
     d    Z1C5RA            1197   1215
     d    Z1C6RA            1216   1234
     d  Z1CTRT              1235   1253
     d  Z1PTRA              1254   1367
     d  Z1PTRT              1368   1386
     d  Z1FILL              1387   1500
     d*
     d                 ds
     d  Z2Data                     1500                                         Record 20
     d  Z2TPA                  1      5S 0
     d  Z2ACCT                 6     13
     d  Z2CLM#                14     33
     d  Z2CVCD                34     35
     d  Z2ACSQ                36     37S 0
     d  Z2SFL2                38     47
     d  Z2TYPE                48     49S 0
     d  Z2AIND                50     50
     d  Z2ADT                 51     58S 0
     d  Z2ATIM                59     64S 0
     d  Z2SFL3                65     80
     d  Z2PSYM                81     83
     d  Z2POL#                84     90
     d  Z2PEFD                91     98S 0
     d  Z2PEFT                99    102S 0
     d  Z2PEXD               103    110S 0
     d  Z2PEXT               111    114S 0
     d  Z2CSTS               115    115
     d  Z2ACDT               116    123S 0
     d  Z2ACST               124    125
     d  Z2ACZP               126    134
     d  Z2ALOC               135    137
     d  Z2SCOD               138    152
     d  Z2RUS                153    154
     d  Z2ISFL               155    155
     d  Z2CAB#               156    158S 0
     d  Z2ADSC               159    161
     d  Z2PART               162    165
     d  Z2SIC                166    166
     d  Z2NLCC               167    168
     d  Z2NITC               169    170
     d  Z2NCC                171    175
     d  Z2NATR               176    177
     d  Z2CAUS               178    179
     d  Z2CHSC               180    182
     d  Z2CSUF               183    183
     d  Z2CMIN               184    184
     d  Z2CEIN               185    185
     d  Z2PAIN               186    186
     d  Z2FIL1               187    236
     d  Z2CNFM               237    237
     d  Z2CNAM               238    301
     d  Z2CLSN               238    255
     d  Z2CFSN               256    268
     d  Z2FIL2               269    301
     d  Z2CORG               238    301
     d  Z2CAD1               302    331
     d  Z2CAD2               332    361
     d  Z2CCTY               362    380
     d  Z2CST                381    382
     d  Z2CZIP               383    391
     d  Z2ATND               392    392
     d  Z2CAF                393    452
     d  Z2CAGE               453    454S 0
     d  Z2CSEX               455    455
     d  Z2SS#                456    467S 0
     d  Z2MSTS               468    468
     d  Z2CWWC               469    469
     d  Z2CDOB               470    477S 0
     d  Z2CDEP               478    479S 0
     d  Z2CCLD               480    487S 0
     d  Z2FIL3               488    495
     d  Z2SSOF               496    496
     d  Z2PPOF               497    497
     d  Z2SFOF               498    498
     d  Z2UNOF               499    499
     d  NEW                  500    500
     d  Z2INC#               501    515
     d  Z2FIL4               516    517
     d  Z2CDOD               518    525S 0
     d  Z2HIDT               526    533S 0
     d  Z2EFEI               534    545S 0
     d  Z2RTWD               546    553S 0
     d  Z2ENDT               554    561S 0
     d  Z2ESTS               562    562
     d  Z2ACDS               563    622
     d  Z2PIMP               623    625S 0
     d  Z2COCC               626    655
     d  Z2DSDT               656    663S 0
     d  Z2MMID               664    671S 0
     d  Z2CCIN               672    672
     d  Z2CSIN               673    673
     d  Z2WJCC               674    677
     d  Z2JRST               678    679
     d  Z2CAWW               680    684S 0
     d  Z2WBA                685    693A
     d  Z2WPO                694    698S 0
     d  Z2PIWW               699    703S 0
     d  Z2STYP               704    705
     d  Z2VRED               706    713S 0
     d  Z2NMCO               714    715
     d  Z2FCI                716    717
     d  Z2LCA                718    719
     d  Z2TLOS               720    721
     d  Z2TCOV               722    723
     d  Z2VRIN               724    724
     d  Z2CAFI               725    734
     d  Z2FIL5               735    784
     d  Z2DCFL               785    785
     d  Z2DNFM               786    786
     d  Z2DNAM               787    850
     d  Z2DLSN               787    804
     d  Z2DFSN               805    817
     d  Z2FIL6               818    850
     d  Z2DORG               787    850
     d  Z2DAGE               851    852S 0
     d  Z2VIN                853    869
     d  Z2IVYR               870    873S 0
     d  Z2IVMK               874    888
     d  Z2MAT                889    891
     d  Z2MATIN              892    892
     d  Z2MAIA               893    893
     d  Z2FIL7               894    943
     d  Z2NODT               944    951S 0
     d  Z2DTRP               952    959S 0
     d  Z2ODE                960    978
     d  Z2RIND               979   1027
     d  Z2SUSE              1028   1042
     d  Z2IPCT              1043   1043
     d  Z2RTWS              1044   1044
     d  Z2FILL              1045   1500
     d*
     d                 ds
     d  Z3Data                     1500                                         Record 30
     d  Z3TPA                  1      5S 0
     d  Z3ACCT                 6     13
     d  Z3CLM#                14     33
     d  Z3ACCD                34     35
     d  Z3CSEQ                36     37S 0
     d  Z3SFL1                38     47
     d  Z3TYPE                48     49S 0
     d  Z3AIND                50     50
     d  Z3ADT                 51     58S 0
     d  Z3ATIM                59     64S 0
     d  Z3SFL2                65     80
     d  Z3CEAM                81    194
     d    Z3C1AM              81     99
     d    Z3C2AM             100    118
     d    Z3C3AM             119    137
     d    Z3C4AM             138    156
     d    Z3C5AM             157    175
     d    Z3C6AM             176    194
     d  Z3CEAT               195    213
     d  Z3PEAM               214    327
     d  Z3PEAT               328    346
     d  Z3CPDA               347    460
     d    Z3C1DA             347    365
     d    Z3C2DA             366    384
     d    Z3C3DA             385    403
     d    Z3C4DA             404    422
     d    Z3C5DA             423    441
     d    Z3C6DA             442    460
     d  Z3CPDT               461    479
     d  Z3PPDA               480    593
     d  Z3PPDT               594    612
     d  Z3CTRA               613    726
     d    Z3C1RA             613    631
     d    Z3C2RA             632    650
     d    Z3C3RA             651    669
     d    Z3C4RA             670    688
     d    Z3C5RA             689    707
     d    Z3C6RA             708    726
     d  Z3CTRT               727    745
     d  Z3PTRA               746    859
     d  Z3PTRT               860    878
     d  Z3FILL               879   1500
     d*
     d                 ds
     d  Z4Data                     1500                                         Record 40
     d  Z4TPA                  1      5S 0
     d  Z4ACCT                 6     13
     d  Z4CLM#                14     33
     d  Z4ACCD                34     35
     d  Z4ACSQ                36     37S 0
     d  Z4PKCD                38     42
     d  Z4PSEQ                43     47S 0
     d  Z4TYPE                48     49S 0
     d  Z4AIND                50     50
     d  Z4PDT                 51     58S 0
     d  Z4PTIM                59     64S 0
     d  Z4SFIL                65     80
     d  Z4PBNM                81     81S 0
     d  Z4PLAM                82    100
     d  Z4BAMT               101    119
     d  Z4PBA#               120    134S 0
     d  Z4CHK#               135    149S 0
     d  Z4PDAT               150    157S 0
     d  Z4PPOI               158    158
     d  Z4NATP               159    222
     d  Z4PNAM               223    286
     d  Z4PAD1               287    316
     d  Z4PAD2               317    346
     d  Z4PCTY               347    368
     d  Z4PST                369    370
     d  Z4PZIP               371    379
     d  Z4PCNT               380    393
     d  Z4TAXT               394    394
     d  Z4FEIN               395    406S 0
     d  Z4LSPC               407    408
     d  Z4FILL               409   1500
     d*
     d                 ds
     d  Z5Data                     1500                                         Record 50
     d  Z5TPA                  1      5S 0
     d  Z5ACCT                 6     13
     d  Z5CLM#                14     33
     d  Z5ACCD                34     35
     d  Z5ASEQ                36     37S 0
     d  Z5RPKC                38     42
     d  Z5RSEQ                43     47S 0
     d  Z5TYPE                48     49S 0
     d  Z5AIND                50     50
     d  Z5RDT                 51     58S 0
     d  Z5RTIM                59     64S 0
     d  Z5SFIL                65     80
     d  Z5BNUM                81     81S 0
     d  Z5TPLA                82    100
     d  Z5TPRA               101    119
     d  Z5RBA#               120    134S 0
     d  Z5RCHK               135    149S 0
     d  Z5RTRD               150    157S 0
     d  Z5FILL               158   1500
     d*
     ddsrundt          ds             8
     d DSYY                    1      4
     d DSMM                    5      6
     d DSDD                    7      8
     d
     D nap             s              2    dim(99) ctdata perrcd(1)
     D naz             s              2    dim(99) alt(nap)
     D pap             s              2    dim(99) ctdata perrcd(1)
     D paz             s              2    dim(99) alt(pap)
     D cap             s              2    dim(103) ctdata perrcd(1)
     D caz             s              3    dim(103) alt(cap)
     d*** Begin Add *** 10/12/17 ***********************************
     d partKey         s                   Like(PAkey)
     d*** End   Add *** 10/12/17 ***********************************
     D x               s              3  0
     D sign            s              1
     d

     d psds          esds                  extname(szq1p)

      *=============================================================================================
      * KLISTS
      *=============================================================================================
     c     upkey         klist
     c                   kfld                    q1user
     c*
      *
      * Claimant Demographic file by Soc Sec#
     c     cdky          klist
     c                   kfld                    kkss#             9
      *
      * Claims History
     c     chky1         klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
     c                   kfld                    cmfyr
     c                   kfld                    cmcase
     c                   kfld                    kkefdt
     c*
     c     chky2         klist
     c                   kfld                    cmco#
     c                   kfld                    cmfnd
     c                   kfld                    cmfyr
     c                   kfld                    cmcase
      *
      * Claims Master by SocSec#, Accident Date
     c     cmky          klist
     c                   kfld                    kkco#
     c                   kfld                    kkss#
     c                   kfld                    kkacdt            7 0
      *
     c     cmky1         klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd

     c     coky          klist
     c                   kfld                    kkco#

     c     f2ky          klist
     c                   kfld                    kkco#
     c                   kfld                    gsfnd
     c
     c     keypap        klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkfyr             3 0
     c                   kfld                    kkcase            7 0
     c
     c     keypap1       klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkfyr             3 0
     c                   kfld                    kkcase            7 0
     c                   kfld                    kktrdt            7 0
     c
     c     keypap5       klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkfyr             3 0
     c                   kfld                    kkcase            7 0
     c*                  kfld                    kktrdt            7 0
     c
     c     keyfop        klist
     c                   kfld                    kkco#
     c                   kfld                    kkeom#            9 0
     c                   kfld                    kkssn#            9
     c
     c     keyzur1       klist
     c                   kfld                    kktpa             5 0
     c                   kfld                    kkclm#           20
     c
     c     keyfup        klist
     c                   kfld                    kkco#
     c                   kfld                    kksub#            5 0
     c                   kfld                    kkfnd             3 0
     c                   kfld                    kkfyr             3 0
     c                   kfld                    kkcase
     c                   kfld                    kkeom#
     c

     c     keyclm3       klist
     c                   kfld                    kkco#             3 0
     c                   kfld                    kkfnd             3 0

     c     keycmp1       klist
     c                   kfld                    kkco#
     c
     c     keyfnl        klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c*
     c     keyemp1       klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#            9 0
     c                   kfld                    kkdiv             5 0
     c*
     c     keyemp2       klist
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkemp#
     c                   kfld                    kkdiv
     c                   kfld                    kkfyr
     c*                                                                         co#/fnd/fyr/case/seq
     c     kychk1        klist                                                  #
     c                   kfld                    kkco#
     c                   kfld                    kkfnd
     c                   kfld                    kkfyr
     c                   kfld                    kkcase
     c                   kfld                    kkseq             5 0
     c*                                                                         co#/fnd/fyr/case/seq
     c     ky10          klist                                                  #
     c                   kfld                    kktpa
     c
     c     ky20          klist                                                  #
     c                   kfld                    kktpa
     c                   kfld                    kkACCT
     c
     c     ky30          klist                                                  #
     c                   kfld                    kktpa             5 0
     c                   kfld                    kkACCT            8
     c                   kfld                    kkclm#           20
     c
     c     ky40          klist                                                  #
     c                   kfld                    kktpa
     c                   kfld                    kkACCT
     c                   kfld                    kkclm#
     c
     c
     c*
      *=============================================================================================
      * PLISTS
      *=============================================================================================
     c     *entry        plist
     c                   parm                    @@TPAC           10
     c                   parm                    @@LRDT            7
     c                   parm                    @@MEDT            7
     c                   parm                    @@co#             3 0
     c                   parm                    @@dcid            8

      * Date Field Conversion Parm Lists
      *
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
      *
      *
      *=============================================================================================
      * Initialize
      *=============================================================================================
      *
     c                   move      *blanks       z0data
     c                   move      *blanks       z1data
     c                   move      *blanks       z2data
     c                   move      *blanks       z3data
     c                   move      *blanks       z4data
     c                   move      *blanks       z5data
      *
     c                   move      *zeros        xxcntr
     c                   move(P)   @@tpac        xxtpac            5 0
     c                   move      @@lrdt        xxlrdt
     c                   move      @@medt        xxmedt
     c*
     c                   eval      srdti = xxmedt
     c                   call      'SRCVTDAT'
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     c                   movel     srdto         dsrundt
     c*
     c                   TIME                    dsdttm
      *
      *=============================================================================================
      * Main loop
      *=============================================================================================
     c*
     c                   evalr     xxzeros = '+000000000000000000'
     c*
     C                   select
     C                   when      dsmm = '01'
     C                   eval      dsdd='31'
     C                   when      dsmm = '02'
     C                   eval      dsdd='28'
     C                   when      dsmm = '03'
     C                   eval      dsdd='31'
     C                   when      dsmm = '04'
     C                   eval      dsdd='30'
     C                   when      dsmm = '05'
     C                   eval      dsdd='31'
     C                   when      dsmm = '06'
     C                   eval      dsdd='30'
     C                   when      dsmm = '07'
     C                   eval      dsdd='31'
     C                   when      dsmm = '08'
     C                   eval      dsdd='31'
     C                   when      dsmm = '09'
     C                   eval      dsdd='30'
     C                   when      dsmm = '10'
     C                   eval      dsdd='31'
     C                   when      dsmm = '11'
     C                   eval      dsdd='30'
     C                   when      dsmm = '12'
     C                   eval      dsdd='31'
     C                   endsl
     c*
     c                   clear                   pzu01
     c*  Retrieve CO# from WMUPP (read using user id Q1USER)
     c*
     c     upkey         chain     pup01                              60
     c     *in60         ifeq      '1'
     c                   move      *zero         upco#
     c                   endif
     c
     c                   move      upco#         xxco#
     c                   move      xxco#         kkco#
     C*
     C* Process selected groups
     C*
     c                   read      pgs01                                  99
     c                   dow       not *in99
     c*
     C                   if        gsiefl='I'
     c*
     c     coky          chain     pco01                              60
     c
     c                   move      coco#         kkco#
     c                   move      gsfnd         kkfnd
     c*
     c* Fill contact details.
     c*
     c                   if        fccon1  = *blank
     c     keycmp1       chain     pcc01                              60
     c                   if        *in60 = *off
     c                   move      cccon1        xxcnm1
     c                   move      cccon2        xxcnm2
     c                   move      cccon3        xxcnm3
     c                   endif                                                   find cont1 details
     c                   else
     c     keyclm3       chain     pfc01                              60
     c                   move      fccon1        xxcnm1
     c                   move      fccon2        xxcnm2
     c                   move      fccon3        xxcnm3
     c                   endif
     c*
     c* Fill contact phone details.
     c*
     c     fcphn1        ifeq      *zeros
     c     keycmp1       chain     pcc01                              60
     c                   if        *in60 = *off
     c                   move      ccphn1        xxcph1
     c                   move      ccphn2        xxcph2
     c                   move      ccphn3        xxcph3
     c                   endif                                                   find cont1 details
     c                   else
     c     keyclm3       chain     pfc01                              60
     c                   move      fcphn1        xxcph1
     c                   move      fcphn2        xxcph2
     c                   move      fcphn3        xxcph3
     c                   endif
     c*
     c* Fill contact title details.
     c*
     c                   if        fcttl1  = *blank
     c     keycmp1       chain     pcc01                              60
     c                   if        *in60 = *off
     c                   move      ccttl1        xxttl1
     c                   move      ccttl2        xxttl2
     c                   move      ccttl3        xxttl3
     c                   endif                                                   find cont1 details
     c                   else
     c     keyclm3       chain     pfc01                              60
     c                   move      fcttl1        xxttl1
     c                   move      fcttl2        xxttl2
     c                   move      fcttl3        xxttl3
     c                   endif
     c
     c*
     c*  write header record
     c*
     c*  Read claims file for CO#/FND
     c*
     c*
     c     keyclm3       chain     pcm01                              60
     c*
     c                   dow       *in60 = *off                                 WMCMP LOOP
     c*
     c                   z-add     0             xxseq
     c*
     c* Read claims file for CO#/FND
     c*
     c                   move      cmfnd         xxfnd1            3
     c                   move      cmfyr         xxfyr1            3
     c                   move      cmcase        xxcase1           7
     c*
     c                   eval      xxclm = xxfnd1 + xxfyr1 + xxcase1
     c*
     c*
     c* Look up claim in history extract file and compare
     c* and Verify the activity in claims
     c*
     c                   move      cmco#         kkco#
     c                   move      cmfnd         kkfnd
     c                   move      cmfyr         kkfyr
     c                   movel     cmcase        kkcase
     c*
     c                   eval      kktrdt=xxmedt
     c*
     c                   movel     @@tpac        kktpa
     c                   movel     xxclm         kkclm#
     c                   eval      write2030=*off
     c*
     c     keypap5       setgt     ppa05
     c     keypap        readpe    ppa05                                  67
     c                   if        not *in67 and patrdt>xxlrdt
     c                   eval      write2030=*on
     c                   endif
     c*
     c                   if        write2030
     c*
     c                   exsr      clmpmt
     c*
     c* write type 20 records
     c*
     c                   exsr      srlod20
     c                   write     pz201
     c*
     c* Calculate Claim amounts for the rcd type 30
     c*
     c                   exsr      clmamt
     c                   exsr      srlod30
     c*
     c* write type 30 records
     c*
     c                   if        xxcea11<>0 or xxcea22<>0 or
     c                             xxcea33<>0
     c                   write     pz301
     c                   eval      xxcntr = xxcntr +1
     c                   endif
     c*
     c                   endif                                                   z2clm# <> xxclm
     c*
     c                   move      cmfnd         svfnd
     c     keyclm3       reade     pcm01                                  60
     c*
     c                   enddo                                                   claim loop
     c*
     c* write type 10 records
     c*
     c                   exsr      srlod10
     c                   write     pz101
     c*
     c                   endif                                                   F2TPAC = @@TPAC
     C*
     c                   read      pgs01                                  99
     C                   enddo
     c*
     c*
     c* write type 00 records
     c*
     c                   exsr      srlod00
     c                   write     pz001
      *
     c*
     c                   exsr      writer
      *
     c                   if        xxcntr >= 1
     c                   exsr      crtdoc
     c                   endif                                                   xxcntr > 1
     c                   if        #xdcid <> *blanks
     c                   move      #xdcid        @@dcid
     c                   movel     'T'           @@dcid
     c                   endif                                                   #xrtcd <> Blank
     c                   move      xxco#         @@co#
     c*** begin chg *** 09/29/10 *******************************************
     c**********         eval      x8562c=dsdate
     c                   eval      srdti=xxmedt
     c                   call      'SRCVTDAT'    dtmdyy
     c                   movel     srdto         x8562c
     c*** end   chg *** 09/29/10 *******************************************
      *
      * ===================================================================== *
      *  Terminate Program
      * ===================================================================== *
      *
     C                   seton                                        lr
      * ===================================================================== *
      *
      *=============================================================================================
      * CRTDOC Cretaes documnet if RCD 20 & 30 has been written
      *=============================================================================================
      *
      * Get Resolved Document Name
      *
     c     crtdoc        begsr
      *
     c                   MOVE      kkco#         #xco#
     c                   MOVE      kkfnd         #xfnd
     c                   MOVE      'TP'          #xpre             2
      *
     c                   CALL      'SNXTDCID'
     c                   PARM      tcfldr        #xfold           80
     c                   PARM      *BLANKS       #xdcid           12
     c                   PARM                    #xpre             2
     c                   PARM                    #xco#             3
     c                   PARM                    #xfnd             3
     c                   PARM      *BLANK        #xrtcd            1
      *
     c                   endsr                                                   CRTDOC
     c
      *=============================================================================================
      * SRLOD00
      *=============================================================================================
     c     srlod00       begsr
     c
     c*                  Load claim data into data structures based upon the extract files.
     c*                  Record type 00
     c*
     c*
     c* Load "A" if new TPA being sent "C" if existing TPA
     c*
     c                   eval      x0ind = 'A'
     c                   dow       *in61 = *off
     c                   read(n)   PZ001H                                 61             ext hist
     c                   If        XXTPAC = H_Z0TPA
     c                   eval      x0ind = 'C'
     c                   leave
     c                   endif
     c                   enddo
     c*
     c                   eval      xxco#  = kkco#
     c                   eval      xxfnd  = kkfnd
     c*
     c     keyfnl        chain     PFN01                                         wmfnl
     c
     c*                  Record type 00
     c                   Move(p)   xxTPAC        Z0TPA
     c                   Move(p)   *BLANKS       Z0SFL1
     c                   Movel(p)  00            Z0TYPE
     c                   Move(p)   x0ind         Z0AIND                          A/C based on age
     c                   Move(p)   dsrundt       Z0EDT
     c                   Move(p)   dstime        Z0ETIM
     c                   Move(p)   *blanks       Z0SFL2
     c                   Movel(p)  coname        Z0TPAN                          SMCOP/CONAME
     c                   Movel(p)  cccon1        Z0PATT                          SMCCP/CCATTN
     c                   Movel(p)  comad1        Z0ADR1                          SMCOP/COMAD1
     c                   Movel(p)  comad2        Z0ADR2                          SMCOP/COMAD2
     c                   Movel(p)  comcty        Z0CTY                           SMCOP/COMCTY
     c                   Movel(p)  comst         Z0ST                            SMCOP/COMST
     c                   Movel(p)  comzip        Z0ZIP                           SMCOP/COMZIP
     c                   Move(p)   *blanks       Z0SATT
     c                   Move(p)   *blanks       Z0SAD1
     c                   Move(p)   *blanks       Z0SAD2
     c                   Move(p)   *blanks       Z0SCTY
     c                   Move(p)   *blanks       Z0SST
     c                   Move(p)   *blanks       Z0SZIP
     c                   Movel(p)  cccon1        Z0CNM1                          SMCCP/CCCON1
     c                   Movel(p)  ccttl1        Z0CTI1                          SMCCP/CCTTL1
     c                   Move(p)   xxcph1        Z0CPH1                          SMCCP/CCARA1+CCPHN1
     c                   Movel(p)  cccon2        Z0CNM2                          SMCCP/CCCON2
     c                   Movel(p)  ccttl2        Z0CTI2                          SMCCP/CCTTL2
     c                   Move(p)   xxcph2        Z0CPH2                          SMCCP/CCARA2+CCPHN2
     c                   Movel(p)  cccon3        Z0CNM3                          SMCCP/CCCON3
     c                   Movel(p)  ccttl3        Z0CTI3                          SMCCP/CCTTL3
     c                   Move(p)   xxcph3        Z0CPH3                          SMCCP/CCARA3+CCPHN3
     c                   Z-add     xxseq10       Z0ACCT                          # of records in 10
     c                   Z-add     xxseq20       Z0#CLM                          # of records in 20
     c                   Z-add     xxseq30       Z0#FIN                          # of records in 30
     c                   Z-add     xxseq40       Z0#PMT                          # of records in 40
     c                   Z-add     xxseq50       Z0#REC                          # of records in 50
     c                   Move      *blanks       Z0FILL
     c                   endsr                                                   SRLOD00 endsr
      *=============================================================================================
      * SRLOD10
      *=============================================================================================
     c     srlod10       begsr
     c*
     c                   eval      xxco#  = kkco#
     c                   eval      xxfnd  = kkfnd
     c*
     c     keyfnl        chain     PFN01                                         wmfnl
     c                   add       1             xxseq10
     c*
     c*
     c                   if        x1c1am  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c1am=x1c1am*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c1am        xxceamc          19
     c                   if        x1c1am  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C1AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c1am=x1c1am*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c
     c                   if        x1c2am  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c2am=x1c2am*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c2am        xxceamc          19
     c                   if        x1c2am  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C2AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c2am=x1c2am*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c
     c*
     c                   if        x1c6am  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c6am=x1c6am*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c6am        xxceamc          19
     c                   if        x1c6am  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C6AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c6am=x1c6am*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      xxzeros       Z1C3AM
     c                   move      xxzeros       Z1C4AM
     c                   move      xxzeros       Z1C5AM
     c                   move      *blanks       xxceamc
     c*
     c                   eval      x1ceat  = x1c1am + x1c2am + x1c6am
     c*
     c                   if        x1ceat  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1ceat=x1ceat*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1ceat        xxceamc          19
     c                   if        x1ceat  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1CEAT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1ceat=x1ceat*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c1da  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c1da=x1c1da*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c1da        xxceamc          19
     c                   if        x1c1da  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1PD1M
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c1da=x1c1da*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c2da  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c2da=x1c2da*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c2da        xxceamc          19
     c                   if        x1c2da  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1PD2M
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c2da=x1c2da*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c6da  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c6da=x1c6da*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c6da        xxceamc          19
     c                   if        x1c6da  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1PD6M
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c6da=x1c6da*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      xxzeros       Z1PD3M
     c                   move      xxzeros       Z1PD4M
     c                   move      xxzeros       Z1PD5M
     c                   move      *blanks       xxceamc
     c*
     c                   eval      x1cpdt  = x1c1da + x1c2da  + x1c6da
     c*
     c                   if        x1cpdt  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1cpdt=x1cpdt*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1cpdt        xxceamc          19
     c                   if        x1cpdt  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1PDAT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1cpdt=x1cpdt*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c1ra  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c1ra=x1c1ra*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c1ra        xxceamc          19
     c                   if        x1c1ra  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C1RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c1ra=x1c1ra*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c2ra  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c2ra=x1c2ra*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c2ra        xxceamc          19
     c                   if        x1c2ra  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C2RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c2ra=x1c2ra*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*
     c                   if        x1c6ra  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1c6ra=x1c6ra*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1c6ra        xxceamc          19
     c                   if        x1c6ra  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1C6RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1c6ra=x1c6ra*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      xxzeros       Z1C3RA
     c                   move      xxzeros       Z1C4RA
     c                   move      xxzeros       Z1C5RA
     c                   move      *blanks       xxceamc
     c*
     c                   eval      x1ctrt = x1c1ra + x1c2ra  + x1c6ra
     c*
     c                   if        x1ctrt  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      x1ctrt=x1ctrt*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      x1ctrt        xxceamc          19
     c                   if        x1ctrt  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z1CTRT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      x1ctrt=x1ctrt*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c*                  Record type 10
     c                   Movel(p)  xxTPAC        Z1TPA
     c                   Movel(p)  kkfnd         Z1ACCT                          changed in qa
     c                   Move(p)   *blanks       Z1SFL1
     c                   Movel(p)  10            Z1TYPE
     c                   Move(p)   x1ind         Z1AIND                          A/C based on check
     c                   Movel(p)  dsrundt       Z1ADT
     c                   Z-add     dstime        Z1ATIM
     c                   Move(p)   *blanks       Z1SFL2
     c                   Movel(p)  fnname        Z1ANAM                          WMFNP/FNNAME
     c                   Movel(p)  fccon1        Z1PATT                          WMFCP/FCATTN ???
     c                   Movel(p)  fnmad1        Z1PAD1                          WMFNP/FNMAD1
     c                   Movel(p)  fnmad2        Z1PAD2                          WMFNP/FNMAD2
     c                   Movel(p)  fnmcty        Z1PCTY                          WMFNP/FNMCTY
     c                   Movel(p)  fnmst         Z1PST                           WMFNP/FNMST
     c                   Movel(p)  fnmzip        Z1PZIP                          WMFNP/FNMZIP
     c                   Move(p)   *blanks       Z1SATT
     c                   Move(p)   *blanks       Z1SAD1
     c                   Move(p)   *blanks       Z1SAD2
     c                   Move(p)   *blanks       Z1SCTY
     c                   Move(p)   *blanks       Z1SST
     c                   Move(p)   *blanks       Z1SZIP
     c                   Move(p)   xxcnm1        Z1CNM1                          WMFCP/FCCNT1 if
     c                   Move(p)   xxttl1        Z1CTI1                          blank then use
     c                   Z-add     xxcph1        Z1CPH1                          values from SMCCP
     c                   Movel(p)  xxcnm2        Z1CNM2                          WMFCP/FCCNT2 if
     c                   Movel(p)  xxttl2        Z1CTI2                          blank then use
     c                   Z-add     xxcph2        Z1CPH2                          values from SMCCP
     c                   Movel(p)  xxcnm3        Z1CNM3                          WMFCP/FCCNT3 if
     c                   Movel(p)  xxttl3        Z1CTI3                          blank then use
     c                   Z-add     xxcph3        Z1CPH3                          values from SMCCP
     c                   Z-add     xxseq20       Z1#CLM                          # of records in 20
     c                   Z-add     xxseq30       Z1#FIN                          # of records in 30
     c                   move      Z3PEAM        Z1PEAM
     c                   move      Z3PEAT        Z1PEAT
     c                   move      Z3PPDA        Z1PPDA
     c                   move      Z3PPDT        Z1PPDT
     c                   move      Z3PTRA        Z1PTRA
     c                   move      Z3PTRT        Z1PTRT
     c                   Move      ' '           Z1FILL
     c                   endsr                                                   SRLOD10 endsr
     c
      *=============================================================================================
      * SRLOD20
      *=============================================================================================
     c     srlod20       begsr
     c*
     c                   add       1             xxseq20
     c*
     c* for fatching data from demographic record file.
     c*
     c                   move      cmco#         kkco#
     c                   move      cmfnd         kkfnd
     c                   move      cmemp#        kkemp#
     c                   move      cmdiv         kkdiv
     c*
     c     keyemp1       chain     pem01                              60
     c
     c*
     c* for fatching data from demographic record file.
     c*
     c
     c     CMSS#         chain     pcd01                              60                 wmcdp
     c
     c*                  eval      xxage = %diff(CMACDT:CDDTOB:*years)
     c                   call      'SRDDIF7'
     c                   parm      cddtob        @@date1           7 0
     c                   parm      cmacdt        @@date2           7 0
     c                   parm      *zeros        @@diff            5 0
     c                   parm      *blank        @@err             1
     c
     c                   if        @@diff <> 0
     c                   eval      xxage = @@diff / 365
     c                   else
     c                   eval      xxage = 0
     c                   endif
     c*
     c                   if        cmcvfl='Y'
     c                   eval      z2ccin='Y'
     c                   else
     c                   eval      z2ccin='N'
     c                   endif
     c*
     c                   if        CMSGDT <> *zero
     c                   eval      z2csin = 'Y'
     c                   else
     c                   eval      z2csin = 'N'
     c                   endif
     c
     c                   if        CMMCO <> 'Y'
     c                   eval      z2nmco = '00'
     c                   else
     c                   eval      z2nmco = '40'
     c                   endif
     c
     c                   if        CMJRST='CA' or cmjrst= 'DE' or cmjrst = 'PA'
     c                   eval      z2fci = '00'
     c                   endif
     c*
     c* finding name of the claimnt
     c*
     c                   eval      z2cnam = *blanks
     c                   eval      z2clsn = cdlsn
     c                   eval      z2cfsn = cdfsn
     c*
     c                   select
     c                   when      CMRT01 = 'LE'
     C                   if        cmar01<0
     C                   eval      sign='-'
     C                   else
     C                   eval      sign='+'
     C                   endif
     c                   Move      CMAR01        z2cafi
     c                   movel     sign          z2cafi
     c                   when      CMRT02 = 'LE'
     C                   if        cmar02<0
     C                   eval      sign='-'
     C                   else
     C                   eval      sign='+'
     C                   endif
     c                   Move      CMAR02        z2cafi
     c                   movel     sign          z2cafi
     c                   when      CMRT03 = 'LE'
     C                   if        cmar03<0
     C                   eval      sign='-'
     C                   else
     C                   eval      sign='+'
     C                   endif
     c                   Move      CMAR03        z2cafi
     c                   movel     sign          z2cafi
     c                   when      CMRT04 = 'LE'
     C                   if        cmar04<0
     C                   eval      sign='-'
     C                   else
     C                   eval      sign='+'
     C                   endif
     c                   Move      CMAR04        z2cafi
     c                   movel     sign          z2cafi
     c                   when      CMRT05 = 'LE'
     C                   if        cmar05<0
     C                   eval      sign='-'
     C                   else
     C                   eval      sign='+'
     C                   endif
     c                   Move      CMAR05        z2cafi
     c                   movel     sign          z2cafi
     c                   endsl
     c*
     c* MCO type
     c*
     c                   if        cmmco <> 'Y'
     c                   move      '00'          xxmco
     c                   else
     c                   move      '04'          xxmco
     c                   endif
     c*
     c* Fraudulent Claim Indicator
     c*
     c                   if        cmjrst <> 'Y'
     c                   move      '00'          xxjrst
     c                   else
     c                   move      '04'          xxjrst
     c                   endif
     c
     c*
     c* Claimnt surgury indicator
     c*
     c                   if        cmsgdt <> *zeros
     c                   move      'Y'           xxcin
     c                   else
     c                   move      'N'           xxcin
     c                   endif
     c
     c*
     c* Claim status
     c*
     c                   if        cmstat = 'O'
     c*
     c     chky1         setgt     pch01
     c     chky2         readpe    pch01
     c                   dow       not %eof
     c                   if        cmstat='C'
     c                   move      'R'           xxcsts
     c                   leave
     c                   endif
     c     chky2         readpe    pch01
     c                   if        %eof
     c                   move      'O'           xxcsts
     c                   endif
     c                   enddo
     c*
     c                   else
     c                   move      'C'           xxcsts
     c                   endif
     c
     c*                  Record type 20
     c                   Move      xxTPAC        Z2TPA
     c                   Movel     CMFND         Z2ACCT                          ?
     c                   Movel(p)  xxclm         Z2CLM#                          CMFND+CMFYR+CMCASE
     c                   Movel     'WC'          Z2CVCD                          CMCOVT
     c                   Movel     01            Z2ACSQ
     c                   Move      *blanks       Z2SFL2
     c                   Movel     20            Z2TYPE
     c*
     c* Load "A" if new Claim being sent "C" if existing claim
     c*
     c                   eval      x2ind = 'A'
     c                   dow       *in61 = *off
     c                   read(n)   PZ201H                                 61             ext hist
     c                   If        XXclm = H_Z2clm#
     c                   eval      x2ind = 'C'
     c                   leave
     c                   endif
     c                   enddo
     c*
     c                   Move      x2ind         Z2AIND
     c                   Movel     dsrundt       Z2ADT
     c                   Move      dstime        Z2ATIM
     c                   Move      *blanks       Z2SFL3
     c                   Movel     'WC'          Z2PSYM                          This is from Zurich
     c                   Move      '6541080'     Z2POL#                          This is a Zurich #
     c*
     c* Find policy effective date
     c*
     c                   z-add     CMCO#         kkco#
     c                   z-add     CMFND         kkfnd
     c                   z-add     CMFYR         kkfyr
     c                   z-add     CMEMP#        kkemp#
     c                   z-add     CMDIV         kkdiv
     c
     c     keyemp2       chain     pel01                              60
     C                   eval      srdti = elpfdt
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxPFDT            8 0
     c*
     c                   Z-add     xxPFDT        Z2PEFD                          ELPFDT
     c                   Move      0001          Z2PEFT                          ELPTDT
     c                   eval      srdti = elptdt
     c*                  call      'SRCVTDAT'    dtmdyy
     c                   call      'SRCVTDAT'    dtyymd
     c                   movel     srdto         xxPTDT            8 0
     c
     c                   Z-add     xxPTDT        Z2PEXD
     c                   Move      0001          Z2PEXT
     c                   Move      xxcsts        Z2CSTS                          O,R,C,D
     c                   eval      srdti = CMACDT
     c*                  call      'SRCVTDAT'    dtmdyy
     c                   call      'SRCVTDAT'    dtyymd
     c                   movel     srdto         xxacdt            8 0
     c
     c                   Z-add     xxACDT        Z2ACDT
     c                   Movel     CMAST         Z2ACST
     c                   Movel     CMACZ1        Z2ACZP
     c                   Movel     CMCTG1        Z2ALOC                          CMCTG1(10)
     c                   Movel     CMACZ1        Z2SCOD                          CMACZ1(5,0)
     c                   Movel     CMJRST        Z2RUS
     c                   Move      'N'           Z2ISFL
     c                   Move      *zeros        Z2CAB#
     c                   Movel     *blanks       Z2caus                          CMCAUS xref with aB
     c*
     c                   eval      x=1
     c     cmnatr        lookup    nap(x)                                 99     CMCAUS xref with aC
     c                   if        *in99
     c                   movel     naz(x)        z2natr
     c                   else
     c                   movel     '59'          z2natr
     c                   endif
     c*
     c*** Begin Chg *** 10/12/17 *******************************************
     c**********         eval      x=1
     c*****cmpart        lookup    pap(x)                                 99     CMCAUS xref with aC
     c**********         if        *in99
     c**********         movel     paz(x)        z2part
     c**********         else
     c**********         movel     '59'          z2part
     c**********         endif
      /free
       // Get the EDI part code
       partKey = CMpart;
       Chain (CMco#:partKey) ppa02;
       If %Found(wtabl);
          z2part = PAediP;
       Else;
          z2Part = '59';
       EndIf;
      /end-free
     d*** End   Add *** 10/12/17 ***********************************
     c*
     c                   eval      x=1
     c     cmcaus        lookup    cap(x)                                 99     CMCAUS xref with aC
     c                   if        *in99 and caz(x) <> *blanks
     c                   movel     caz(x)        z2adsc
     c                   else
     c                   movel     '059'         z2adsc
     c                   endif
     c*
     c                   Move      *blank        Z2SIC
     c                   Movel     '01'          Z2NLCC                          CMLSCV xref with aJ
     c                   Movel     cmncci        Z2NITC                          CMNCCI(2) xrf withJ
     c                   Movel     *blanks       Z2NCC                           CMCATA(5) xrfwith85
     c                   Movel     cmsupr        Z2CHSC                          CMSUPR
     c                   Move      *blank        Z2CSUF
     c                   Move      *blank        Z2CMIN
     c                   Movel     CMEXCS        Z2CEIN                          CMEXCS(1)
     c                   Move      *blank        Z2PAIN
     c                   Move      *blanks       Z2FIL1
     c                   Move      'I'           Z2CNFM
     c                   Move      *blanks       Z2FIL2
     c                   Movel(P)  CDADR1        Z2CAD1                          CDADR1(40)
     c                   Movel(P)  CDADR2        Z2CAD2                          CDADR2(40)
     c                   Movel(P)  CDCTY         Z2CCTY                          CDCTY(25)
     c                   Movel     CDST          Z2CST                           CDST(2)
     c                   Movel     CDZIP         Z2CZIP                          CDZIP(9 0)
     c                   Move      'N'           Z2ATND
     c                   Move      *blanks       Z2CAF
     c                   Move      xxage         Z2CAGE                          CMACDT-CDDTOB
     c                   Movel     cdsex         Z2CSEX
     c                   if        z2csex=*blanks
     c                   eval      z2csex='U'
     c                   endif
     c                   Movel     CDSS#         x1SS#             9 0
     c                   Z-add     x1SS#         Z2SS#
     C*
     C                   select
     C                   when      cdmsts='K' or cdmsts=' '
     C                   eval      z2msts='U'
     C                   when      cdmsts='M'
     C                   eval      z2msts='M'
     C                   when      cdmsts='S'
     C                   eval      z2msts='P'
     C                   when      cdmsts='U'
     C                   eval      z2msts='U'
     C                   endsl
     C*
     c                   Movel     'A'           Z2CWWC
     c                   Movel     xxcsts        Z2CSTS                          O,R,C,D
     c
     C                   eval      srdti = CDDTOB
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDTOB            8 0
     c
     c                   Z-add     xxDTOB        Z2CDOB
     c                   Z-add     CM#DEP        Z2CDEP
     c
     c*
     c                   z-add     *zeros        xxefdt
     c                   if        cmstat='C'
     c     chky1         setgt     pch01
     c     chky2         readpe    pch01
     c                   if        not %eof
     C                   eval      srdti = chefdt
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxefdt            8 0
     c                   endif
     c                   endif
     c
     c                   Move      xxefdt        Z2CCLD                          WMCHP/CHEFDT
     c                   Move      *blanks       Z2FIL3
     c                   Movel     CMSSFF        Z2SSOF
     c                   Movel     CMPEFF        Z2PPOF
     c                   Movel     CMSFFF        Z2SFOF
     c                   Movel     CMUEFF        Z2UNOF
     c                   Movel     CMOTFF        NEW
     c
     c                   Move      *blanks       Z2inc#
     c                   Move      *blanks       Z2FIL4
     c
     C                   eval      srdti = CMDOD
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDOD             8 0
     c
     c                   Z-add     xxDOD         Z2CDOD                          CMDOD(7,0)
     c
     C                   eval      srdti = CMHIDT
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxHIDT            8 0
     c
     c                   Z-add     xxHIDT        Z2HIDT                          CMHIDT(7,0)
     c                   Z-add     emfein        Z2EFEI                          WMEMP/EMFEIN
     c
     C                   eval      srdti = CMRTW
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxRTW             8 0
     c                   Z-add     xxRTW         Z2RTWD
     c
     C                   eval      srdti = CMDTRP
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDTRP            8 0
     c
     c                   Z-add     xxDTRP        Z2ENDT                          CMDTRP(7,0)
     C*
     C                   select
     C                   when      cmests='FT'
     C                   eval      z2ests='F'
     C                   when      cmests='PT'
     C                   eval      z2ests='P'
     C                   when      cmests='3'
     C                   eval      z2ests='U'
     C                   when      cmests='4'
     C                   eval      z2ests='S'
     C                   when      cmests='5'
     C                   eval      z2ests='D'
     C                   other
     C                   eval      z2ests='F'
     C                   endsl
     C*
     c                   Movel(p)  CMACDS        Z2ACDS                          CMACDS(60)
     c
     c                   Z-add     CMIPCT        Z2PIMP                          CMIPCT(5,2)
     c                   Movel(p)  CMOCDS        Z2COCC                          CMOCDS(40)
     c
     C                   eval      srdti = CMDBDT
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDBDT            8 0
     c
     c                   Z-add     xxDBDT        Z2DSDT                          CMDBDT(7,0)
     c
     C                   eval      srdti = CMMMI
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxMMI             8 0
     c
     c*                  Move      CMMMI         Z2MMID                          CMMMI
     c                   Z-add     xxMMI         Z2MMID                          CMMMI
     c
     c                   Move      xxcin         Z2CCIN                          CMSGDT<>*zeros then
     c*                                                                          Y else N
     c                   if        cmclas<>0
     c                   Move (p)  CMCLAS        Z2WJCC                          CMCLAS(5,0)
     c                   else
     c     keyemp1       chain     pep01
     c                   if        %found
     c                   move (p)  epclas        z2wjcc
     c                   else
     c                   Move (p)  '00940'       z2wjcc                          CMCLAS(5,0)
     c                   endif
     c                   endif
     c*                                                                          Y else N
     c                   Movel(p)  CMJRST        Z2JRST                         CMJRST
     c                   Z-add     CMAWW         Z2CAWW                         CMAWW(7,2)
     c*
     c                   if        cmntam  < 0
     c                   move      '-'           sign
     c                   else
     c                   move      '+'           sign
     c                   endif
     c                   move      cmntam        z2wba
     c                   if        x1c1am  <> 0
     c                   movel     sign          z2wba
     c                   else
     c                   eval      z2wba = '+00000000'
     c                   endif                                                           amt <> 0
     c*
     c                   Z-add     CMOTWK        Z2WPO                          CMOTWK(7,2)
     c                   Z-add     CMPOWW        Z2PIWW                          CMPOWW(7,2)
     c*
     c                   Move      '00'          Z2STYP
     c                   if        z2csts='C'
     c                   Move      '09'          Z2STYP
     c                   endif
     c*
     c                   Z-add     *zeros        Z2VRED
     c                   Movel     xxmco         Z2NMCO                          CMMCO<>Y then 00/04
     c                   Movel     xxjrst        Z2FCI                          00ifCMJRST=CA,DE,PA
     c                   Movel     '01'          Z2LCA
     C*
     C                   if        cmodis='Y'
     c                   eval      Z2TLOS='02'
     c                   else
     c                   eval      Z2TLOS='01'
     c                   endif
     C*
     c                   Movel     '01'          Z2TCOV                         CMOCVT(2)
     c                   Movel     CMVORC        Z2VRIN                         CMVORC(1)
     c
     c                   Move      *blanks       Z2FIL5
     c                   Move      *blank        Z2DCFL
     c                   Move      *blanks       Z2DNFM
     c                   Move      *blanks       Z2DNAM
     c                   Move      *blanks       Z2DLSN
     c                   Move      *blanks       Z2DFSN
     c                   Move      *blanks       Z2FIL6
     c                   Move      *blanks       Z2DORG
     c                   Move      *zeros        Z2DAGE
     c                   Move      *blanks       Z2VIN
     c                   Move      *zeros        Z2IVYR
     c                   Move      *blanks       Z2IVMK
     c                   Move      *blanks       Z2MAT
     c                   Move      *blanks       Z2MATIN
     c                   Move      *blanks       Z2MAIA
     c                   Move      *blanks       Z2FIL7
     c                   Z-add     *zeros        Z2NODT
     c
     C                   eval      srdti = CMDTRP
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxDTRP            8 0
     c
     c                   Z-add     xxDTRP        Z2DTRP
     c                   eval      Z2ODE='+000000000000000000'
     c                   Movel     CMRECL        Z2RIND
     c                   Move      *blanks       Z2SUSE
     c*
     c                   Move      '0'           Z2IPCT
     c                   if        cmimp <> 'Y' or cmipct = 0
     c                   Move      '0'           Z2IPCT
     c                   else
     c                   if        cmipct = 100
     c                   Move      '1'           Z2IPCT
     c                   else
     c                   Move      '2'           Z2IPCT
     c                   endif
     c                   endif
     c*
     c                   if        cmrtw=0
     c                   Move      *blanks       Z2RTWS
     c                   else
     c                   if        cmfsal='Y'
     c                   Move      'Y'           Z2RTWS
     c                   else
     c                   Move      'N'           Z2RTWS
     c                   endif
     c                   endif
     c*
     c                   Move      *blanks       Z2FILL
     c                   endsr                                                   SRLOD20 endsr
      *=============================================================================================
      * SRLOD30
      *=============================================================================================
     c     srlod30       begsr
     c
     c                   add       1             xxseq30
     c*
     c
     c*
     c* Comp-bucket 1
     c*
     C                   eval      xxceam1=0
     C                   eval      xxceam2=0
     C                   eval      xxceam3=0
     C                   eval      xxceam4=0
     C                   eval      xxceam5=0
     C                   eval      xxceam6=0
     C                   eval      xxceam7=0
     C                   eval      xxceam8=0
     C                   eval      xxceam9=0
     C                   eval      xxceam10=0
     c*
     c                   if        CMRT01 ='CO'
     c                   eval      xxceam1=  CMRR01
     c                   endif
     c
     c                   if        CMRT02 ='CO'
     c                   eval      xxceam2=  CMRR02
     c                   endif
     c
     c                   if        CMRT03 ='CO'
     c                   eval      xxceam3=  CMRR03
     c                   endif
     c
     c                   if        CMRT04 ='CO'
     c                   eval      xxceam4=  CMRR04
     c                   endif
     c
     c                   if        CMRT05 ='CO'
     c                   eval      xxceam5=  CMRR05
     c                   endif
     c
     c                   if        CMRT06 ='CO'
     c                   eval      xxceam6=  CMRR06
     c                   endif
     c
     c                   if        CMRT07 ='CO'
     c                   eval      xxceam7=  CMRR07
     c                   endif
     c
     c                   if        CMRT08 ='CO'
     c                   eval      xxceam8=  CMRR08
     c                   endif
     c
     c                   if        CMRT09 ='CO'
     c                   eval      xxceam9=  CMRR09
     c                   endif
     c
     c                   if        CMRT10 ='CO'
     c                   eval      xxceam10 =  CMRR10
     c                   endif
     c*
     c* Populate the bucket 1 values
     c*
     c                   eval      xxceam = xxceam1 + xxceam2 + xxceam3 +
     c                             xxceam5 + xxceam6 + xxceam7 + xxceam8 +
     c                             xxceam8 + xxceam9 + xxceam10 + xxcea81 +
     c                             xxcea91
     c*
     c* Remove decimal
     c*
     c                   if        xxceam < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxceam=xxceam*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxceam        xxceamc          19
     c                   Z-ADD     xxceam        xxcea11          16 2
     c                   if        xxceam <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                           xxceam<> 0
     c*
     c                   movel     xxceamc       Z3C1AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxceam=xxceam*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxceam        x1C1AM           16 2
     c                   move      *blanks       xxceamc
     c*
     c* Medical-bucket 2
     c*
     C                   eval      xxceam1=0
     C                   eval      xxceam2=0
     C                   eval      xxceam3=0
     C                   eval      xxceam4=0
     C                   eval      xxceam5=0
     C                   eval      xxceam6=0
     C                   eval      xxceam7=0
     C                   eval      xxceam8=0
     C                   eval      xxceam9=0
     C                   eval      xxceam10=0
     c*
     c                   if        cmrt01 ='ME'
     c                   eval      xxceam1=  CMRR01
     c                   endif
     c
     c                   if        cmrt02 ='ME'
     c                   eval      xxceam2=  CMRR02
     c                   endif
     c
     c                   if        cmrt03 ='ME'
     c                   eval      xxceam3=  CMRR03
     c                   endif
     c
     c                   if        cmrt04 ='ME'
     c                   eval      xxceam4=  CMRR04
     c                   endif
     c
     c                   if        cmrt05 ='ME'
     c                   eval      xxceam5=  CMRR05
     c                   endif
     c
     c                   if        cmrt06 ='ME'
     c                   eval      xxceam6=  CMRR06
     c                   endif
     c
     c                   if        cmrt07 ='ME'
     c                   eval      xxceam7=  CMRR07
     c                   endif
     c
     c                   if        cmrt08 ='ME'
     c                   eval      xxceam8=  CMRR08
     c                   endif
     c
     c                   if        cmrt09 ='ME'
     c                   eval      xxceam9=  CMRR09
     c                   endif
     c
     c                   if        cmrt10 ='ME'
     c                   eval      xxceam10 =  CMRR10
     c                   endif
     c*
     c* Populate the bucket 2 values
     c*
     c                   eval      xxceam = xxceam1 + xxceam2 + xxceam3 +
     c                             xxceam5 + xxceam6 + xxceam7 + xxceam8 +
     c                             xxceam8 + xxceam9 + xxceam10 + xxcea82 +
     c                             xxcea92
     c*
     c* Remove decimal
     c*
     c                   if        xxceam < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxceam=xxceam*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxceam        xxceamc          19
     c                   Z-ADD     xxceam        xxcea22          16 2
     c                   if        xxceam <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                           xxceam<> 0
     c*
     c                   movel     xxceamc       Z3C2AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxceam=xxceam*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxceam        x1C2AM           16 2
     c                   move      *blanks       xxceamc
     c*
     C* Expense and Legal Reserves
     c*
     C                   eval      xxceam1=0
     C                   eval      xxceam2=0
     C                   eval      xxceam3=0
     C                   eval      xxceam4=0
     C                   eval      xxceam5=0
     C                   eval      xxceam6=0
     C                   eval      xxceam7=0
     C                   eval      xxceam8=0
     C                   eval      xxceam9=0
     C                   eval      xxceam10=0
     c*
     c
     c                   if        cmrt01 ='EX'
     c                   eval      xxceam1=  CMRR01
     c                   endif
     c
     c                   if        cmrt02 ='EX'
     c                   eval      xxceam2=  CMRR02
     c                   endif
     c
     c                   if        cmrt03 ='EX'
     c                   eval      xxceam3=  CMRR03
     c                   endif
     c
     c                   if        cmrt04 ='EX'
     c                   eval      xxceam4=  CMRR04
     c                   endif
     c
     c                   if        cmrt05 ='EX'
     c                   eval      xxceam5=  CMRR05
     c                   endif
     c
     c                   if        cmrt06 ='EX'
     c                   eval      xxceam6=  CMRR06
     c                   endif
     c
     c                   if        cmrt07 ='EX'
     c                   eval      xxceam7=  CMRR07
     c                   endif
     c
     c                   if        cmrt08 ='EX'
     c                   eval      xxceam8=  CMRR08
     c                   endif
     c
     c                   if        cmrt09 ='EX'
     c                   eval      xxceam9=  CMRR09
     c                   endif
     c
     c                   if        cmrt10 ='EX'
     c                   eval      xxceam10 =  CMRR10
     c                   endif
     c*
     c
     c                   if        cmrt01 ='LE'
     c                   eval      xxceam1=  xxceam1+CMRR01
     c                   endif
     c
     c                   if        cmrt02 ='LE'
     c                   eval      xxceam2=  xxceam2+CMRR02
     c                   endif
     c
     c                   if        cmrt03 ='LE'
     c                   eval      xxceam3=  xxceam3+CMRR03
     c                   endif
     c
     c                   if        cmrt04 ='LE'
     c                   eval      xxceam4=  xxceam4+CMRR04
     c                   endif
     c
     c                   if        cmrt05 ='LE'
     c                   eval      xxceam5=  xxceam5+CMRR05
     c                   endif
     c
     c                   if        cmrt06 ='LE'
     c                   eval      xxceam6=  xxceam6+CMRR06
     c                   endif
     c
     c                   if        cmrt07 ='LE'
     c                   eval      xxceam7=  xxceam7+CMRR07
     c                   endif
     c
     c                   if        cmrt08 ='LE'
     c                   eval      xxceam8=  xxceam8+CMRR08
     c                   endif
     c
     c                   if        cmrt09 ='LE'
     c                   eval      xxceam9=  xxceam9+CMRR09
     c                   endif
     c
     c                   if        cmrt10 ='LE'
     c                   eval      xxceam10 =  xxceam10+CMRR10
     c                   endif
     c*
     c* Populate the bucket 6 values
     c*
     c                   eval      xxceam = xxceam1 + xxceam2 + xxceam3 +
     c                             xxceam5 + xxceam6 + xxceam7 + xxceam8 +
     c                             xxceam8 + xxceam9 + xxceam10 + xxcea83 +
     c                             xxcea93
     c*
     c* Remove decimal
     c*
     c                   if        xxceam < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxceam=xxceam*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxceam        xxceamc          19
     c                   Z-ADD     xxceam        xxcea33          16 2
     c                   if        xxceam <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                           xxceam<> 0
     c*
     c                   movel     xxceamc       Z3C6AM
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxceam=xxceam*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   movel     xxceam        x1C6AM           16 2
     c                   movel     xxzeros       Z3C3AM
     c                   movel     xxzeros       Z3C4AM
     c                   movel     xxzeros       Z3C5AM
     c                   move      *blanks       xxceamc
     c*
     c                   eval      xxcea = xxcea11 + xxcea22 + xxcea33
     c
     c                   if        xxcea < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea=xxcea*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea         xxceamc          19
     c                   if        xxcea  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                           xxceam<> 0
     c*
     c                   movel     xxceamc       Z3CEAT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea=xxcea*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   movel     xxcea         x1CEAT           16 2
     c                   move      *blanks       xxceamc
     c*
     c*                  Record type 30
     c*
     c                   Move      xxTPAC        Z3TPA
     c                   Movel     CMFND         Z3ACCT
     c                   Movel     xxclm         Z3CLM#                          CMFND+CMFYR+CMCASE
     c                   Movel     'WC'          Z3ACCD                          CMCOVT
     c                   Movel     01            Z3CSEQ
     c                   Move      *blanks       Z3SFL1
     c                   Move      30            Z3TYPE
     c                   Movel     x3ind         Z3AIND
     c                   Movel     dsrundt       Z3ADT
     c                   Move      dstime        Z3ATIM
     c                   Move      *blanks       Z3SFL2
     c*                                                                          ?
     c                   eval      z3peam='+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000'
     c                   eval      z3peat='+000000000000000000'
      *
     c                   eval      z3ppda='+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000'
     c                   eval      z3ppdt='+000000000000000000'
      *
     c                   eval      z3ptra='+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000' +
     c                                    '+000000000000000000'
     c                   eval      z3ptrt='+000000000000000000'
     c                   Move      ' '           Z3FILL
     c*
     c                   endsr                                                   SRLOD30 endsr
      *=============================================================================================
      * SRLOD40
      *=============================================================================================
     c     srlod40       begsr
     c                   add       1             xxseq40
     c*
     c                   if        patype='V' or patype='S'
     c                   eval      paamt=paamt*-1
     c                   endif
     c*
     c* Payment kind code
     c*
     c                   if        paeob# <> 0
     c                   select
     c                   when      eobtyp='H'
     c
     c                   move      cmss#         kkss#
     c     keyfop        chain     peo01                              60
     c
     c                   move      eoco#         kkco#                           finding eublty
     c                   move      eosub#        kksub#                          billinng type
     c                   move      eofnd         kkfnd
     c                   move      eofyr         kkfyr
     c                   move      eocase        kkcase
     c                   move      eoeob#        kkeom#
     c     keyfup        chain     peu01                              61
     c
     c                   if        *in60 = *off and eublty = '13X'
     c                   move      '30HOP'       x4pkcd
     c                   else
     c                   move      '30HIP'       x4pkcd
     c                   endif
     c                   when      eobtyp='X'
     c                   move      '30DRG'       x4pkcd
     c                   other
     c                   move      '30PHY'       x4pkcd
     c                   endsl
     c                   else
     c                   select
     c                   when      paretp='L'
     c                   move      '35DJE'       x4pkcd
     c                   when      paretp='E'
     c                   move      '37MIS'       x4pkcd
     c                   when      paretp='M'
     c                   move      '30OTH'       x4pkcd
     c                   when      paretp='C'
     c                   move      '46LUP'       x4pkcd
     c                   endsl
     c                   endif
     c*
     c                   select
     c                   when      pabr2='29'
     c                   eval      x4pkcd='53CLE'
     c                   when      pabr2='D6'
     c                   eval      x4pkcd='30CHI'
     c                   when      pabr2='70'
     c                   eval      x4pkcd='37MCE'
     c                   when      pabr2='68'
     c                   eval      x4pkcd='37MCE'
     c                   when      pabr2='85'
     c                   eval      x4pkcd='30DME'
     c                   when      pabr2='MD'
     c                   eval      x4pkcd='30PHY'
     c                   when      pabr2='16'
     c                   eval      x4pkcd='30DME'
     c                   when      pabr2='07'
     c                   eval      x4pkcd='30HOP'
     c                   when      pabr2='HP'
     c                   eval      x4pkcd='30HIP'
     c                   when      pabr2='HI'
     c                   eval      x4pkcd='30HOP'
     c                   when      pabr2='10'
     c                   eval      x4pkcd='30IME'
     c                   when      pabr2='22'
     c                   eval      x4pkcd='37SUR'
     c                   when      pabr2='67'
     c                   eval      x4pkcd='35ATY'
     c                   when      pabr2='24'
     c                   eval      x4pkcd='37RPT'
     c                   when      pabr2='21'
     c                   eval      x4pkcd='30OTH'
     c                   when      pabr2='17'
     c                   eval      x4pkcd='30OTH'
     c                   when      pabr2='18'
     c                   eval      x4pkcd='30DME'
     c                   when      pabr2='01'
     c                   eval      x4pkcd='30DRG'
     c                   when      pabr2='15'
     c                   eval      x4pkcd='30PHT'
     c                   when      pabr2='20'
     c                   eval      x4pkcd='30TST'
     c                   when      pabr2='TT'
     c                   eval      x4pkcd='40TTD'
     c                   endsl
     c*
     c* Payment bucket code
     c*
     c                   if        PARETP='C'
     c                   z-add     1             X4PBNM            1 0
     c                   endif
     c                   if        PARETP='M'
     c                   z-add     2             X4PBNM
     c                   endif
     c                   if        PARETP='E' or PARETP='L'
     c                   z-add     6             X4PBNM
     c                   endif
     c
     c* Payee Tax Number Type
     c     cmss#         chain     PCD01                              60           PFILE(WMCDP)
     c                   if        *in60 =*off
     c                   move      'S'           X4TAXT            1
     c                   else
     c                   move      'N'           X4TAXT
     c                   endif
     c                   eval      x4fein =  pafein + pamod
     c
     c* If PAEOB# <> 0 then go out to FAEOP and see if EONTWK is blank.
     c                   if        paeob#<> 0
     c                   if        eontwk = ' '
     c                   move      'N'           X4PPOI            1
     c                   else
     c                   move      'Y'           X4PPOI
     c                   endif
     c                   endif
     c*
     c* Payee name
     c*
     c                   move      PACO#         kkco#
     c                   move      PAFND         kkfnd
     c                   move      PAFYR         kkfyr
     c                   move      PACASE        kkcase
     c                   move      PASEQ#        kkseq
     c*
     c     kychk1        chain     PRP01                              60                (WDRPP)
     c*
     c*                  Record type 40
     c*
     c                   Move      xxTPAC        Z4TPA
     c                   Movel     cmfnd         Z4ACCT                          CMFND+CMFYR+CMCASE
     c                   Movel     xxclm         Z4CLM#
     c                   Movel     'WC'          Z4ACCD
     c                   Movel     01            Z4ACSQ                          xxseq
     c                   Movel     X4PKCD        Z4PKCD                            ?
     c                   Movel     xxseq         Z4PSEQ
     c                   Movel     40            Z4TYPE
     c                   Movel     x4ind         Z4AIND                          "A"or"C", check prv
     c                   Movel     dsrundt       Z4PDT
     c                   Move      dstime        Z4PTIM
     c                   Move      *blanks       Z4SFIL
     c                   Z-add     X4PBNM        Z4PBNM                           payment bucket no#
     c*                  Z-add     PAAMT         Z4PLAM
     c*                  Z-add     PAAMT         Z4BAMT
     c                   if        PAAMT   < 0
     c                   move      '-000000000'  xxsign1          10
     c                   eval      paamt=paamt*-1
     c                   else
     c                   move      '+000000000'  xxsign1          10
     c                   endif
     c                   move      PAAMT         xxceamc          19
     c                   if        PAAMT   <> 0
     c                   movel     xxsign1       xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z4PLAM
     c                   move      xxceamc       Z4BAMT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign1= '-000000000'
     c                   eval      paamt=paamt*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c                   Move      PAACCT        Z4PBA#                          PAACCT(20)
     c                   Z-add     PACHK#        Z4CHK#                          PACHK#(7,0)
     c*
     C                   eval      srdti = PATRDT
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxTRDT1           8 0
     c*
     c                   Move      xxTRDT1       Z4PDAT                          PATRDT(7,0)
     c                   Movel     X4PPOI        Z4PPOI                          'N' or 'Y'
     c                   Movel(p)  PADESC        Z4NATP                          blank
     c                   Movel     RPPNAM        Z4PNAM                          RPPNAM
     c                   Movel     RPADR1        Z4PAD1                          RPADR1(40)
     c                   Movel     RPADR2        Z4PAD2                          RPADR2(40)
     c                   Movel     RPCTY         Z4PCTY                          RPCTY(25)
     c                   Movel     RPST          Z4PST                           RPST(2)
     c                   Movel     RPZIP         Z4PZIP                          RPZIP(9,0)
     c                   movel     cdctry        z4pcnt
     c                   if        z4pcnt=*blanks
     c                   Movel     'USA'         Z4PCNT
     c                   endif
     c                   Movel     X4TAXT        Z4TAXT
     c                   Movel     X4FEIN        Z4FEIN                          PAFEIN + PAMOD
     c*
     c                   Movel     '  '          Z4LSPC                          PAFEIN + PAMOD
     c                   if        z4pbnm=1
     c                   if        x4pkcd='30LUP' or x4pkcd='30MSA' or
     c                             x4pkcd='40TTL' or x4pkcd='44LUP' or
     c                             x4pkcd='46LUP' or x4pkcd='47LUP' or
     c                             x4pkcd='49LUP'
     c                   Movel     'SF'          Z4LSPC                          PAFEIN + PAMOD
     c                   endif
     c                   endif
     c*
     c                   Move      *blanks       Z4FILL
     c                   endsr                                                   SRLOD40 endsr
     c
      *=============================================================================================
      * SRLOD50
      *=============================================================================================
     c*                  Record type 50
     c     srlod50       begsr
     c*                  Record type 50
     c*
     c* Determine Recovery bucket no#
     c*
     c                   if        PARETP='C'
     c                   move      '1'           X5BNUM            1 0
     c                   endif
     c                   if        PARETP='M'
     c                   move      '2'           X5BNUM
     c                   endif
     c                   if        PARETP='E' or PARETP='L'
     c                   move      '6'           X5BNUM
     c                   endif
     c*
     c                   add       1             xxseq50
     c
     c                   Move      xxTPAC        Z5TPA
     c                   Movel     cmfnd         Z5ACCT
     c                   Movel     xxclm         Z5CLM#                          CMFND+CMFYR+CMCASE
     c                   Movel     'WC'          Z5ACCD                          CMCOVT(2)
     c                   Movel     01            Z5ASEQ
     c                   Movel     '66OTH'       Z5RPKC                          blank
     c                   Movel     CMPCTR        Z5RSEQ                          CMPCTR(5,0)
     c                   Movel     50            Z5TYPE
     c                   Movel     x5ind         Z5AIND                          A/C based on tp
     c                   Movel     dsrundt       Z5RDT
     c                   Move      dstime        Z5RTIM
     c                   Move      *blanks       Z5SFIL
     c                   Move      X5BNUM        Z5BNUM
     c**                 Z-add     PAAMT         Z5TPLA
     c                   if        PAAMT   < 0
     c*
     c                   move      '+000000000'  xxsign1          10
     c                   eval      paamt=paamt*-1
     c                   else
     c*
     c                   move      '-000000000'  xxsign1          10
     c                   endif
     c                   move      PAAMT         xxceamc          19
     c                   if        PAAMT   <> 0
     c                   movel     xxsign1       xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z5TPLA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign1= '+000000000'
     c                   eval      paamt=paamt*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   move      *blanks       xxceamc
     c*
     c                   Move      xxzeros       Z5TPRA
     c                   Move      PAACCT        Z5RBA#
     c                   Z-add     PACHK#        Z5RCHK
     c
     C                   eval      srdti = PATRDT
     C*                  call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtyymd
     C                   movel     srdto         xxTRDT1           8 0
     c
     c                   Move      xxTRDT1       Z5RTRD
     c                   Move      *blanks       Z5FILL
     c                   endsr                                                   SRLOD50 endsr
      *=============================================================================================
      * CLMPMT - Claims payment subroutine
      *=============================================================================================
     c*
     c     clmpmt        begsr
     c*
     c*    CO#/FND/FYR/CASE
     c
     c                   movel     cmco#         kkco#
     c                   movel     cmfnd         kkfnd
     c                   movel     cmfyr         kkfyr
     c                   movel     cmcase        kkcase
     c*
     c* reset the indicator for rcd50
     c*
     c                   eval      *in50 = '0'
     c*
     c                   eval      kktrdt=xxlrdt
     c
     c     keypap1       setgt     ppa05                                                 wdpap
     c     keypap        reade     ppa05                                  65             wdpap
     c                   dow       *in65 = *off
     c                   if        patrdt<=xxmedt
     c*
     c* payment sequence no for 40 format.
     c*
     c                   add       1             xxseq             5 0
     c*
     c                   if        PATYPE = 'R'
     c*
     c* write type 50 records
     c*
     c                   exsr      srlod50
     c                   write     pz501
     c*
     c                   else
     c*
     c* write type 40 records
     c*
     c                   exsr      srlod40
     c                   write     pz401
     c*
     c                   endif                                                   R,S,V,Z
     c*
     c                   endif
     c*
     c     keypap        reade     ppa05                                  65             wdpap
     c                   enddo                                                   wdpap file loop
     c*
     c                   endsr                                                   CLMPMT SR
      *=============================================================================================
      * CLMAMT - Claims amount subroutine
      *=============================================================================================
     c*
     c     clmamt        begsr
     c*
     c                   z-add     *zeros        xxcea81          16 2
     c                   z-add     *zeros        xxcea82          16 2
     c                   z-add     *zeros        xxcea83          16 2
     c                   z-add     *zeros        xxcea91          16 2
     c                   z-add     *zeros        xxcea92          16 2
     c                   z-add     *zeros        xxcea93          16 2
     c                   z-add     *zeros        xxcea1           16 2
     c                   z-add     *zeros        xxcea2           16 2
     c*
     c                   movel     cmco#         kkco#
     c                   movel     cmfnd         kkfnd
     c                   movel     cmfyr         kkfyr
     c                   movel     cmcase        kkcase
     c*
     c* Set the key list for rcd30 calculations
     c*
     c                   eval      kktrdt=0
     c
     c     keypap1       setll     ppa05                                                 wdpap
     c     keypap        reade     ppa05                                  75             wdpap
     c*
     c                   dow       *in75 = *off
     c                   if        patrdt<=xxmedt
     c*
     c                   eval      z3cpda = *zeros
     c                   if        PATYPE='P' or PATYPE='M' or
     c                             PATYPE='A' or PATYPE='V' or
     c                             PATYPE='Z' or PATYPE='S'
     c*
     c                   if        PATYPE='V' or PATYPE='S'
     c                   eval      paamt=paamt*-1
     c                   endif
     c*
     c* Comp-bucket 1
     c*
     c                   if        PARETP='C'
     c                   eval      x3cpda = paamt
     c                   add       x3cpda        xxcea81
     c                   endif
     c*
     c* Medical - bucket 2
     c*
     c                   if        paretp='M'
     c                   eval      x3cpda =  paamt
     c                   add       x3cpda        xxcea82
     c                   endif                                                   medical type
     c*
     c* Expense - bucket 3
     c*
     c                   if        paretp='E' or paretp='L'
     c                   eval      x3cpda =  paamt
     c                   add       x3cpda        xxcea83
     c                   endif                                                   Expense type
     c*
     c                   endif                                                   PARETP='P'
     c*
     c                   if        PATYPE='R'
     c*
     c                   eval      paamt=paamt*-1
     c*
     c* Comp-bucket 1
     c*
     c                   if        PARETP='C'
     c                   eval      x3ctra = paamt
     c                   add       x3ctra        xxcea91
     c                   endif
     c*
     c* Medical - bucket 2
     c*
     c                   if        PARETP='M'
     c                   eval      x3ctra = paamt
     c                   add       x3ctra        xxcea92
     c                   endif
     c*
     c* Expense - bucket 3
     c*
     c                   if        PARETP='E' or PARETP='L'
     c                   eval      x3ctra = paamt
     c                   add       x3ctra        xxcea93
     c                   endif
     c                   endif                                                          r,s,v,z
     c                   endif                                                          r,s,v,z
     c*
     c     keypap        reade     ppa05                                  75             wdpap
     c                   enddo                                                   do loop wdpap
     c*
     c* totals
     c*
     c*
     c* Populate the bucket 1 values
     c* Remove decimal
     c*
     c                   move      *blanks       xxceamc
     c                   if        xxcea81 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea81 = xxcea81*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea81       xxceamc          19
     c                   if        xxcea81 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                           amt <> 0
     c*
     c                   move      xxceamc       Z3C1DA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea81 = xxcea81*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea81       x1C1DA           16 2
     c                   move      *blanks       xxceamc
     c*
     c                   if        xxcea91 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea91 = xxcea91*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea91       xxceamc          19
     c                   if        xxcea91 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                       amt <> 0
     c*
     c                   movel     xxceamc       Z3C1RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea91 = xxcea91*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea91       x1C1RA           16 2
     c                   move      *blanks       xxceamc
     c*
     c* Populate the bucket 2 values
     c* Remove decimal
     c*
     c                   if        xxcea82 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea82 = xxcea82*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea82       xxceamc          19
     c                   if        xxcea82 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                         amt <> 0
     c*
     c                   movel     xxceamc       Z3C2DA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea82 = xxcea82*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea82       x1C2DA           16 2
     c                   move      *blanks       xxceamc
     c*
     c                   if        xxcea92 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea92 = xxcea92*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea92       xxceamc          19
     c                   if        xxcea92 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   eval      xxceamc = '+000000000000000000'
     c                   endif                                                     amt   <> 0
     c*
     c                   move      xxceamc       Z3C2RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea92 = xxcea92*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea92       x1C2RA           16 2
     c                   move      *blanks       xxceamc
     c*
     c*
     c* Populate the bucket 3 values
     c* Remove decimal
     c*
     c                   if        xxcea83 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea83 = xxcea83*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea83       xxceamc          19
     c                   if        xxcea83 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                       amt <> 0
     c*
     c                   move      xxceamc       Z3C6DA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea83 = xxcea83*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea83       x1C6DA           16 2
     c                   move      xxzeros       Z3C3DA
     c                   move      xxzeros       Z3C4DA
     c                   move      xxzeros       Z3C5DA
     c                   move      *blanks       xxceamc
     c*
     c                   if        xxcea93 < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea93 = xxcea93*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea93       xxceamc          19
     c                   if        xxcea93 <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                     amt   <> 0
     c*
     c                   movel     xxceamc       Z3C6RA
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea93 = xxcea93*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea93       x1C6RA           16 2
     c                   movel     xxzeros       Z3C3RA
     c                   movel     xxzeros       Z3C4RA
     c                   movel     xxzeros       Z3C5RA
     c                   move      *blanks       xxceamc
     c*
     c                   eval      xxcea1 = xxcea81 + xxcea82 + xxcea83
     c                   if        xxcea1  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea1 = xxcea1*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea1        xxceamc          19
     c                   if        xxcea1  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                     amt   <> 0
     c*
     c                   move      xxceamc       Z3CPDT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea1 = xxcea1*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea1        x1CPDT           16 2
     c                   move      *blanks       xxceamc
     c*
     c                   eval      xxcea2 = xxcea91 + xxcea92 + xxcea93
     c                   if        xxcea2  < 0
     c                   move      '-00'         xxsign            3
     c                   eval      xxcea2 = xxcea2*-1
     c                   else
     c                   move      '+00'         xxsign            3
     c                   endif
     c                   move      xxcea2        xxceamc          19
     c                   if        xxcea2  <> 0
     c                   movel     xxsign        xxceamc
     c                   else
     c                   evalr     xxceamc = '+000000000000000000'
     c                   endif                                                     amt   <> 0
     c*
     c                   move      xxceamc       Z3CTRT
     c***** beg add ***** 10/27/10 ************************************
     c* Make appropriate sign for final total
     c*
     c                   if        xxsign = '-00'
     c                   eval      xxcea2 = xxcea2*-1
     c                   endif
     c***** end add ***** 10/27/10 ************************************
     c                   add       xxcea2        x1CTRT           16 2
     c                   move      *blanks       xxceamc
     c*
     c                   endsr                                                   CLMAMT
      *=============================================================================================
      * WRITER - WRITES the extract file
      *=============================================================================================
     c     writer        begsr
     c*
     c     *loval        setll     pz002
     c                   read      pz002
     c*                  dow       not %eof(wzz0l)
     c*
     c                   movel     Z0Data        zudata
     c                   write(e)  pzu01
     c*
     c                   eval      kktpa = z0tpa
     c     ky10          setll     pz102
     c     ky10          reade     pz102
     c                   dow       not %eof(wzz1l)
     c*
     c                   movel     Z1Data        zudata
     c                   write(e)  pzu01
     c*
     c                   eval      kkacct = z1acct
     c     ky20          setll     pz202
     c     ky20          reade     pz202
     c                   dow       not %eof(wzz2l)
     c*
     c                   eval      kkclm# = z2clm#
     c     ky30          chain     pz302
     c                   if        %found
     c                   movel     Z2Data        zudata
     c                   write(e)  pzu01
     c                   movel     Z3Data        zudata
     c                   write(e)  pzu01
     c*
     c     ky40          setll     pz402
     c     ky40          reade     pz402
     c                   dow       not %eof(wzz4l)
     c                   movel     Z4Data        zudata
     c                   write(e)  pzu01
     c     ky40          reade     pz402
     c                   enddo                                              wzz4l
     c*
     c     ky40          setll     pz502
     c     ky40          reade     pz502
     c                   dow       not %eof(wzz5l)
     c                   movel     Z5Data        zudata
     c                   write(e)  pzu01
     c     ky40          reade     pz502
     c                   enddo                                              wzz5l
     c                   endif
     c*
     c     ky20          reade     pz202
     c                   enddo                                                   wzz3l
     c*
     c     ky10          reade     pz102
     c                   enddo                                                   wzz1l
     c*
     c                   endsr                                                   update count
**  nap - nature
0159
0202
0303
0404
0559
0659
0707
0859
0959
1010
1159
1259
1313
1459
1559
1616
1759
1859
1919
2059
2159
2222
2359
2459
2525
2659
2759
2828
2959
3030
3131
3232
3359
3434
3559
3636
3737
3859
3959
4040
4141
4242
4343
4459
4559
4646
4747
4859
4949
5059
5159
5252
5353
5454
5555
5657
5757
5858
5959
6060
6161
6262
6363
6464
6565
6666
6767
6868
6969
7070
7171
7272
7373
7474
7575
7676
7777
7878
7980
8080
8180
8280
8380
8480
8580
8680
8780
8880
8980
9090
9159
9259
9359
9459
9501
9659
9759
9859
9959
**  pap - part
0111
0211
0311
0412
0513
0613
0715
0816
0917
1018
1119
1226
1324
1430
1531
1638
1738
1838
1938
2032
2132
2233
2333
2434
2534
2635
2735
2836
2936
3037
3137
3240
3365
3441
3542
3643
3744
3845
3946
4047
4148
4249
4363
4425
4550
4651
4751
4852
4952
5053
5153
5254
5354
5455
5555
5656
5756
5857
5957
6058
6158
6290
6314
6414
6513
6661
6738
6838
6932
7033
7134
7235
7351
7453
7554
7655
7756
7814
7938
8061
8139
8220
8321
8422
8523
8660
8762
8864
8965
9066
9191
92
93
94
95
96
97
98
9999
**  cap - cause
01214
02
03
04
05
06177
07
08
09
10
11
12170
13212
14
15
16
17
18
19229
20
21
22
23
24
25210
26
27165
28
29209
30
31167
32
33169
34
35
36
37
38
39
40
41
42
43
44
45180
46
47178
48
49
50
51
52
53390
54
55157
56206
57
58
59
60162
61
62
63
64
65
66206
67
68
69
70196
71
72
73
74
75
76
77
78
79
80203
81196
82
83
84
85224
86
87
88221
89
90
91190
92
93
94
95
96
97
98
99190
A1393
A2392
A3220
A4395
