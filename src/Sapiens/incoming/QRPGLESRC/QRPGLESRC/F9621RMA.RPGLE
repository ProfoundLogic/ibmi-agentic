    H*****************************************************************
    H** (C) COPYRIGHT PROGRESSIVE DATA SOLUTIONS, INC., 2003
    H*****************************************************************
    H*    PROGRAM: F9621RMA
    H*    DESCRIPTION: MA DATA SUBMISSION
    H*
    H*    CALLED FROM:
    H*      F9621RMA
    H*****************************************************************
    H** CREATED:
    H*  07/30/03 C:79 Proj: MA013
    H*****************************************************************
    H*  REVISIONS:
     H*
    H*  02/19/04 C:79 Supp: MC129 41288
    H*                1. Remove CPT Code as selection criteria
    H*                2. On phys bills exclude codes with modifiers
    H*                3. Zip Code - Only include the 5 digit code
     H*
    H*  10/28/03 C:79 Supp: MC129 39648
    H*                Update arrays for proc/diag codes
    H*****************************************************************
    H*  INDICATOR USAGE:
    H*  in25 - Used to indicate if Work Comp is installed
    H*  in98 - Used to indicate if an item is found using lookup
    H*  in99 - General Chain indicator
    H*****************************************************************
     Ffdm1pma   o    e             disk    usropn
     Fwmcml13   if   e           k disk    usropn
     Ffmcmlu    if   e           k disk    rename(pcm01:pcm02) prefix(C1:2)
     Ffmf1p     if   e             disk
     Ffaeolg    if   e           k disk
     Ffdcalh    if   e           k disk
     Ffaehl     if   e           k disk
     Ffaeul     if   e           k disk
     Ffddhl     if   e           k disk
     Ffddul     if   e           k disk
     Ffmcdl     if   e           k disk
     Ffmpdl     if   e           k disk
     Fsmupl     if   e           k disk
     D diag            s              5    dim(322) ctdata perrcd(1) ascend
     C*** BEG DEL *** 02/19/04 *********************************************
     D*prcd ********** s              5    dim(112) ctdata perrcd(1) ascend
     C*** END DEL *** 02/19/04 *********************************************
     D qca             s             56    dim(1) ctdata perrcd(1)
     D qqcmd           s             56
     D xxdate          s               D   datfmt(*usa)
     D zzdia           s              5a   varying
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D                 ds
     D xxclm#                        15
     D xxfnd                          3    overlay(xxclm#)
     D dash                           1    overlay(xxclm#:4) inz('-')
     D xxfyr                          3    overlay(xxclm#:5)
     D dash1                          1    overlay(xxclm#:8) inz('-')
     D xxcase                         7    overlay(xxclm#:9)
    D*
     D fids01        e ds                  extname(szq2p)
     D psds          esds                  extname(szq1p)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D kkcase          s                   like(eocase)
     D kkco#           s                   like(eoco#)
     D kkcpt4          s              5
     D kkeob#          s                   like(eoeob#)
     D kkdseq          s                   like(cadseq)
     D kkfdat          s              7  0
     D kkfnd           s                   like(eofnd)
     D k1fnd           s                   like(eofnd)
     D kkfyr           s                   like(eofyr)
     D kkss#           s                   like(cass#)
     D kksub#          s                   like(eosub#)
     D kkst            s                   like(eoast)
     D qqco#           s              3
     D qqsys           s              1
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D xxage           s              3  0
     D xxanam          s              3  0
     D xxanmn          s              3  0
     D xxdays          s              5  0
     D xxdt            s              8  0
     D xxerr           s              1
     D xxfrdt          s               d   datfmt(*usa)
     D yyfrdt          s              8  0
     D xxtodt          s               d   datfmt(*usa)
     D yytodt          s              8  0
     D zzfrdt          s              7  0
     D zztodt          s              7  0
     D xxlos           s              3  0
     D xxpmod          s              3  0
     D xxunts          s              3  0
     D ##wcif          s              1
     D ##mcif          s              1
     D ##wcin          s              1
     D #1co#           s              3
     D #1wcf#          s              3
     D #1sub#          s              5
     D #1fnd           s              3
     D length          s             15  5
     D xxstep          s              1  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*   PLISTs
    C*================================================================
    C*
    C*  Entry Parameters
    C*
     C     *entry        plist
     C                   parm                    qqfrdt            8
     C                   parm                    qqtodt            8
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
    C*================================================================
    C*   KLISTs
    C*================================================================
    C*
    C* Key List for the Company Master File
    C*
     C     cokey         klist
     C                   kfld                    upco#
    C*
    C* Key List for the User Parameter Master File
    C*
     C     upkey         klist
     C                   kfld                    q1user
    C*
    C* Key List for the Claim File
    C*
     C     cmky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkst
    C*
    C* Key List for the Claim File to select outside accounts
    C*
     C     cmky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkst
     C                   kfld                    k1fnd
    C*
    C* Key List for the Claim File
    C*
     C     cdky          klist
     C                   kfld                    kkss#
    C*
    C* Key List for the EOB header file
    C*
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
    C* Generic Key List for several files
    C*
     C     kkky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C* Generic Key List for the detail files
    C*
     C     k1ky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
    C*================================================================
    C*   MAIN PROGRAM - STEP 1: ALL EXCEPT OUTSIDE ACCOUNTS
    C*================================================================
    C*
     C                   exsr      pgmstr
     C*
     C* Cylce through all MA claims except for outside accounts
     C*
     C   25cmky          chain     pcm01                              99
     C  n25cmky          chain     pcm02                              99
     C*
     C                   dow       *in99 = *off
    C*
     C                   exsr      process
    C*
     C   25cmky          reade     pcm01                                  99
     C  n25cmky          reade     pcm02                                  99
    C*
     C                   enddo
    C*
    C*================================================================
    C*   MAIN PROGRAM - STEP 2: OUTSIDE ACCOUNTS IF WC INSTALLED
    C*================================================================
    C*
     C                   eval      xxstep = 2
    C*
    C* Process outside accounts if Work Comp is installed
    C*
     C                   if        *in25 = *on
    C*
     C                   read      pf101                                  99
     C                   dow       *in99 = *off
    C*
    C* If outside account read MA claims for the group
    C*
     C                   if        f1outa = 'Y'
    C*
     C                   eval      k1fnd = f1fnd
    C*
     C     cmky1         chain     pcm02                              99
    C*
     C                   dow       *in99 = *off
    C*
     C                   exsr      process
    C*
     C     cmky1         reade     pcm02                                  99
    C*
     C                   enddo
    C*
     C                   endif
    C*
     C                   read      pf101                                  99
    C*
     C                   enddo
    C*
     C                   endif                                                  *in25 = *on
    C*
    C*  End of Program
    C*
     C                   exsr      pgmend
     C                   move      *on           *inlr
    C*
    C*================================================================
    C*   KEYSET - Subroutine to set key values
    C*================================================================
     C     keyset        begsr
     C*
     C* Get the Managed Care Sub/Group
     C*
     C                   clear                   kksub#
     C                   clear                   kkfnd
     C*
     C                   select
     C*
     C                   when      *in25 = *on and xxstep = 1
     C*
     C                   eval      kkfyr  = cmfyr
     C                   eval      kkcase = cmcase
     C                   eval      kkss#  = cmss#
     C*
     C                   move(p)   upco#         #1co#
     C                   move(p)   cmfnd         #1wcf#
    C*
     C                   call      'FA700R'
     C                   parm      *blanks       ##rtcd            1
     C                   parm                    #1co#
     C                   parm                    #1wcf#
     C                   parm      *blanks       #1sub#
     C                   parm      *blanks       #1fnd
     C*
     C                   if        ##rtcd <> 'F'
     C                   move(p)   #1sub#        kksub#
     C                   move(p)   #1fnd         kkfnd
     C                   endif
     C*
     C                   when      *in25 = *off or xxstep = 2
     C
     C                   eval      kksub# = c1sub#
     C                   eval      kkfnd  = c1fnd
     C                   eval      kkfyr = c1fyr
     C                   eval      kkcase = c1case
     C                   eval      kkss#  = c1ss#
     C
     C                   endsl
    C*
     C                   endsr
    C*
    C*================================================================
    C*   PGMEND - Subroutine for Program end
    C*================================================================
     C     pgmend        begsr
    C*
     C                   if        ##wcif = 'Y'
     C                   close     WMCML13
     C                   endif
     C*
     C                   close     fdm1pma
    C*
     C                   endsr
    C*
    C*================================================================
    C*   PGMSTR - Subroutine for Program start
    C*================================================================
     C     pgmstr        begsr
     C*
     C*  Clear the output file and open it
     C*
     C                   eval      length = 56
     C                   movel(p)  qca(1)        qqcmd
     C                   call      'QCMDEXC'
     C                   parm                    qqcmd
     C                   parm                    length
     C*
     C                   open      fdm1pma
    C*
    C*  GET THE USER PARAMETER FILE
    C*
     C     upkey         chain     pup01                              60
     C                   if        *in60 = *on
     C                   clear                   upco#
     C                   endif
    C*
     C                   eval      kkco# = upco#
     C                   eval      kkst  = 'MA'
    C*
     C                   time                    dsdttm
     C*
     C*  Determine if WC is installed. If so open WMCML13.
     C*
     C                   call      'SA001R'
     C                   parm      *blank        ##wcif
     C                   parm      *blank        ##mcif
     C                   parm      *blank        ##wcin
     C*
     C                   if        ##wcif = 'Y'
     C                   open      WMCML13
     C                   seton                                        25
     C                   else
     C                   setoff                                       25
     C                   endif
    C*
    C* Convert the from/to dates
    C*
     C                   move      qqfrdt        yyfrdt
     C                   move      yyfrdt        xxfrdt
     C     *cymd         move      xxfrdt        zzfrdt
    C*
     C                   move      qqtodt        yytodt
     C                   move      yytodt        xxtodt
     C     *cymd         move      xxtodt        zztodt
    C*
    C* Initialize work fields
    C*
     C                   eval      xxstep = 1
    C*
     C                   endsr
    C*
    C*================================================================
    C*   PROCESS - Subroutine to Process the Back Injury Data
    C*================================================================
     C     process       begsr
    C*
     C                   exsr      keyset
     C*
     C* Cylce through EOB file for all EOB's for this case
     C*
     C     eoky          chain     peo01                              99
     C                   dow       *in99 = *off
     C*
     C                   if        eobtyp = 'N'
     C                             and eowcck >= zzfrdt and eowcck <= zztodt
     C                             and eoblst = 'C'
     C                             or eobtyp = 'H'
     C                             and eowcck >= zzfrdt and eowcck <= zztodt
     C                             and eoblst = 'C'
     C*
     C                   eval      kkeob# = eoeob#
     C*
     C* See if the diag code is in the list
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C     kkky          chain     peh01                              99
     C                   movel(p)  ehdia1        xxdia             5
     C     xxdia         lookup    diag                                   98
     C*
     C                   when      eobtyp = 'H'
     C     kkky          chain     peu01                              99
     C                   movel(p)  eudia1        xxdia
     C     xxdia         lookup    diag                                   98
     C*
     C                   endsl
     C*
     C* The diag code is in the list now read through the line items
     C*
     C                   if        *in98 = *on
     C*
     C     kkky          chain     pca01                              99
     C*
     C                   dow       *in99 = *off
     C*
     C                   eval      kkdseq = cadseq
     C*
     C                   setoff                                       98
     C*
     C*** BEG DEL *** 02/19/04 *********************************************
     C****************** select
     C******************
     C******************
     C****************** when      eobtyp = 'N'
     C*****k1ky********* chain     pdh01                              99
     C*****dhprcd******* lookup    prcd                                   98
     C******************
     C******************
     C****************** when      eobtyp = 'H'
     C*****k1ky********* chain     pdu01                              99
     C*****durate******* lookup    prcd                                   98
     C******************
     C****************** endsl
     C******************
     C**Proc*code*is*in*the list also so write the data to FDM!PMA
     C******************
     C****************** if        *in98 = *on
     C******************
     C****************** exsr      writedta
     C******************
     C****************** endif
     C*** END DEL *** 02/19/04 *********************************************
     C*** BEG ADD *** 02/19/04 *********************************************
     C*
     C* If this is a physician bill and the line has a modifier exclude
     C*
     C                   select
     C*
     C                   when      eobtyp = 'N'
     C*
     C     k1ky          chain     pdh01                              99
     C                   if        *in99 = *off
     C
     C                   if        dhmod1 = *blanks and dhmod2 = *blanks and
     C                             dhmod3 = *blanks and dhmod4 = *blanks
     C                   exsr      writedta
     C                   endif
     C*
     C                   endif
     C*
     C                   when      eobtyp = 'H'
     C*
     C     k1ky          chain     pdu01                              99
     C*
     C                   if        *in99 = *off and durate <> *blanks
     C                   exsr      writedta
     C                   endif
     C*
     C                   endsl
     C*** END ADD *** 02/19/04 *********************************************
     C
     C     kkky          reade     pca01                                  99
     C
     C                   enddo                                                  *in99 = *off
     C
     C                   endif                                                  *in98 = *on
     C*
     C                   endif                                                  eobtyp = N
     C*
     C     eoky          reade     peo01                                  99
     C*
     C                   enddo
    C*
     C                   endsr
    C*
    C*================================================================
    C*   WRITEDTA - Write required fields to FDM1PMA
    C*================================================================
     C     writedta      begsr
     C*
     C                   clear                   pm101
     C*
     C* Get claim demographic data
     C*
     C* Gender
     C*
     C     cdky          chain     pcd01                              99
     C*
     C                   if        *in99 = *off
     C*
     C                   eval      m1gend = cdsex
     C*
     C* Date of Birth
     C*
     C                   if        cddtob <> 0
     C     *cymd         move      cddtob        xxdate
     C                   movel     xxdate        m1dob
     C                   endif                                                  cddtob <> 0
     C*
     C* Claimant Zip Code
    C*
     C*** BEG CHG *** 02/19/04 ****************************************
     C*****************  movel(p)  cdzip         xxzip             9
     C*****************  eval      m1zip = %subst(xxzip:1:5) + '-'
     C*****************                   + %subst(xxzip:6:4)
     C                   movel     cdzip         m1zip
     C*** END CHG *** 02/19/04 ****************************************
     C*
     C                   endif                                                  *in99 = *off
     C*
     C* Date of Injury
    C*
     C                   select
     C*
     C                   when      *in25 = *on and xxstep = 1
     C*
     C     *cymd         move      cmacdt        xxdate
     C*
     C                   when      *in25 = *off or xxstep = 2
     C*
     C     *cymd         move      c1acdt        xxdate
     C*
     C                   endsl
     C*
     C                   movel     xxdate        m1acdt
     C*
     C* Date of Treatment
    C*
     C     *cymd         move      cafdat        xxdate
     C                   movel     xxdate        m1svdt
     C*
     C* Procedure Code
    C*
     C                   select
     C                   when      eobtyp = 'N'
     C*** BEG CHG *** 02/19/04 ****************************************
     C****************** eval      m1prcd = dhprcd + dhmod1 + dhmod2
     C                   eval      m1prcd = dhprcd
     C*** END CHG *** 02/19/04 ****************************************
     C*
     C                   when      eobtyp = 'H'
     C                   eval      m1prcd = durate
     C*
     C                   endsl
     C*
     C* Diagnosis Code - Move to num field first to replace ' ' with '0'
    C*
     C                   move      *zeros        m1diag
     C                   eval      zzdia = %trimr(xxdia)
     C                   movel     zzdia         m1diag
     C*
     C* Insurers FEIN
    C*
     C*
     C*
     C* Claim Number
     C*
     C                   select
     C*
     C                   when      *in25 = *on and xxstep = 1
     C*
     C                   move(p)   cmfnd         xxfnd
     C                   move(p)   cmfyr         xxfyr
     C                   move(p)   cmcase        xxcase
     C*
     C                   when      *in25 = *off or xxstep = 2
     C*
     C                   move(p)   c1fnd         xxfnd
     C                   move(p)   c1fyr         xxfyr
     C                   move(p)   c1case        xxcase
     C*
     C                   endsl
    C*
     C                   movel(p)  xxclm#        m1case
     C*
     C* Providers FEIN
    C*
     C                   movel(p)  eofein        m1pfei
     C*
     C* Providers Zip
    C*
     C*** BEG CHG *** 02/19/04 ****************************************
     C****************** movel(p)  eozip         xxzip             9
     C****************** eval      m1pzip = %subst(xxzip:1:5) + '-'
     C******************                  + %subst(xxzip:6:4)
     C                   movel     eozip         m1pzip
     C*** END CHG *** 02/19/04 ****************************************
     C*
     C                   write     pm101
     C*
     C                   endsr
** diag
V570
V571
V5721
V574
3531
3532
3534
3550
71615
71618
71625
71628
71645
71648
71655
71658
71665
71668
71685
71688
71695
71698
71805
71808
71815
71818
71825
71828
71835
71838
71845
71848
71875
71878
71885
71888
71895
71898
71945
71948
72010
72020
72089
72090
7210
7211
7212
7213
72141
72142
7215
7216
7217
7218
72190
7220
72210
72211
7222
7224
72251
72252
7226
72270
72271
72272
72273
72290
72291
72292
72293
7230
7231
7232
7233
7234
7239
72400
72401
72402
72409
7241
7242
7243
7244
7245
7246
72479
7248
7249
7327
7328
7329
73340
7384
7385
7391
7392
7393
7394
7395
74259
7429
80500
80501
80502
80503
80504
80505
80506
80507
80508
80510
80511
80512
80513
80514
80515
80516
80517
80518
80520
80521
80522
80523
80524
80525
80526
80527
80528
80530
80531
80532
80533
80534
80535
80536
80537
80538
80540
80541
80542
80543
80544
80545
80546
80547
80548
80550
80551
80552
80553
80554
80555
80556
80557
80558
80560
80561
80562
80563
80564
80565
80566
80567
80568
80570
80571
80572
80573
80574
80575
80576
80577
80578
80580
80581
80582
80583
80584
80585
80586
80587
80588
80590
80591
80592
80593
80594
80595
80596
80597
80598
80600
80601
80602
80603
80604
80605
80606
80607
80608
80609
80610
80611
80612
80613
80614
80615
80616
80617
80618
80619
80620
80621
80622
80623
80624
80625
80626
80627
80628
80629
80630
80631
80632
80633
80634
80635
80636
80637
80638
80639
80640
80650
80660
80661
80662
80669
80670
80671
80672
80679
80680
80690
83900
83901
83902
83903
83904
83905
83906
83907
83908
83910
83911
83912
83913
83914
83915
83916
83917
83918
83920
83921
83930
83931
83940
83941
83942
83949
83950
83951
83952
83959
8460
8461
8462
8463
8468
8469
8471
8472
8473
8474
8479
8485
92231
92611
95200
95201
95202
95203
95204
95205
95206
95207
95208
95209
95210
95211
95212
95213
95214
95215
95216
95217
95218
95219
9522
9523
9524
9528
9529
9530
9531
9532
9533
9535
95380
95390
**  (QCMDEX)
CLRPFM FDM1PMA
