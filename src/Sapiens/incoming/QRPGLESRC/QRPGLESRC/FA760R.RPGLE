     H**********************************************************************
     Hcopyright('(c) 2011 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**********************************************************************
     H* PROGRAM: FA760R
     H* DESCRIPTION: Retrieve PA Code Allowance
     H**********************************************************************
     H* CREATED: 10/08/08 C:212     PROJECT: 114MRJ
     H**********************************************************************
     H* REVISIONS:
     H*
     H* 06/22/11 C:257 Support: 77180 MC114
     H*                Chain to the Dental Detail file so that it returns
     H*                valid allowance and not zero for dental bill.
     H*
     H* 03/16/11 C:251 Support: 76242 MC114
     H*                Add rental allowances for DME codes
     H*
     H**********************************************************************
     F*
     Ffdcal13   if   e           k disk
     Ffaeolg    if   e           k disk
     Ffmzrlpa   if   e           k disk
     Ffmprl     if   e           k disk
     Ffddhl     if   e           k disk
     F*** Beg Add *** 06/22/11 ***********************************************
     Ffdddl     if   e           k disk
     F*** End Add *** 06/22/11 ***********************************************
     F*
     D kkco#           s              3  0 INZ                                  Company #
     D kksub#          s              5  0 INZ                                  Subscriber #
     D kkfnd           s              3  0 INZ                                  Fund #
     D kkfyr           s              3  0 INZ                                  Year
     D kkcase          s              7  0 INZ                                  Case
     D kkeob#          s              9  0 INZ                                  Eob#
     D kkdseq          s              5  0 INZ                                  Dtl Seq#
     D kkprcd          s              5    INZ                                  Procedure Code
     D kkefdt          s              7  0 INZ                                  Effective Date
     D kkzipa          s              5  0 INZ                                  Zip Code
     D kkmod1          s              2    INZ                                  Modifier1
     D kkmod2          s              2    INZ                                  Modifier2
     D kkmod3          s              2    INZ                                  Modifier3
     D kkmod4          s              2    INZ                                  Modifier4
     D xxallw          s              9  2 INZ                                  Allowable Amount
     D xxrgn#          s              3  0 INZ                                  Allowable Amount
     D*
     D srdti           S              8  0
     D srdto           S             10
     D srerr           S              1
     D srffmt          S              8
     D srtfmt          S              8
     D*
     D psds          esds                  extname(szq1p)
     D                 DS
     D DSDTTM                        14  0
     D  DSTIME                        6  0 OVERLAY(DSDTTM)
     D  DSDATE                        8  0 OVERLAY(DSDTTM:7)
     D*
     C*================================================================
     C* KEYLIST - DEFINITION
     C*================================================================
     C*
    C*  Procedure Code file
     C*
     C     prky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkprcd
     C                   kfld                    kkefdt
     C*
     C     prky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kkprcd
     C*
     C     caky1         klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
     C     eoky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C*
     C     dhky          klist
     C                   kfld                    kkco#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
     C                   kfld                    kkdseq
    C*
     C*  State Zip Code/Region File
     C     zrky          klist
     C                   kfld                    kkco#
     C                   kfld                    kkzipa
     C                   kfld                    kkefdt
    C*
     C*================================================================
     C* PARAMETER LIST
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@DSEQ            5 0
     C                   PARM                    @@PRCD            5
     C                   PARM                    @@EFDT            7 0
     C                   PARM                    @@ZIPA            5 0
     C                   PARM                    @@MOD1            2
     C                   PARM                    @@MOD2            2
     C                   PARM                    @@MOD3            2
     C                   PARM                    @@MOD4            2
     C                   PARM                    @@ALLW            9 2
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*================================================================
     C*  MAINLINE CODE
     C*================================================================
     C* Set key field variables
     C*
     C                   eval      kkco#  = @@co#
     C                   eval      kksub# = @@sub#
     C                   eval      kkfnd  = @@fnd
     C                   eval      kkfyr  = @@fyr
     C                   eval      kkcase = @@case
     C                   eval      kkeob# = @@eob#
     C                   eval      kkdseq = @@dseq
     C                   eval      kkprcd = @@prcd
     C                   eval      kkefdt = @@efdt
     C                   eval      kkzipa = @@zipa
     C                   eval      kkmod1 = @@mod1
     C                   eval      kkmod2 = @@mod2
     C                   eval      kkmod3 = @@mod3
     C                   eval      kkmod4 = @@mod4
     C*
     C* Read Procedure Code file to retrieve allowable amount
     C*
     C     prky          setgt     ppr01
     C     nxtpr3        tag
     C     prky1         readpe    ppr01                                  60
    C*
     C                   if        *in60 = *off
     C*
     C     prstat        cabeq     'I'           nxtpr3
     C*
     C     eoky          chain     peo01                              61
     C*
     C                   if        *in61 = *off
     C*
     C     dhky          chain     pdh01                              62
     C*** Beg Add *** 06/22/11 ***********************************************
     C     dhky          chain     pdd01                              63
     C*** End Add *** 06/22/11 ***********************************************
     C*
     C*** Beg Chg *** 06/22/11 ***********************************************
     C********           if        *in62 = *off
     C                   if        *in62 = *off or  *in63 = *off
     C*** End Chg *** 06/22/11 ***********************************************
     C                   exsr      loadamt
     C                   endif                                                  *in62 = *off
     C*
     C                   endif                                                  *in61 = *off
     C*
     C                   endif                                                  *in60 = *off
     C*
     C                   seton                                        LR
     C                   return
     C*
    C********************************************************************
    C** LOADAMT SUBROUTINE                             L O A D A M T    *
    C********************************************************************
     C     loadamt       begsr
     C*
     C                   eval      xxrgn# = 3
     C*
     C     zrky          setgt     pzr01
     C                   readp     pzr01                                  99
     C*
     C                   if        *in99 = *off and kkzipa >= zrfzcd
     C                             and kkzipa <= zrtzcd
     C*
     C                   eval      xxrgn# = zrrgn#
     C*
     C                   endif
     C*
     C* Load amount fields based on region
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C*
     C* Load the site of service amount for certain specialties
     C*
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             dhpos = ' 22')
     C                             and pr92nf <> 0
     C                   eval      xxallw = pr92nf
     C                   else
     C                   eval      xxallw = prmra
     C                   endif
     C*
     C                   when      xxrgn# = 2
     C*
     C* Load the site of service amount for certain specialties
     C*
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             dhpos = ' 22')
     C                             and pr92ng <> 0
     C                   eval      xxallw = pr92ng
     C                   else
     C                   eval      xxallw = pr92n3
     C                   endif
     C*
     C                   when      xxrgn# = 3
     C*
     C* Load the site of service amount for certain specialties
     C*
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             dhpos = ' 22')
     C                             and pr92nh <> 0
     C                   eval      xxallw = pr92nh
     C                   else
     C                   eval      xxallw = pr92n7
     C                   endif
     C*
     C*
     C                   when      xxrgn# = 4
     C*
     C* Load the site of service amount for certain specialties
     C*
     C                   if        (eospcd = 'LAB' or eospcd = 'PSY' or
     C                             dhpos = ' 22')
     C                             and pr92ni <> 0
     C                   eval      xxallw = pr92ni
     C                   else
     C                   eval      xxallw = pr92nb
     C                   endif
     C*
     C                   endsl
     C*
     C* Per 167.2 Payment for Anesthesia services we mult the conv factor
     C* by 1.632 on or after Dec 4, 2004
     C*
     C     caky1         chain     pca01                              63
     C                   if        *in63 = *off
     C*
     C                   if        cafdat >= 1041204
     C                             and cafdat <= 1041231
     C                             and kkprcd >= '00100'
     C                             and kkprcd <= '01999'
     C                   eval(H)   xxallw = xxallw * 1.632
     C                   endif
     C                   endif
    C*
     C                   if        kkmod1 = 'UE' or kkmod2 = 'UE' or
     C                             kkmod3 = 'UE' or kkmod4 = 'UE'
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxallw = pr92n2
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxallw = pr92n6
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxallw = pr92na
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxallw = pr92ne
     C*
     C                   endsl
     C*
     C                   endif
     C*** Beg Add *** 03/16/11 ***********************************************
     C                   if        kkmod1 = 'RR' or kkmod2 = 'RR' or
     C                             kkmod3 = 'RR' or kkmod4 = 'RR'
     C*
     C                   select
     C*
     C                   when      xxrgn# = 1
     C                   eval      xxallw = pranam
     C*
     C                   when      xxrgn# = 2
     C                   eval      xxallw = pr92nj
     C*
     C                   when      xxrgn# = 3
     C                   eval      xxallw = pr92nk
     C*
     C                   when      xxrgn# = 4
     C                   eval      xxallw = pr92nl
     C*
     C                   endsl
     C*
     C                   endif
     C*** End Add *** 03/16/11 ***********************************************
     C*
     C** Populate the parameter field to pass the value to calling program.
     C                   eval      @@allw = xxallw
     C*
     C     loadamte      endsr
    C********************************************************************
