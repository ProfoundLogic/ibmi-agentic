     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0340R2 - CHANGE EAEPFL TO A 'P'                         *
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*                                                               *
     H* PROGRAMMER: C:94                                              *
     H* CREATED:    01/29/99  PROJECT: 48MBV                          *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 05/27/15  C:62 PROJECT: 114MRH                                *
     H*                Skip Eft eobs.                                 *
     H*                                                               *
     H* 09/07/01  C:62 PROJECT: FS231                                 *
     H*                UPDATE EAEPDT                                  *
     H*                                                               *
     H* 09/16/99  C:62 PROJECT: MC201                                 *
     H*                ADD RCN.                                       *
     H*                                                               *
     H* 07/27/99  C:62 PROJECT: MC201                                 *
     H*                UPDATE EOSTAT AND EAEPFL FROM FYEAP            *
     H*                                                               *
     H* 04/28/99  C:94 PROJECT: MC152                                 *
     H*                ADD UPDATE OF EOSTAT = P                       *
     H*                                                               *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H* 60 - GENERAL CHAIN INDICATOR                                  *
     H* 61 - GENERAL CHAIN INDICATOR                                  *
     H*                                                               *
     H*****************************************************************
     F*** EOMB File selected on EOSTAT *NE 'P'
     FFAEAL1    UF   E           K DISK
     F*** BEGIN ADD *** 07/27/99 **************************************
     F                                     RENAME(PEA01:PEA02)
     F*** END   ADD *** 07/27/99 **************************************
     F*
     F*** BEGIN DEL *** 07/27/99 **************************************
     F*** BEG ADD *** 04/28/99 ****************************************
     F**********FAEOLG  UF  E           K        DISK
     F*** END ADD *** 04/28/99 ****************************************
     F*** END   DEL *** 07/27/99 **************************************
     F*** BEGIN ADD *** 07/27/99 **************************************
     FFYEAP     IF   E             DISK
     F*** END   ADD *** 07/27/99 **************************************
     F*** BEGIN ADD *** 09/10/01 **************************************
     FSZDBP     IF   E             DISK
     F*** END   ADD *** 09/10/01 **************************************
     F*

     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Begin Add *** 05/27/15 *************************************
     D isEft           s               n   Inz(*Off)
     D #1rtcd          s              1
     D #1co#           s              3
     D #1sub#          s              5
     D #1fnd           s              3
     D #1outs          s              1
     D #1type          s              1
     D #1key           s             30
     D #1date          s              7
     D #1eft           s              1
     D #1pre           s              1
     D #1rrn           s              9
     D #1eobType       s              3

     D ##wcif          s              1                                         1
     D ##mcif          s              1                                         2
     D ##wcin          s              1                                         3
     D ##ar            s              1                                         4
     D ##ntwk          s              1                                         5
     D ##ur            s              1                                         6
     D ##med           s              1                                         7
     D ##edi           s              1                                         8
     D ##wcdk          s              1                                         9
     D ##rhab          s              1                                         10
     D ##mss           s              1                                         11
     D ##fsch          s              1                                         12
     D ##prvb          s              1                                         13
     D ##npwc          s              1                                         14
     D ##os            s              1                                         15
     D ##qrls          s              1                                         16
     D ##ccod          s              1                                         17
     D ##cchk          s              1                                         18
     D ##prom          s              1                                         19
     D ##mcrv          s              1                                         20
     D ##freq          s              1                                         21
     D ##crac          s              1                                         22
     D ##preo          s              1                                         23
     D ##parn          s              1                                         24
     D ##mgri          s              1                                         25
     D ##kcif          s              1                                         26
     D ##curs          s              1                                         27
     D ##pcib          s              1                                         28
     D ##clt#          s              3                                         29
     D ##ucpf          s              1                                         30
     D ##dcf           s              1                                         31
     D ##autb          s              1                                         32
     D ##deif          s              1                                         33
     D ##aure          s              1                                         34
     D ##cred          s              1                                         35
     D ##idrt          s              1                                         36
     D ##cprv          s              1                                         37
     D ##eft           s              1                                         38
     D ##govr          s              1                                         39
     D ##ncdc          s              1                                         40
     D ##splt          s              1                                         41
     D ##last          s              1                                         42
     D ##pblt          s              1                                         43
     D ##inze          s              1                                         44
     D*** End   Add *** 05/27/15 *************************************

     C*
     C*** BEGIN ADD *** 07/27/99 **************************************
     C     EAKY          KLIST
     C                   KFLD                    EACO#
     C                   KFLD                    EASUB#
     C                   KFLD                    EAFND
     C                   KFLD                    EAFYR
     C                   KFLD                    EACASE
     C                   KFLD                    EAEOB#
     C*** BEGIN ADD *** 09/16/99 **************************************
     C                   KFLD                    EARCN
     C*** END   ADD *** 09/16/99 **************************************
     C*** END   ADD *** 07/27/99 **************************************
     C*** BEGIN DEL *** 07/27/99 **************************************
     C*** BEG ADD *** 04/28/99 ***************************************
     C********** EOKEY     KLIST
     C**********           KFLD           EACO#
     C**********           KFLD           EASUB#
     C**********           KFLD           EAFND
     C**********           KFLD           EAFYR
     C**********           KFLD           EACASE
     C**********           KFLD           EAEOB#
     C*** END ADD *** 04/28/99 ***************************************
     C*** END   DEL *** 07/27/99 **************************************
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
     C*** BEGIN ADD *** 09/07/01 **************************************
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDYY'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** END   ADD *** 09/07/01 **************************************
     C*** BEGIN ADD *** 09/07/01 **************************************
     C     *LIKE         DEFINE    EAEPDT        XXEPDT
     C*** END   ADD *** 09/07/01 **************************************
     C*
     C*  INITIALIZE WORK FIELDS
     C*
     C*** Begin Add *** 05/27/15 *************************************
     C*
     C* Find out if WCMS is installed
     C*
     C                   Call      'SA001R'
     C                   Parm                    ##wcif                         WC INST
     C                   Parm                    ##mcif                         MC INST
     C                   Parm                    ##wcin                         WC INTERFACE
     C                   Parm                    ##ar                           MC AR INST
     C                   Parm                    ##ntwk                         NETWORK INST
     C                   Parm                    ##ur                           PRECERT INST
     C                   Parm                    ##med                          MEDMGMT INST
     C                   Parm                    ##edi                          EDI INST
     C                   Parm                    ##wcdk                         WC DARTS KEY
     C                   Parm                    ##rhab                         REHAB INST
     C                   Parm                    ##mss                          MS SUPPORT
     C                   Parm                    ##fsch                         FEE SCHEDULE
     C                   Parm                    ##prvb                         PROVIDER BILLS
     C                   Parm                    ##npwc                         NON-PDS WC  LS
     C                   Parm                    ##os                           OUTOME/SAVINGS
16   C                   Parm                    ##qrls                         Auto Release Flag
17   C                   Parm                    ##ccod                         Correct Coding Flag
18   C                   Parm                    ##cchk                         Claim Check Flag
19   C                   Parm                    ##prom                         ProMatch Flag
20   C                   Parm                    ##mcrv                         Medicare CR Flag
21   C                   Parm                    ##freq                         Frequency Flag
22   C                   Parm                    ##crac                         CR Auto Complete
23   C                   Parm                    ##preo                         CR Auto Complete
24   C                   Parm                    ##parn                         Par/Non-Par/Both
25   C                   Parm                    ##mgri                         Multi Group Restr
26   C                   Parm                    ##kcif                         Keep Claimant info
27   C                   Parm                    ##curs                         PBILLS entry cursor
28   C                   Parm                    ##pcib                         Prov Check in Batch
29   C                   Parm                    ##clt#                         Client Number
30   C                   Parm                    ##ucpf                         UCR Percentile
31   C                   Parm                    ##dcf                          Data Center
32   C                   Parm                    ##autb                         Auto Batch   Flag
33   C                   Parm                    ##deif                         DE/Image Flag
34   C                   Parm                    ##aure                         Auto-Reinstate
35   C                   Parm                    ##cred                         Credit Module
36   C                   Parm                    ##idrt                         Idarts Module
37   C                   Parm                    ##cprv                         Create Provider
38   C                   Parm                    ##eft                          EFT Flag
39   C                   Parm                    ##govr                         GO VERSION
40   C                   Parm                    ##ncdc                         NCCI data
41   C                   Parm                    ##splt                         SPLIT
42   C                   Parm                    ##last                         LA PARISH SALES TAX
43   C                   Parm                    ##pblt                         Provider bill light
44   C                   Parm                    ##inze                         Include $0 EOB
     C*** End   Add *** 05/27/15 *************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN ADD *** 09/07/01 **************************************
     C     1             CHAIN     PDB01                              99
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBSDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         XXEPDT
     C                   ENDIF
     C*** END   ADD *** 09/07/01 **************************************
     C*
     C* READ THE EOB FILE
     C*** BEGIN DEL *** 07/27/99 **************************************
     C********** *LOVAL    SETLLPEA01
     C*** END   DEL *** 07/27/99 **************************************
     C*
     C                   READ      PEA01                                  60
     C     *IN60         DOWEQ     *OFF
     C*** BEGIN ADD *** 07/27/99 **************************************
     C     EAKY          CHAIN     PEA02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*** Begin Add *** 05/27/15 *************************************
     C*
     C* Check to see if this is an eft
     C*
     C                   ExSr      SrProvEFT
     C*
     C                   If        isEft = *Off
     C*** End   Add *** 05/27/15 *************************************
     C*** END   ADD *** 07/27/99 **************************************
     C                   MOVEL     'P'           EAEPFL
     C*** BEGIN DEL *** 07/27/99 **************************************
     C**********           MOVEL'Y'       EAAPPF
     C*** END   DEL *** 07/27/99 **************************************
     C*** BEGIN ADD *** 09/07/01 **************************************
     C                   Z-ADD     XXEPDT        EAEPDT
     C*** END   ADD *** 09/07/01 **************************************
     C*** Begin Add *** 05/27/15 *************************************
     C                   Else
     C                   Eval      EAeftC = 'E'
     C                   EndIf
     C*** End   Add *** 05/27/15 *************************************
     C*
     C                   MOVE      Q1PGM         EAPGM
     C*** BEGIN ADD *** 09/07/01 **************************************
     C**********           TIME           DSDTTM
     C*** END   ADD *** 09/07/01 **************************************
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   Z-ADD     DSTIME        EATIME
     C*
     C*** BEGIN CHG *** 07/27/99 **************************************
     C**********           UPDATPEA01
     C                   UPDATE    PEA02
     C*
     C                   ENDIF
     C*** END   CHG *** 07/27/99 **************************************
     C*** BEGIN DEL *** 07/27/99 **************************************
     C*** BEG ADD *** 04/28/99 ***************************************
     C********** EOKEY     CHAINPEO01                99
     C********** *IN99     IFEQ *OFF
     C**********           MOVEL'P'       EOSTAT
     C*
     C**********           MOVE Q1PGM     EOPGM
     C**********           TIME           DSDTTM
     C*
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     EODT
     C*
     C**********           Z-ADDDSTIME    EOTIME
     C*
     C**********           UPDATPEO01
     C**********           ENDIF
     C*** END ADD *** 04/28/99 ***************************************
     C*** END   DEL *** 07/27/99 **************************************
     C*
     C                   READ      PEA01                                  60
     C                   ENDDO
      *
     C                   SETON                                        LR
     C*** Begin Add *** 05/27/15 *************************************
     C*================================================================
     C*
     C*  SrProvEft - Check to see if provider is an eft provider
     C*
     C*================================================================
     C     SrProvEft     BegSr
     C*
     C                   Eval      isEft = *Off
     C*
     C                   move      EAco#         #1co#
     C                   move      EAsub#        #1sub#
     C                   move      EAfnd         #1fnd
    C*
     C                   call      'FA701R'
     C                   parm      *blanks       #1rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm      *blanks       #1outs
    C*
    C* Only Check EFT if Workers Comp is inst and linked case
    C*
B04  C                   if        ##wcin = 'Y' and #1rtcd = *blank and
     C                             #1outs <> 'Y' and ##npwc <> 'Y' and
     C                             ##wcif = 'Y'
     C*
     C                   Eval      #1key = %EditC(EAfein:'X') +
     C                                     %EditC(EAmod:'X')
     C*
     C                   Z-Add     dsDate        srDtI
     C                   Call      'SRCVTDAT'    dtCYMD
     C                   MoveL     srDtO         #1Date
     C*
     C                   call      'WA023R3'
     C                   parm                    #1co#
     C                   parm      'P'           #1type
     C                   parm                    #1fnd
     C                   parm                    #1key
     C                   parm                    #1date
     C                   parm      ' '           #1eft
     C                   parm      ' '           #1pre
     C                   parm      '000000000'   #1rrn
     C                   parm      '   '         #1eobType
     C*
     C                   If        #1eft = *On And #1pre = *Off And
     C                             #1eobType = 'PDF'
     C                   If        EApaid <> 0 Or (EApaid = 0 and ##inze = 'Y')
     C                   Eval      isEft = *on
     C                   EndIf
     C                   EndIf
     C*
     C                   EndIf
     C*
     C                   EndSr
     C*** End   Add *** 05/27/15 *************************************
