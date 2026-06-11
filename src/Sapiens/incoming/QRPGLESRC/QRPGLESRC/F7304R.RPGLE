     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE W7304R - UPDATE OF FAEOP AND FAEAP
     H*****************************************************************
     H** PROGRAM DESCRIPTION:                                         *
     H**   THIS PROGRAM WILL UPDATE FAEOP AND FAEAP                   *
     H**                                                              *
     H** CALLED FROM:                                                 *
     H**                                                              *
     H**   W7000R                                                     *
     H**                                                              *
     H** CALLS TO:                                                    *
     H**                                                              *
     H** PROGRAMMER: C:94 - MIKE MAXWELL                              *
     H** CREATED:    12/02/98         PROJECT: 25MKL                  *
     H**                                                              *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  05/27/15 C:062 Project: 114MRH                               *
     H*                 Update eft flag in FAEAP.                     *
     H*                                                               *
     H*  05/29/03 C:062 Project: 129MBO                               *
     H*                 Add processing for types "1", "2" and "3".    *
     H*                                                               *
     H*  07/02/01 C:062 Project: MC111 28066                          *
     H*                 Correct Recon usage.                          *
     H*                                                               *
     H*  08/03/00 C:117 Project: FL032                                *
     H*                 Depending on what is installed, update EAESDT *
     H*                 and EOWCCK.  May need to be updated in        *
     H*                 in F7304R instead.                            *
     H*                                                               *
     H*  03/02/00  C:062 PROJECT: MCMNT                               *
     H*                  IF NETWORK IT NEEDS TO CHECK PASSED IN PARM  *
     H*                  TO DETERMINE IF INHOUSE NETWORK OR BRS.      *
     H*                                                               *
     H*  02/07/00  C:062 PROJECT: Y2K                                 *
     H*                  CHANGE EAWC DATES TO YYMD.                   *
     H*                                                               *
     H*  02/02/00  C:062 PROJECT: MC221                               *
     H*                  DON'T UPDATE EOUSER AND EAUSER               *
     H*                                                               *
     H*  12/17/99  C:062 PROJECT: Y2K                                 *
     H*                  FIXED DATES.                                 *
     H*                                                               *
     H*  09/21/99  C:117 PROJECT: FL021                               *
     H*                  ADDED A CALL TO SA001R TO SEE IF @@PRVB IS   *
     H*                  INSTALLED; EOWCCK IS NOT TO BE POPULATED     *
     H*                  IF @@PRVB='Y'.                               *
     H*                                                               *
     H*  06/10/99  C:79  PROJECT: SUP17939                            *
     H*                  ADD RECON NUMBER TO ENTRY PARMS SO THAT      *
     H*                  CHECK DATES/NUMBERS ARE WRITTEN CORRECTLY    *
     H*                  TO FAEAP.                                    *
     H*                                                               *
     H*  05/10/99  C:94  PROJECT: SUP17993                            *
     H*                  MOVE UPDATE OF FAEAP INSIDE OF IF STATEMENT  *
     H*                  TO KEEP FROM GETTING ERROR                   *
     H*                                                               *
     H*****************************************************************
     F*
     FFAEOLG    UF   E           K DISK
     F*
     FFAEAL1    UF   E           K DISK
     F*
     F*** Begin Add *** 05/27/15 ******************************************
     Fwarpln    if   e           k Disk    Rename(prp01:prp0n)
     F*** End   Add *** 05/27/15 ******************************************

     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*** Begin Add *** 05/27/15 ***********************************************
     D  ##rtcd         s              1
     D  ##co#          s              3
     D  ##wcf#         s              3
     D  ##sub#         s              5
     D  ##fnd          s              3
     D*** End   Add *** 05/27/15 ***********************************************

     I*

     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*** BEGIN ADD *** 12/17/99 **************************************
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** END   ADD *** 12/17/99 **************************************
     C*** BEGIN ADD *** 02/07/00 **************************************
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*** END   ADD *** 02/07/00 **************************************
     C*
     C     EOKEY         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     EAKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKRCN
     C*** Begin Add *** 05/27/15 ******************************************
     C*
     C     keyWARPLN     KLIST
     C                   KFLD                    EAco#
     C                   KFLD                    kkWcf#
     C                   KFLD                    EAfYr
     C                   KFLD                    EAcase
     C                   KFLD                    EAeob#
     C                   KFLD                    kkEobs
     C*** End   Add *** 05/27/15 ******************************************
     C*
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EORCN         KKRCN
     C*** Begin Add *** 05/27/15 ******************************************
     C     *Like         Define    RPfnd         kkWcf#
     C     *Like         Define    RPeobs        kkEobs
     C*** End   Add *** 05/27/15 ******************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@FND             3 0
     C                   PARM                    @@FYR             3 0
     C                   PARM                    @@CASE            7 0
     C                   PARM                    @@EOB#            9 0
     C                   PARM                    @@CH#             7 0
     C                   PARM                    @@CHDT            7 0
     C                   PARM                    @@CHAT            9 2
     C                   PARM                    @@CHTY            1
      *** BEG ADD *** 06/10/99 ****************************************
     C                   PARM                    @@RCN             7 0
      *** END ADD *** 06/10/99 ****************************************
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   PARM                    @@NTWK            1
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
     C                   MOVE      @@FYR         KKFYR
     C                   MOVE      @@CASE        KKCASE
     C                   MOVE      @@EOB#        KKEOB#
     C*
     C                   MOVE      @@CO#         ##CO#
     C                   MOVE      @@FND         ##WCF#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANK        ##RTCD            1
     C                   PARM                    ##CO#             3
     C                   PARM                    ##WCF#            3
     C                   PARM      *BLANKS       ##SUB#            5
     C                   PARM      *BLANKS       ##FND             3
     C*
     C     ##RTCD        IFEQ      ' '
     C                   MOVE      ##SUB#        KKSUB#
     C                   MOVE      ##FND         KKFND
     C                   ENDIF
     C*
     C*** BEG ADD *** 03/20/99 ***************************************
     C*
     C     EOKEY         CHAIN     PEO01                              99
      *** BEG DEL *** 06/10/99 ***************************************
     C********** *IN99     IFEQ *OFF
     C**********           MOVE EORCN     KKRCN
     C**********           ENDIF
      *** END DEL *** 06/10/99 ***************************************
      *** BEGIN ADD *** 07/02/01 **************************************
     C                   Z-ADD     @@RCN         KKRCN
      *** END   ADD *** 07/02/01 **************************************
     C*
     C*** END ADD *** 03/20/99 ***************************************
     C                   SELECT
     C*
     C     @@CHTY        WHENEQ    'P'
     C                   EXSR      PUPDAT
     C*
     C     @@CHTY        WHENEQ    'B'
     C*** BEGIN ADD *** 03/02/00 **************************************
     C     @@NTWK        IFEQ      'B'
     C*** END   ADD *** 03/02/00 **************************************
     C                   EXSR      BUPDAT
     C*** BEGIN ADD *** 03/02/00 **************************************
     C                   ELSE
     C*
     C                   EXSR      NUPDAT
     C*
     C                   ENDIF
     C*** END   ADD *** 03/02/00 **************************************
     C*
     C     @@CHTY        WHENEQ    'N'
     C                   EXSR      NUPDAT
     C*** BEGIN ADD *** 05/29/03 **************************************
     C*
     C     @@CHTY        WHENEQ    '1'
     C                   EXSR      ONEDAT
     C*
     C     @@CHTY        WHENEQ    '2'
     C                   EXSR      TWODAT
     C*
     C     @@CHTY        WHENEQ    '3'
     C                   EXSR      THRDAT
     C*** END   ADD *** 05/29/03 **************************************
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           *INLR
     C*
     C*================================================================
     C* PUPDAT - UPDATE FAEOP AND FAE1P FOR PROVIDER CHECK.
     C*================================================================
     C     PUPDAT        BEGSR
     C*
     C     EOKEY         CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C*
      *** BEG DEL *** 06/10/99 ****************************************
     C******************** MOVE EORCN     KKRCN
      *** END DEL *** 06/10/99 ****************************************
     C*** BEG ADD *** 09/21/99 ****************************************
     C*
     C* Call API to see if Prov Bills is installed
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           @@WCIF            1            WC INST
     C                   PARM      ' '           @@MCIF            1            MC INST
     C                   PARM      ' '           @@WCIN            1            WC INTERFACE
     C                   PARM      ' '           @@AR              1            MC AR INST
     C                   PARM      ' '           @@NTWK            1            NETWORK INST
     C                   PARM      ' '           @@UR              1            PRECERT INST
     C                   PARM      ' '           @@MED             1            MEDMGMT INST
     C                   PARM      ' '           @@EDI             1            EDI INST
     C                   PARM      ' '           @@WCDK            1            WC DARTS KEY
     C                   PARM      ' '           @@RHAB            1            REHAB INST
     C                   PARM      ' '           @@MSS             1            MS SUPPORT
     C                   PARM      ' '           @@FSCH            1            FEE SCHEDULE
     C                   PARM      ' '           @@PRVB            1            PROVIDER BILLS
     C                   PARM      ' '           @@NPWC            1            NON-PDS WC  LS
     C                   PARM      ' '           @@OS              1            OUTOME/SAVINGS
     C*** BEG ADD *** 09/21/99 ****************************************
     C*** BEG CHG *** 09/21/99 ***PUT COND AROUND MOVE*****************
     C     @@PRVB        IFNE      'Y'
     C                   MOVE      @@CH#         EOWCCK
     C*** BEG ADD *** 08/03/00 ****************************************
     C                   ELSE
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOWCCK
     C                   MOVEL     EORCN         KKRCN
     C*
     C     EAKY          CHAIN     PEA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     SRDTO         EAESDT
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C*
     C                   ENDIF
     C*** END ADD *** 08/03/00 ****************************************
     C                   ENDIF
     C*** END CHG *** 09/21/99 ****************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 02/02/00 **************************************
     C**********           MOVELQ1USER    EOUSER
     C*** END   DEL *** 02/02/00 **************************************
     C                   MOVEL     Q1PGM         EOPGM
     C                   MOVEL     DSTIME        EOTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EODT
     C*
     C                   UPDATE    PEO01
     C*
      *** BEG ADD *** 06/10/99 ****************************************
     C                   Z-ADD     @@RCN         KKRCN
      *** END ADD *** 06/10/99 ****************************************
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAWCPC
     C*** BEGIN ADD *** 12/17/99 *************************************
     C**********           MOVE @@CHDT    EAWCPD
     C                   Z-ADD     @@CHDT        SRDTI
     C*** BEGIN CHG *** 02/07/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*** END   CHG *** 02/07/00 **************************************
     C                   MOVEL     SRDTO         EAWCPD
     C*** END   ADD *** 12/17/99 *************************************
     C*** Begin Add *** 05/27/15 ******************************************
     C                   Eval      ##co# = %EditC(EAco#:'X')
     C                   Eval      ##sub# = %EditC(EAsub#:'X')
     C                   Eval      ##fnd = %EditC(EAfnd:'X')
     C*
     C                   Call      'FA700R'
     C                   Parm      ' '           ##rtcd
     C                   Parm                    ##co#
     C                   Parm      ' '           ##wcf#
     C                   Parm                    ##sub#
     C                   Parm                    ##fnd
     C*
     C                   Move      ##wcf#        kkWcf#
     C                   Z-Add     eaRcn         kkEobs
     C*
     C     keyWARPLN     Chain     prp0n
     C                   If        %Found(WARPLN)
     C                   Eval      EAeftC = RPeftC
     C                   EndIf
     C*** End   Add *** 05/27/15 ******************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 02/02/00 **************************************
     C**********           MOVELQ1USER    EAUSER
     C*** END   DEL *** 02/02/00 **************************************
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*** BEG ADD *** 05/10/99 ***************************************
     C                   UPDATE    PEA01
     C*** END ADD *** 05/10/99 ***************************************
     C*
     C                   ENDIF
     C*
     C*** BEG DEL *** 05/10/99 ***************************************
     C**********           UPDATPEA01
     C*** END DEL *** 05/10/99 ***************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* BUPDAT - UPDATE FAEAP FOR BRS CHECK.
     C*================================================================
     C     BUPDAT        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAWCBC
     C*** BEGIN ADD *** 12/17/99 *************************************
     C**********           MOVE @@CHDT    EAWCBD
     C                   Z-ADD     @@CHDT        SRDTI
     C*** BEGIN CHG *** 02/07/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*** END   CHG *** 02/07/00 **************************************
     C                   MOVEL     SRDTO         EAWCBD
     C*** END   ADD *** 12/17/99 *************************************
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 02/02/00 **************************************
     C**********           MOVELQ1USER    EAUSER
     C*** END   DEL *** 02/02/00 **************************************
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* NUPDAT - UPDATE FAEAP FOR NWS CHECK.
     C*================================================================
     C     NUPDAT        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAWCNC
     C*** BEGIN ADD *** 12/17/99 *************************************
     C**********           MOVE @@CHDT    EAWCND
     C                   Z-ADD     @@CHDT        SRDTI
     C*** BEGIN CHG *** 02/07/00 **************************************
     C**********           CALL 'SRCVTDAT'DTMDYY
     C                   CALL      'SRCVTDAT'    DTYYMD
     C*** END   CHG *** 02/07/00 **************************************
     C                   MOVEL     SRDTO         EAWCND
     C*** END   ADD *** 12/17/99 *************************************
     C*******              MOVE @@CHAT    EANAMT
     C*
     C                   TIME                    DSDTTM
     C*** BEGIN DEL *** 02/02/00 **************************************
     C**********           MOVELQ1USER    EAUSER
     C*** END   DEL *** 02/02/00 **************************************
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*** BEGIN ADD *** 05/29/03 **************************************
     C*================================================================
     C* ONEDAT - UPDATE FAEAP FOR GROUP TYPE 1 CHECKS
     C*================================================================
     C     ONEDAT        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAOC#1
     C                   Z-ADD     @@CHDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAOCD1
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* TWODAT - UPDATE FAEAP FOR GROUP TYPE 2 CHECKS
     C*================================================================
     C     TWODAT        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAOC#2
     C                   Z-ADD     @@CHDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAOCD2
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* THRDAT - UPDATE FAEAP FOR GROUP TYPE 3 CHECKS
     C*================================================================
     C     THRDAT        BEGSR
     C*
     C     EAKY          CHAIN     PEA01                              99
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVE      @@CH#         EAOC#3
     C                   Z-ADD     @@CHDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EAOCD3
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     Q1PGM         EAPGM
     C                   MOVEL     DSTIME        EATIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EADT
     C*
     C                   UPDATE    PEA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*** END   ADD *** 05/29/03 **************************************
