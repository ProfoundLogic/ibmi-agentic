     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* StoneRiver is a trademark of StoneRiver, Inc.
     H*****************************************************************
     H/TITLE W0071R6 - DARTS DIARY REMARKS API
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H*   10/17/11 C:279 Project: SY195                               *
     H*                  - Modify program to pass/return UNC path     *
     H*                    field and IFS flag to called/calling       *
     H*                    programs.                                  *
     H*                                                               *
     H* 03/24/10 C:232 Project: 139WNN                                *
     H*                Update programs to use the new EDI Control API *
     H*                WA076R instead of using WMIXP.                 *
     H*                                                               *
     H* 12/04/06 C:53 Support: 55193
     H*               Correct date issue
     H*
     H*  04/25/00 C:31  Project: DR097
     H*                 Chg ALL PDS pgms
     H*                 1. Replace write to SDCMP with API DA700R
     H*
     H* 01/19/00 C:15 Support: WC087/21429                            *
     H*               Changed code which retrieves the next available *
     H*               DARTS seq#.                                     *
     H*                                                               *
     H* 07/01/99 C:118 SUPPORT: DR095/13079,DR048/13846,DR087/16339   *
     H*                              DR048/17294                      *
     H*                Added FEOD SDCM* to WRITEPCM0* so DARTS updates*
     H*                immediately.                                   *
     H*
     H* 05/23/98 C:58 Project: WC577
     H*          Send 148 ACK error diary to claim adjuster.
     H*
     H* 02/23/98 C:85 Project: WC577                                  *
     H*               CHANGED THE FIELD MOVED TO CMSMCD               *
     H*               FROM *BLANKS TO CTSMCD.                         *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*****************************************************************
     H*
     F*** BEGIN DLT *** 03/24/10 **************************************
     F***WMIXL   IF  E           K        DISK
     F*** END DLT ***** 03/24/10 **************************************
     FSTCTL1    IF   E           K DISK
     F                                     RENAME(PCT01:PCT03)
     F*** BEGIN CHG *** 04/25/00 **************************************
     F*****SDCML   UF  E           K        DISK                      A
     F********    PCM01                             KRENAMEPCM02
     FSDCML     UF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** END CHG *** 04/25/00 ****************************************
     F*** BEGIN DEL *** 04/25/00 **************************************
     F********SDCMLI  IF  E           K        DISK
     F********    PCM01                             KRENAMEPCM0I
     F*** END DEL *** 04/25/00 ****************************************
     FWMUPL1    IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM04)

     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** BEG ADD ***** 10/17/11 ***********************************
     D RPGM            C                   CONST('RTVDOCROOT')
     D RPGM1           C                   CONST('RTVDOCENV')

     IPCT03
     I              CTDESC                      C3DESC
     I*
     IPCM02
     I              CMKEY                       C2KEY
      ***** END ADD ***** 10/17/11 ***********************************

     C*
     C***        WMCXL - Claims DARTS Setup Control File
     C*
     C     CXKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C***        WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTSET
     C*
     C***        WMCML - Claim Master
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     *LIKE         DEFINE    QQFND         XCFND
     C     *LIKE         DEFINE    QQFYR         XCFYR
     C     *LIKE         DEFINE    QQCASE        XCCASE
     C*** BEG DEL ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXCO#     XXCO#
     C*** END DEL ****** 03/24/10 *************************************
     C     *LIKE         DEFINE    C2KEY         XCKEY
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        *LIKE     DEFN IXLRSC    XXLRSC
     C     *LIKE         DEFINE    CMCLEX        XXLRSC
     C*** END CHG ****** 03/24/10 *************************************
     C                   Z-ADD     *ZERO         XXFND             3 0
     C                   Z-ADD     *ZERO         XXFYR             3 0
     C                   Z-ADD     *ZERO         XXCASE            7 0
     C                   MOVEL     *BLANKS       XXTSET            3
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3            COMPANY
     C                   PARM                    QQJRST            2            JURISTDICTION
     C                   PARM                    QQFND             3            FUND ID
     C                   PARM                    QQFYR             3            FUND YR
     C                   PARM                    QQCASE            7            CASE/CLAIM
     C                   PARM                    QQTSET            3            TRANS SET
     C                   PARM                    QQDSQG            5            DARTS SEQ#
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
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
      *
     C*** BEG ADD ****** 03/24/10 *************************************
     C     IXAPI         PLIST
     C                   PARM                    XXCO#             3 0
     C                   PARM                    XXJRST
     C                   PARM                    XXTSET
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
     C*** END ADD ****** 03/24/10 *************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQJRST        XXJRST            2
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQTSET        XXTSET
     C                   MOVE      *BLANKS       Q@STS             2
      *
      ***** BEG ADD ***** 10/17/11 ***********************************
      * GET ROOT
     C                   CALL      RPGM
     C                   PARM                    ROOT             10
      * Find IFS
     C                   CALL      RPGM1
     C                   PARM                    ENV               1
      ***** END ADD ***** 10/17/11 ***********************************
      *** Retrieve the EDI Claim Control Maintenance Record.
      *
     C*** BEG CHG ****** 03/24/10 *************************************
     C***        IXKEY     CHAINPIX01                61
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN61
     C                   ELSE
     C                   MOVE      '1'           *IN61
     C                   ENDIF
     C*** END CHG ****** 03/24/10 *************************************
      *
      ***   Don't write diary entry if code is not found or blank.
      *
     C     *IN61         IFEQ      *ON
     C     IXEADC        OREQ      *BLANKS
     C                   MOVE      *ON           *INLR
     C                   RETURN
     C                   ENDIF
     C*
      *
      *  ADD OR UPDATE EDI REVIEWER DARTS REMARK CODE IXEADC
      *
     C                   EXSR      SRDDRY
      *
     C                   MOVE      *ON           *INLR
     C*
     C/EJECT
      *****************************************************************
      *  SRDDRY - ADD OR UPDATE DIARY RECORDS.
      *****************************************************************
      *
-BSR CSR   SRDDRY        BEGSR
      *
     CSR                 MOVE      *BLANKS       XCKEY
     CSR                 MOVEL     XXCO#         XCKEY
     CSR                 MOVE      XXFND         XCFND
     CSR                 MOVE      XXFYR         XCFYR
     CSR                 MOVE      XXCASE        XCCASE
     CSR                 MOVE      *BLANKS       CMRMCD
     CSR   XCKEY         CAT       XCFND:0       XCKEY
     CSR   XCKEY         CAT       XCFYR:0       XCKEY
     CSR   XCKEY         CAT       XCCASE:0      XCKEY
      *
     CSR   XCKEY         CHAIN     PCM02                              63
      *
      *** Update Diary item if already there.
      *
     CSR   *IN63         DOWEQ     *OFF
     C     IXEADC        IFEQ      CMRMCD
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
      *
      ***        Retrieve the remark table entry
      *
     C     IXEADC        CHAIN     PCT03                              60
     CSR   *IN60         IFEQ      *OFF
     CSR                 MOVEL     C3DESC        CMCOMM
     CSR                 MOVEL     CTDEPG        CMDCID
     CSR   CMDCID        IFNE      *BLANKS
     CSR                 MOVEL     C2KEY         CMFOLD
     CSR                 MOVE      'Y'           CMFL01
     CSR                 ENDIF
      ***** BEG ADD ***** 10/17/11 ***********************************
     CSR                 MOVEL     *BLANKS       CMIFSP
     CSR                 MOVEL     *BLANKS       CMUNCN
      *
     CSR   CMFOLD        IFNE      ' '
     CSR   CMDCID        ORNE      ' '
     CSR                 MOVEL     '/'           CMUNCN
     CSR   CMUNCN        CAT       ROOT:0        CMUNCN
     CSR   CMUNCN        CAT       '/':0         CMUNCN
     CSR   CMUNCN        CAT       CMFOLD:0      CMUNCN
     CSR   CMUNCN        CAT       '/':0         CMUNCN
     CSR   CMUNCN        CAT       CMDCID:0      CMUNCN
     CSR                 MOVEL     ENV           CMIFSP
     CSR                 ELSE
     CSR                 MOVEL     *BLANKS       CMUNCN
     CSR                 ENDIF                                                  CMFOLD IFNE ' '
      ***** END ADD ***** 10/17/11 ***********************************
     CSR   CTDPRG        IFGT      *ZEROS
     CSR                 MOVE      'Y'           CMFL02
     CSR                 ENDIF
     CSR                 MOVE      CTFL01        CMDIFL
     CSR                 MOVE      CTFL02        CMFL03
     CSR                 ENDIF
      *
     CSR                 UPDATE    PCM02
      *
     CSR                 MOVE      CMSEQ#        QQDSQG
      *
     CSR                 ENDIF
     CSR   XCKEY         READE     PCM02                                  63
     CSR                 ENDDO
      *
      *** Write Diary item if not already there.
      *
     CSR   *IN63         IFEQ      *ON
     CSR                 MOVEL     XCKEY         C2KEY
     CSR                 MOVE      *BLANKS       CMCSID
     CSR                 TIME                    CMTIME
     CSR                 MOVE      *BLANKS       CMCOMM
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMCDAT
     CSR                 MOVE      *BLANKS       CMCODE
     CSR                 MOVE      *BLANKS       CMDCID
     CSR                 MOVEL     'W0070R '     CMPNAM
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMTRDT
     CSR                 TIME                    CMTRTM
     CSR                 MOVE      *ZEROS        CMSECL
     CSR                 MOVE      'N'           CMSBYL
     CSR                 MOVE      'A'           CMSTAT
     C*                                                                *********
     C*** BEGIN CHG *** 05/23/98 ***********************************************
     C********** For .......******************************************
     C********** XXTSET    IFEQ '148'
     C**********           MOVELIXLRSC    XXLRSC
     C**********           ELSE
     C*
     C     XXTSET        IFEQ      '148'
     C     XXTSET        OREQ      'A49'
     C*** END CHANGE ** 05/23/98 ***********************************************
     C*      Get adjuster staff code from Claim Master
     C*
     C     CMKY          CHAIN     PCM04                              60
     C                   MOVEL     CMCLEX        XXLRSC
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       CMUSER
     C     XXLRSC        CHAIN     PUP01                              60
     C                   MOVEL     UPUSR         CMUSER
     C*
     CSR                 MOVEL     Q1PGM         CMPGM
     CSR                 Z-ADD     1             CMTIME
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDT
     CSR                 MOVE      Q1USER        CMIUSR
     CSR                 MOVE      *BLANKS       CMFOLD
     CSR                 MOVE      *ZEROS        CMDCDT
     CSR                 MOVE      'Y'           CMDIFL
     CSR                 MOVE      *BLANKS       CMAPID
     CSR                 MOVE      *BLANKS       CMIDOC
     CSR                 MOVE      *BLANKS       CMIQUE
     CSR                 MOVE      *BLANKS       CMIMGF
     CSR                 MOVE      *BLANKS       CMIPTH
     CSR                 MOVE      *ZEROS        CMPIDX
     CSR                 MOVE      *BLANKS       CMQPTY
     CSR                 MOVE      *BLANKS       CMDCTP
     CSR                 MOVEL     Q1PGM         CMIPGM
     C*                                                                *********
     C*** BEGIN DEL *** 02/23/98 ***********************************************
     C*SR*******           MOVE *BLANKS   CMSMCD
     C*** END   DEL *** 02/23/98 ***********************************************
     C*                                                                *********
     CSR                 MOVE      *BLANKS       CMFL01
     CSR                 MOVE      *BLANKS       CMFL02
     CSR                 MOVE      *BLANKS       CMFL03
     CSR                 MOVE      *BLANKS       CMFL04
     CSR                 MOVE      *BLANKS       CMFL05
     C                   MOVEL     IXEADC        CMRMCD
     C*                                                                *********
     C*** BEGIN ADD *** 02/23/98 ***********************************************
     C*                                                                *********
     C     CMRMCD        CHAIN     PCT03                              60
     C*                                                                *********
     C     *IN60         IFEQ      *OFF
     C                   MOVE      CTSMCD        CMSMCD
     C                   ENDIF                                                  *IN60=OFF
     C*                                                                *********
     C*** END   ADD *** 02/23/98 ***********************************************
     C*
     C                   Z-ADD     UDATE         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMIDT
      *
      ***        Retrieve the remark table entry
      *
     C     IXEADC        CHAIN     PCT03                              60
     CSR   *IN60         IFEQ      *OFF
     CSR                 MOVEL     C3DESC        CMCOMM
     CSR                 MOVEL     CTDEPG        CMDCID
     CSR   CMDCID        IFNE      *BLANKS
     CSR                 MOVEL     C2KEY         CMFOLD
     CSR                 MOVE      'Y'           CMFL01
     CSR                 ENDIF
     CSR   CTDPRG        IFGT      *ZEROS
     CSR                 MOVE      'Y'           CMFL02
     CSR                 ENDIF
     CSR                 MOVE      CTFL01        CMDIFL
     CSR                 MOVE      CTFL02        CMFL03
     CSR                 ENDIF
     C*** BEGIN DEL *** 04/25/00 **************************************
      ********
      ********   Get last sequence # used
      ********
     C********   XCKEY     SETGTPCM0I
     C****BEGIN CHG *** 01/19/00 **************************************
     C*******************  READPPCM0I                    50
     C********   XCKEY     REDPEPCM0I                    50
     C****END*CHG ***** 01/19/00 **************************************
      ********
     C********   *IN50     IFEQ *OFF
     C********             ADD  1         CMSEQ#
     C********             ELSE
     C********             Z-ADD1         CMSEQ#
     C********             ENDIF
      ********
     C********             WRITEPCM02
     C******BEGIN ADD **** 07/01/99 ***********************************
     C********             FEOD SDCML
     C******END   ADD **** 07/01/99 ***********************************
      ********
     C********             MOVE CMSEQ#    QQDSQG
     C*** END DEL *** 04/25/00 ****************************************
     C*** BEGIN ADD *** 04/25/00 **************************************
     C*
     C*          Call API to write a DARTS Remark
     C*          ------------------------------------------------------
     C                   MOVEL(P)  C2KEY         Q#KEY            40
     C                   MOVEL(P)  CMCOMM        Q#COMM          256
     C                   MOVE      CMCOMM        Q#COM2           19
     C*** BEG ADD *** 12/04/06 **************************************
     C                   Z-ADD     CMCDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL(P)  SRDTO         Q#CDAT            8
     C*** END ADD *** 12/04/06 **************************************
     C*** BEG DEL *** 12/04/06 **************************************
     C***********          MOVELCMCDAT    Q#CDAT  8 P
     C*** END DEL *** 12/04/06 **************************************
     C                   MOVEL(P)  CMDCID        Q#DCID           12
     C                   MOVEL(P)  CMPNAM        Q#PNAM           10
     C                   MOVE      CMSECL        Q#SECL            3
     C                   MOVEL(P)  CMUSER        Q#USER           10
     C                   MOVEL(P)  CMPGM         Q#PGM            10
     C                   MOVEL(P)  CMFOLD        Q#FOLD           80
     C                   MOVEL(P)  CMDIFL        Q#DIFL            1
     C                   MOVEL(P)  CMIPGM        Q#IPGM           10
     C                   MOVEL(P)  CMFL01        Q#FL01            1
     C                   MOVEL(P)  CMFL02        Q#FL02            1
     C                   MOVEL(P)  CMRMCD        Q#RMCD            5
     C                   MOVE      '00000'       Q#SEQ#            5
     C                   MOVEL(P)  CMSBYL        Q#SBYL            1
     C                   MOVEL(P)  CMAPID        Q#APID            2
     C                   MOVEL(P)  CMIDOC        Q#IDOC           12
     C                   MOVEL(P)  CMIQUE        Q#IQUE           10
     C                   MOVEL(P)  CMIMGF        Q#IMGF            1
     C                   MOVEL(P)  CMIPTH        Q#IPTH           10
     C                   MOVE      CMPIDX        Q#PIDX            5
     C                   MOVEL(P)  CMQPTY        Q#QPTY            1
     C                   MOVEL(P)  CMDCTP        Q#DCTP            5
     C                   MOVEL     CMCSID        Q#CSID            2
     C                   CALL      'DA700R'
     C                   PARM                    Q#KEY
     C                   PARM                    Q#COMM
     C                   PARM                    Q#CDAT
     C                   PARM                    Q#DCID
     C                   PARM                    Q#PNAM
     C                   PARM                    Q#SECL
     C                   PARM                    Q#USER
     C                   PARM                    Q#PGM
     C                   PARM                    Q#FOLD
     C                   PARM                    Q#DIFL
     C                   PARM                    Q#IPGM
     C                   PARM                    Q#FL01
     C                   PARM                    Q#FL02
     C                   PARM                    Q#RMCD
     C                   PARM                    Q#SEQ#
     C                   PARM                    Q#SBYL
     C                   PARM                    Q#COM2
     C                   PARM                    Q#APID
     C                   PARM                    Q#IDOC
     C                   PARM                    Q#IQUE
     C                   PARM                    Q#IMGF
     C                   PARM                    Q#IPTH
     C                   PARM                    Q#PIDX
     C                   PARM                    Q#QPTY
     C                   PARM                    Q#DCTP
     C                   PARM                    Q#CSID
     C                   MOVE      Q#SEQ#        QQDSQG
     C*** END ADD *** 04/25/00 ****************************************
      *
     CSR                 ENDIF
      *
     CSR                 ENDSR
      *
