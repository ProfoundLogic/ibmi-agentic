     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W7031R - CREATE WORK FILE FOR CHECK REGISTER
     H*****************************************************************
     H* REVISIONS:                                                    *
     H* 08/27/15 C:180 SUPPORT PHW36059                               *
     H*                Fix blank payee error.                         *
     H*                                                               *
     H* 02/10/10 C:246  PROJECT: C133WIJ                              *
     H*            POPULATE  JURISDICTION STATE IN WXCMP  FILE.       *
     H*                                                               *
     H* 12/15/06 C:132 Project: 139WGE                                *
     H*                Load MCPODT with PARQDT if PARQDT not zero.    *
     H*                                                               *
     H*                                                               *
     H* 12/06/04 C:53 SUPPORT: WC072 44910                            *
     H*               ADDED CHECK FOR DBA TO WRITE THE CORRECT NAME.  *
     H*                                                               *
     H* 05/06/92 RJV PROJECT WC142                                    *
     H*              USE WDPAL2 FOR LOGICAL ON PAYMENT FILE, SETLL    *
     H*              WITH THE BEGINNING OF THE MONTH, READ WDPAP      *
     H*              UNTIL THE END OF THE MONTH.                      *
     H*****************************************************************
     FWMPDL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     FWDPAL2    IF   E           K DISK
     FWXWCL     IF   E           K DISK
     FWXMCL     IF A E           K DISK
      *
      **** Begin Add 08/27/15 ****************************************************
     FWDRPL3    IF   E           K DISK    RENAME(PRP01:PRP02)
     FWDPAL4    IF   E           K DISK    RENAME(PPA01:DBRCDF4) PREFIX(P4:2)
      **** End   Add 08/27/15 ****************************************************

     D                 DS
     D  XXDATE                 1      8  0
     D  XXMO                   1      2  0
     D  XXDAY                  3      4  0

     D GetPayee        PR            30A
     D  @Co                                Like(PaCo#)  Const
     D  @Fnd                               Like(Pafnd)  Const
     D  @Fyr                               Like(Pafyr)  Const
     D  @Case                              Like(Pacase) Const
     D  @Chk#                              Like(Pachk#) Const
     D  @Seq#                              Like(Paseq#) Const
     D  @Basq                              Like(Pabasq) Const
     C*
     C*  FUND MASTER FILE
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C*
     C*  PAYMENT DETAIL FILE
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#             3 0
     C                   KFLD                    XXFRDT
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*  WASTE CHECKS FILE
     C*
     C     WCKEY         KLIST
     C                   KFLD                    XXCO#             3 0
     C*
     C*  PAYEE DEMOGRAPHICS FILE
     C*
     C     PDKEY         KLIST
     C                   KFLD                    PAFEIN
     C                   KFLD                    PAMOD
     C*
     C*  CLAIMANT DEMOGRAPHICS
     C*
     C     CDKEY         KLIST
     C                   KFLD                    CMSS#
     C*
     C*  CLAIM MASTER
     C*
     C     CMKEY         KLIST
     C                   KFLD                    PACO#
     C                   KFLD                    PAFND
     C                   KFLD                    PAFYR
     C                   KFLD                    PACASE
     C*
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C*
     C*   ENTRY PARAMETERS
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
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
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVEL     QQDATE        XXDATE
     C*
     C                   Z-ADD     1             XXDAY
     C*
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVEL     XXFRDT        XXYM              5 0
     C*
     C                   READ      PMC01                                  60
     C*
     C*  READ THE PAYMENT RECORDS
     C*
     C     PAKY          SETLL     PPA01
     C     *IN51         DOUEQ     '1'
     C     PAKY1         READE     PPA01                                  51
     C     *IN51         IFEQ      '0'
     C*
     C                   MOVEL     PATRDT        XXYM1             5 0
     C     XXYM1         IFEQ      XXYM
     C*
     C*  CHECK FOR PREV.MONTH
     C*
     C                   MOVEL     PAPODT        XXYM1
     C     XXYM1         IFLT      XXYM
     C                   Z-ADD     1             MCMCSQ
     C                   ELSE
     C*
     C     PATYPE        IFEQ      'M'
     C                   Z-ADD     18            MCMCSQ
     C                   ELSE
     C     PATYPE        IFEQ      'R'
     C                   Z-ADD     21            MCMCSQ
     C                   ELSE
     C     PATYPE        IFEQ      'A'
     C                   Z-ADD     31            MCMCSQ
     C                   ELSE
     C                   Z-ADD     11            MCMCSQ
     C                   END
     C                   END
     C                   END
     C                   END
     C*
     C*  RETRIEVE PAYEE NAME
     C*
     C     CMKEY         CHAIN     WMCML1                             60
     C   60              MOVE      *BLANK        CMSS#
     C*
     C******BEG ADD  02/10/10******************************
     C                   MOVE      CMJRST        MCJRST
     C******END ADD  02/10/10******************************
     C                   MOVE      PAFEIN        XXSS#             9
     C*
     C     PAFEIN        IFNE      0
     C     XXSS#         ANDNE     CMSS#
     C*
     C     PDKEY         CHAIN     WMPDL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     PDNAME        XXLAST           56
     C                   MOVEL     PDFSN         XXFRST           15
     C*** BEG ADD *** 12/06/04 **************************************
     C     PDDBA1        IFEQ      'Y'
     C                   MOVEL(P)  PDADR1        XXLAST
     C                   ELSE
     C*** END ADD *** 12/06/04 **************************************
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL     PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   END
     C*** BEG ADD *** 12/06/04 **************************************
     C                   ENDIF
     C*** END ADD *** 12/06/04 **************************************
     C                   MOVEL     XXLAST        MCDESC
     C                   ELSE
     C                   MOVE      *BLANK        MCDESC
     C                   END
     C                   ELSE
     C*
     C*  RETRIEVE CLAIMANT NAME
     C*
     C     CDKEY         CHAIN     WMCDL                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      *BLANKS       XXLAST           56
     C                   MOVE      *BLANKS       XXFRST           15
     C                   MOVEL     CDLSN         XXLAST           56
     C                   MOVEL     CDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRFRLS'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        MCDESC
     C*
     C                   ELSE
     C                   MOVE      *BLANK        MCDESC
     C                   END
     C                   END
     C*
      *** Begin Add *** 08/27/15 *****************************************
      /Free
       If MCDESC = *blanks;
          MCDESC = GetPayee(Paco#:Pafnd:Pafyr:Pacase:Pachk#:Paseq#:Pabasq);
       Endif;
      /End-Free
      *** End   Add *** 08/27/15 *****************************************
     C                   MOVE      PACO#         MCCO#
     C                   MOVE      PAFND         MCFND
     C                   MOVE      PAFYR         MCFYR
     C                   MOVE      PACASE        MCCASE
     C                   MOVE      PASEQ#        MCSEQ#
     C                   MOVE      PACHK#        MCCHK#
     C                   MOVE      PATRDT        MCTRDT
      ***** BEG CHG ***** 12/15/06 ***************************************
     C     PARQDT        IFNE      *ZEROS
     C                   MOVE      PARQDT        MCPODT
     C                   ELSE
     C                   MOVE      PAPODT        MCPODT
     C                   ENDIF
     C**********           MOVE PAPODT    MCPODT
      ***** END CHG ***** 12/15/06 ***************************************
     C                   MOVE      PAFEIN        MCFEIN
     C                   MOVE      PAMOD         MCMOD
     C                   MOVE      PAAMT         MCAMT
     C                   MOVE      PABR2         MCBR2
     C                   MOVE      PABR7         MCBR7
     C                   MOVE      PAFRDT        MCFRDT
     C                   MOVE      PATODT        MCTODT
     C                   MOVE      PAWKS         MCWKS
     C                   MOVE      PADAYS        MCDAYS
     C                   MOVE      PATRCD        MCTRCD
     C                   MOVE      PATYPE        MCTYPE
     C                   MOVE      PARECD        MCRECD
     C                   MOVE      PARETP        MCRETP
     C                   MOVE      PAADST        MCADST
     C                   MOVE      PASTAT        MCSTAT
     C                   MOVE      PAUSER        MCUSER
     C                   MOVE      PAPGM         MCPGM
     C                   MOVE      PATIME        MCTIME
     C                   MOVE      PADT          MCDT
     C                   MOVE      PABUL1        MCBUL1
     C*
     C                   WRITE     PMC01
     C                   ELSE
     C                   MOVE      '1'           *IN51
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  READ THE WASTE CHECK TRANSACTIONS
     C*
     C     WCKEY         SETLL     PWC01
     C                   MOVE      '0'           *IN51
     C     *IN51         DOUEQ     '1'
     C     WCKEY         READE     PWC01                                  51
     C     *IN51         IFEQ      '0'
     C*
     C                   MOVEL     WCTRDT        XXYM1
     C     XXYM1         IFEQ      XXYM
     C*
     C                   Z-ADD     11            MCMCSQ
     C                   MOVE      WCCO#         MCCO#
     C                   MOVE      WCFND         MCFND
     C                   MOVE      *BLANKS       MCFYR
     C                   MOVE      *BLANKS       MCCASE
     C                   MOVE      *BLANKS       MCSEQ#
     C                   MOVE      WCCHK#        MCCHK#
     C                   MOVE      WCTRDT        MCTRDT
     C                   MOVE      *BLANKS       MCPODT
     C                   MOVE      *BLANKS       MCFEIN
     C                   MOVE      *BLANKS       MCMOD
     C                   MOVE      *BLANKS       MCAMT
     C                   MOVE      *BLANKS       MCBR2
     C                   MOVE      *BLANKS       MCBR7
     C                   MOVE      *BLANKS       MCFRDT
     C                   MOVE      *BLANKS       MCTODT
     C                   MOVE      *BLANKS       MCWKS
     C                   MOVE      *BLANKS       MCDAYS
     C                   MOVE      *BLANKS       MCTRCD
     C                   MOVE      'W'           MCTYPE
     C                   MOVE      *BLANKS       MCRECD
     C                   MOVE      *BLANKS       MCRETP
     C                   MOVE      WCDESC        MCDESC
     C                   MOVE      *BLANKS       MCADST
     C                   MOVE      *BLANKS       MCSTAT
     C                   MOVE      WCUSER        MCUSER
     C                   MOVE      WCPGM         MCPGM
     C                   MOVE      WCTIME        MCTIME
     C                   MOVE      WCDT          MCDT
     C                   MOVE      WCBUL1        MCBUL1
     C*
     C                   WRITE     PMC01
     C                   END
     C*
     C                   END
     C                   END
     C*
     C                   SETON                                        LR
     C*

     p*===========================================================================
     p* Get Payee name
     p*===========================================================================
     p GetPayee        b
     D GetPayee        PI            30A
     D  @Co                                Like(PaCo#)  Const
     D  @Fnd                               Like(Pafnd)  Const
     D  @Fyr                               Like(Pafyr)  Const
     D  @Case                              Like(Pacase) Const
     D  @Chk#                              Like(Pachk#) Const
     D  @Seq#                              Like(Paseq#) Const
     D  @Basq                              Like(Pabasq) Const

     D  xxbseq#        S                   like(Pabasq) inz

      /Free
       Chain (@Co:@Fnd:@Fyr:@Case:@Seq#) PRP02;

       If (%found(WDRPL3) and RPCHK# <> @Chk#) or not %found(WDRPL3);
           If @Basq <> *zero;
              Chain (@Co:@Fnd:@Fyr:@Case:@Basq) PRP02;
           Endif;
       Endif;

       If (%found(WDRPL3) and RPCHK# <> @Chk#) or  not %found(WDRPL3);
          If @Basq <> *zero and @Chk# <> *zero;
              xxbseq# = @Basq;
             Chain (@Co:@Fnd:@Chk#) DBRCDF4;
             If %found(WDPAL4);
                Dow not %EOF(WDPAL4);
                    If P4SEQ# =  xxbseq#;
                       Chain (@Co:@Fnd:@Fyr:@Case:P4BASQ) PRP02;
                       If %found(WDRPL3) and RPCHK# = @Chk#;
                          Leave;
                       Endif;
                       If P4basq = *zeros;
                          Leave;
                       Endif;
                       xxbseq# = P4basq;
                    Endif;
                 Reade (@Co:@Fnd:@chk#) DBRCDF4;
                Enddo;
             Endif;
          Endif;
       Endif;

       If %found(WDRPL3) and RPCHK# = @Chk#;
          return RPPNAM;
       Else;
          return *blanks;
       Endif;
      /End-Free
     p GetPayee        e
