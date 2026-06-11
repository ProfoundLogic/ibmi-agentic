     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE SRPRCC - PAYMENT RELEASED FOR CLOSED CLAIM
     H*****************************************************************
     H*  PROGRAM DESCRIPTION:                                         *
     H*       THIS PROGRAM UPDATES THE RESERVES FOR A CLOSED CLAIM    *
     H*       WHEN A PAYMENT IS RELEASED, PENDED, DELETED, ADJUSTED,  *
     H*       VOIDED, STOPPED, REVERSED, OR REFUNDED.                 *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0080R - REQUEST PAYMENT                                   *
     H*    W0120R - MANUAL PAYMENTS/PAYMENT CORRECTIONS               *
     H*    W0583R2- CHANGE PAYMENT REQUEST                            *
     H*    W0581R - PAYMENT INQUIRY                                   *
     H*    W0581R1- PENDED PAYMENT SEARCH                             *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  PROGRAMMER: C:46                                             *
     H*  CREATED:    01/09/96    PROJECT: WC411                       *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date                                    *
     H*                                                               *
     H* 04/25/06 C:33  Seq#: PQ135/52403                              *
     H*                If called from batch, use the batch date as    *
     H*                 opposed to the system date.                   *
     H*                                                               *
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to default the reserve buckets to a *
     H*                specific order if PDSQuest is installed.       *
     H*                                                               *
     H* 12/12/03 C:62 Support: 40348                                  *
     H*               Don't update WDELP if check request             *
     H*                                                               *
     H* 08/12/03 C:15 Support: WC129/                                 *
     H*               Added code to prevent duplicate maint date and  *
     H*               time.                                           *
     H*                                                               *
     H* 04/22/99 C:15 Support: WC051/16502                            *
     H*               Fixed pgm to load the amount when QQTYPE = '1'  *
     H*               or 'P'.                                         *
     H*                                                               *
     H* 11/17/98 C:15 Project: WC577                                  *
     H*               Added a description for payments which are      *
     H*               pended and deleted.                             *
     H*****************************************************************
     H* INDICATOR USAGE:                                              *
     H*                                                               *
     H*    60 - UTILITY INDICATOR                                     *
     H*    98 - RESERVE CODE FOUND IN CLAIM                           *
     H*    99 - ADD RESERVE CODE TO CLAIM                             *
     H*                                                               *
     H*****************************************************************
     FWMCML1    UF   E           K DISK
     FWDELL     UF   E           K DISK
     FWMRHP     O    E             DISK
     F*** Begin add *** 08/12/03 **************************************
     FWMRHL     IF   E           K DISK
     F                                     RENAME(PRH01:PRH02)
     F*** End add ***** 08/12/03 **************************************
     F***  Begin Add  ***  04/25/06  **************************************
     FWZDBP     IF   E             DISK
     F                                     RENAME(PDB01:PDB0P)
     FWZD3P     IF   E             DISK
     F                                     RENAME(PD301:PD30P)
     F***  End   Add  ***  04/25/06  **************************************
      *----------------------------------------------------------------

     D*** Begin add *** 03/23/04 **************************************
     D*** End add ***** 03/23/04 **************************************
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)                              RESERVE TYPE
     D  CMRT01                 1      2
     D  CMRT02                 3      4
     D  CMRT03                 5      6
     D  CMRT04                 7      8
     D  CMRT05                 9     10
     D  CMRT06                11     12
     D  CMRT07                13     14
     D  CMRT08                15     16
     D  CMRT09                17     18
     D  CMRT10                19     20
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)                              RESERVE AMOUNT
     D  CMRR01                 1      9  2
     D  CMRR02                10     18  2
     D  CMRR03                19     27  2
     D  CMRR04                28     36  2
     D  CMRR05                37     45  2
     D  CMRR06                46     54  2
     D  CMRR07                55     63  2
     D  CMRR08                64     72  2
     D  CMRR09                73     81  2
     D  CMRR10                82     90  2
     D*
     D*** Begin add *** 03/23/04 **************************************
     D                 DS                  INZ
     D  RRT                    1     20
     D                                     DIM(10)                              RH rsv types
     D  RHRT01                 1      2
     D  RHRT02                 3      4
     D  RHRT03                 5      6
     D  RHRT04                 7      8
     D  RHRT05                 9     10
     D  RHRT06                11     12
     D  RHRT07                13     14
     D  RHRT08                15     16
     D  RHRT09                17     18
     D  RHRT10                19     20
     D                 DS                  INZ
     D  RSU                    1     90  2
     D                                     DIM(10)                              RH rsv amt
     D  RHSU01                 1      9  2
     D  RHSU02                10     18  2
     D  RHSU03                19     27  2
     D  RHSU04                28     36  2
     D  RHSU05                37     45  2
     D  RHSU06                46     54  2
     D  RHSU07                55     63  2
     D  RHSU08                64     72  2
     D  RHSU09                73     81  2
     D  RHSU10                82     90  2
     D*** End add ***** 03/23/04 **************************************
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D XXEXPL          C                   CONST('INCREASE RESERVE FOR-
     D                                      PAYMENT TO CLOSED C-
     D                                     LAIM')
     D XXEXPV          C                   CONST('DECREASE RESERVE FOR-
     D                                      VOID TO CLOSED C-
     D                                     LAIM')
     D XXEXPS          C                   CONST('DECREASE RESERVE FOR-
     D                                      STOP TO CLOSED C-
     D                                     LAIM')
     D XXEXPR          C                   CONST('DECREASE RESERVE FOR-
     D                                      REFUND TO CLOSED C-
     D                                     LAIM')
     D XXEXPZ          C                   CONST('INCREASE RESERVE FOR-
     D                                      REVERSAL TO CLOSED -
     D                                     CLAIM')
     D XXEXPC          C                   CONST('INCREASE RESERVE FOR-
     D                                      ADJUSTMENT TO CLOS-
     D                                     ED CLAIM')
     D XXEXPO          C                   CONST('DECREASE RESERVE FOR-
     D                                      ADJUSTMENT TO CLOS-
     D                                     ED CLAIM')
     D*** BEGIN ADD *** 11/17/98 *************************************
     D XXEXPD          C                   CONST('DECREASE RESERVE FOR-
     D                                      DELETED PAYMENT')
     D XXEXPP          C                   CONST('DECREASE RESERVE FOR-
     D                                      PENDED PAYMENT')

     I*** END ADD ***** 11/17/98 *************************************

     C****************************************************************
     C*   PROGRAM FILE KEY LISTINGS
     C****************************************************************
     C*
     C*   KEY LIST FOR THE CLAIM MASTER FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*   KEY LIST FOR THE EMPLOYER DETAIL FILE
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    XXFYR
     C*
     C*** Begin add *** 08/12/03 **************************************
     C*   Key list for the Reserve History File
     C*
     C     RHKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMCASE
     C                   KFLD                    XXEFDT
     C*** End add ***** 08/12/03 **************************************
     C****************************************************************
     C*   PROGRAM *LIKE DEFINES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMCO#         QQCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFND         QQFND
     C     *LIKE         DEFINE    RHRT01        XXRVCD
     C     *LIKE         DEFINE    RHRT01        QQRVCD
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMFYR         QQFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMCASE        QQCASE
     C     *LIKE         DEFINE    RHSU01        XXAMT
     C     *LIKE         DEFINE    RHSU01        QQAMT
     C*** Begin add *** 08/12/03 **************************************
     C     *LIKE         DEFINE    RHEFDT        XXEFDT
     C*** End add ***** 08/12/03 **************************************
     C***  Begin Add  ***  04/25/06  **************************************
     C     *LIKE         DEFINE    *IN60         BTCH
     C     *LIKE         DEFINE    *IN60         GRBTCH
     C***  End   Add  ***  04/25/06  **************************************
     C*
     C****************************************************************
     C*   PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRVCD
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQAMT
     C*** BEGIN ADD *** 12/12/03 **************************************
     C                   PARM                    QQPTYP            1
     C*** END   ADD *** 12/12/03 **************************************
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
     C***  Begin Add  ***  04/25/06  **************************************
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W6000C'      ##PGM            10
     C     BTCH          PARM                    ##RTCD            1
     C*
     C                   CALL      'SRCHKPGM'
     C                   PARM      'W6000C1'     ##PGM            10
     C     GRBTCH        PARM                    ##RTCD            1
     C***  End   Add  ***  04/25/06  **************************************
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQRVCD        XXRVCD
     C*
     C*  IF THIS IS A RELEASED (OR INCREASED) PAYMENT, INCREASE THE
     C*  RESERVES.
     C*
     C     QQTYPE        IFEQ      'R'
     C                   Z-ADD     QQAMT         XXAMT
     C                   END
     C*
     C*  IF THIS IS A HELD (OR DECREASED) PAYMENT, DECREASE THE
     C*  RESERVES.
     C     QQTYPE        IFEQ      'D'
     C                   Z-SUB     QQAMT         XXAMT
     C                   END
     C*
     C*  IF THIS IS A VOIDED, STOPPED, REFUNDED PAYMENT, DECREASE
     C*  THE RESERVES.
     C     QQTYPE        IFEQ      'V'
     C     QQTYPE        OREQ      'S'
     C     QQTYPE        OREQ      'F'
     C                   Z-SUB     QQAMT         XXAMT
     C                   END
     C*
     C*  IF THIS IS A REVERSAL, INCREASE THE RESERVES.
     C     QQTYPE        IFEQ      'Z'
     C                   Z-ADD     QQAMT         XXAMT
     C                   END
     C*
     C*  IF THIS IS A OFFSETTING ADJUSTMENT, DECREASE THE RESERVES.
     C     QQTYPE        IFEQ      'O'
     C*** BEGIN CHG *** 04/22/99 *************************************
     C******************** Z-SUBQQAMT     XXAMT
     C                   Z-ADD     QQAMT         XXAMT
     C*** END CHG ***** 04/22/99 *************************************
     C                   END
     C*
     C*  IF THIS IS A CORRECTING ADJUSTMENT, INCREASE THE RESERVES.
     C     QQTYPE        IFEQ      'C'
     C                   Z-ADD     QQAMT         XXAMT
     C                   END
     C*
     C*** BEGIN ADD *** 04/22/99 *************************************
     C*  If a request pmt was deleted, decrease the reserves.
     C*
     C     QQTYPE        IFEQ      '1'
     C                   Z-SUB     QQAMT         XXAMT
     C                   ENDIF                                                  DELETED PMT
     C*
     C*  If a request pmt was pended, decrease the reserves.
     C*
     C     QQTYPE        IFEQ      'P'
     C                   Z-SUB     QQAMT         XXAMT
     C                   ENDIF                                                  PENDED PMT
     C*** END ADD ***** 04/22/99 *************************************
     C*
     C*  UPDATE THE CLAIM MASTER
     C*
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      '0'
     C                   Z-ADD     1             X                 3 0
     C     XXRVCD        LOOKUP    RT(X)                                  98
     C*
     C     *IN98         IFEQ      '1'
     C                   ADD       XXAMT         RR(X)
     C*
     C                   ELSE
     C                   Z-ADD     1             X
     C     *BLANKS       LOOKUP    RT(X)                                  99
     C*
     C     *IN99         IFEQ      '1'
     C                   MOVE      XXRVCD        RT(X)
     C                   Z-ADD     XXAMT         RR(X)
     C                   END                                                    *IN99
     C*
     C                   END                                                    *IN98
     C                   UPDATE    PCM01
     C*
     C*   UPDATE THE OUTSTANDING RESERVES IN THE EMPLOYER DETAIL
     C*
     C*** BEGIN ADD *** 12/12/03 **************************************
     C     Q1PRMS        IFGT      7
     C     QQPTYP        IFEQ      'P'
     C*** END   ADD *** 12/12/03 **************************************
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       XXAMT         ELOUTS
     C                   UPDATE    PEL01
     C                   ENDIF
     C*** BEGIN ADD *** 12/12/03 **************************************
     C                   ENDIF
     C                   ELSE
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      *OFF
     C                   ADD       XXAMT         ELOUTS
     C                   UPDATE    PEL01
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 12/12/03 **************************************
     C*
     C*** Begin add *** 08/12/03 **************************************
     C                   TIME                    DSDTTM
     C*
     C***  Begin Add  ***  04/25/06  **************************************
     C                   SELECT
     C     BTCH          WHENEQ    *ON
     C                   CLEAR                   DBSDT
     C     1             SETLL     PDB0P
     C                   READ      PDB0P                                  60
     C                   Z-ADD     DBSDT         SRDTI
     C     GRBTCH        WHENEQ    *ON
     C                   CLEAR                   D3SDT
     C     1             SETLL     PD30P
     C                   READ      PD30P                                  60
     C                   Z-ADD     D3SDT         SRDTI
     C                   OTHER
     C***  End   Add  ***  04/25/06  **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C***  Begin Add  ***  04/25/06  **************************************
     C                   ENDSL
     C*
     C     SRDTI         IFEQ      *ZERO
     C                   Z-ADD     DSDATE        SRDTI
     C                   ENDIF
     C***  End   Add  ***  04/25/06  **************************************
     C*
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     RHKY          CHAIN     PRH02                              99
     C     *IN99         DOWEQ     *OFF
     C*
     C     RHTIME        IFEQ      DSTIME
     C                   ADD       1             DSTIME
     C                   ENDIF
     C*
     C     RHKY          READE     PRH02                                  99
     C                   ENDDO
     C*** End add ***** 08/12/03 **************************************
     C*
     C*   A RESERVE HISTORY RECORD WILL BE WRITTEN IN THE AMOUNT OF THE
     C*   PAYMENT TO KEEP CLAIM MASTER (WMCMP) AND RESERVE HIST (WMRHP)
     C*   IN BALANCE.
     C*
     C                   MOVE      CMCO#         RHCO#
     C                   MOVE      CMFND         RHFND
     C                   MOVE      CMFYR         RHFYR
     C                   MOVE      CMCASE        RHCASE
     C*** Begin add *** 03/23/04 **************************************
     C                   CLEAR                   RRT
     C                   CLEAR                   RSU
     C*
     C                   MOVE      XXRVCD        RRT(X)
     C                   Z-ADD     XXAMT         RSU(X)
     C*** End add ***** 03/23/04 **************************************
     C*** Begin dlt *** 03/23/04 **************************************
     C**********           MOVE XXRVCD    RHRT01
     C**********           MOVE *BLANKS   RHRT02
     C**********           MOVE *BLANKS   RHRT03
     C**********           MOVE *BLANKS   RHRT04
     C**********           MOVE *BLANKS   RHRT05
     C**********           MOVE *BLANKS   RHRT06
     C**********           MOVE *BLANKS   RHRT07
     C**********           MOVE *BLANKS   RHRT08
     C**********           MOVE *BLANKS   RHRT09
     C**********           MOVE *BLANKS   RHRT10
     C**********           MOVE XXAMT     RHSU01
     C**********           MOVE *ZEROS    RHSU02
     C**********           MOVE *ZEROS    RHSU03
     C**********           MOVE *ZEROS    RHSU04
     C**********           MOVE *ZEROS    RHSU05
     C**********           MOVE *ZEROS    RHSU06
     C**********           MOVE *ZEROS    RHSU07
     C**********           MOVE *ZEROS    RHSU08
     C**********           MOVE *ZEROS    RHSU09
     C**********           MOVE *ZEROS    RHSU10
     C*** End dlt ***** 03/23/04 **************************************
     C     QQTYPE        IFEQ      'R'
     C                   MOVEL     XXEXPL        RHEXPL
     C                   ENDIF                                                  PAYMENT
     C     QQTYPE        IFEQ      'D'
     C                   MOVEL     'DECREASE'    RHEXPL
     C                   ENDIF                                                  DECREASED PYMT
     C     QQTYPE        IFEQ      'V'
     C                   MOVEL     XXEXPV        RHEXPL
     C                   ENDIF                                                  VOID
     C     QQTYPE        IFEQ      'S'
     C                   MOVEL     XXEXPS        RHEXPL
     C                   ENDIF                                                  STOP
     C     QQTYPE        IFEQ      'F'
     C                   MOVEL     XXEXPR        RHEXPL
     C                   ENDIF                                                  REFUND
     C     QQTYPE        IFEQ      'Z'
     C                   MOVEL     XXEXPZ        RHEXPL
     C                   ENDIF                                                  REVERSAL
     C     QQTYPE        IFEQ      'O'
     C                   MOVEL     XXEXPO        RHEXPL
     C                   ENDIF                                                  OFFSET ADJ
     C     QQTYPE        IFEQ      'C'
     C                   MOVEL     XXEXPC        RHEXPL
     C                   ENDIF                                                  CORRECT ADJ
     C*** BEGIN ADD *** 11/17/98 *************************************
     C     QQTYPE        IFEQ      '1'
     C                   MOVEL     XXEXPD        RHEXPL
     C                   ENDIF                                                  DELETED PMT
     C     QQTYPE        IFEQ      'P'
     C                   MOVEL     XXEXPP        RHEXPL
     C                   ENDIF                                                  PENDED PMT
     C*** END ADD ***** 11/17/98 *************************************
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      *BLANKS       RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C*** Begin dlt *** 08/12/03 **************************************
     C**********           TIME           DSDTTM
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     RHDT
     C**********
     C**********           Z-ADDDSDATE    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     RHEFDT
     C*** End dlt ***** 08/12/03 **************************************
     C*** Begin add *** 08/12/03 **************************************
     C*** BEG DEL ***** 03/03/10 **************************************
     C****                 MOVE XXEFDT    RHDT
     C*** END DEL ***** 03/03/10 **************************************
     C                   MOVE      XXEFDT        RHEFDT
     C*** End add ***** 08/12/03 **************************************
     C                   Z-ADD     DSTIME        RHTIME
     C*** BEG ADD ***** 03/03/10 **************************************
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
      *
     C                   MOVEL     SRDTO         RHIDT
      *
     C                   MOVE      Q1USER        RHIUSR
     C                   MOVE      Q1PGM         RHIPGM
      *
     C                   Z-ADD     DSTIME        RHITIM
     C*** END ADD ***** 03/03/10 **************************************
      //***  Begin Add  ***  08/15/17  ************************************
      /Free
       RHinitRsv = GetInitRsvFlag(RHco#:RHfnd:RHfyr:RHcase);
      /End-free
      //***  End   Add  ***  08/15/17  ************************************
     C                   WRITE     PRH01
     C                   END                                                    *IN60
     C*
     C                   MOVE      *ON           *INLR
