     Hcopyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H/TITLE W8025R - DEDUCTIBLE RECOVERY REVERSAL
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*   This program allows correction of a deductible recovery     *
     H*   overpayment or billing error. An offsetting entry will be   *
     H*   made in the payment detail file (a negative recovery).  The *
     H*   claim's payment amount and reserves will be updated and if  *
     H*   the claim is closed, a reserve history record will be       *
     H*   created.  The deductible files will be updated to reflect   *
     H*   the change.  The total claim payment amount in the file will*
     H*   be updated                                                  *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   W0576R - Work With Payment Detail                           *
     H*                                                               *
     H* CALLS TO:                                                     *
     H*   NONE                                                        *
     H*                                                               *
     H* PROGRAMMER: C:112                                             *
     H* CREATED:    09/09/99    PROJECT: WC928                        *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*
     H* 08/15/17 C:33  Project: SY394                                 *
     H*                Update initial reserve flag                    *
     H*                                                               *
     H* 12/09/15 C:136 Project: WC2393                                *
     H*                Add mode (@@rtcd = 'Z') to process reversals   *
     H*                on negative check amounts when called from     *
     H*                batch (W8023R). Do not update WDDDP or WADQP   *
     H*                in this mode.                                  *
     H*                                                               *
     H* 08/29/11 C:277 SUPPORT 77320                                  *
     H*                Populate WDPAP initial fields                  *
     H*                                                               *
     H* 03/03/10 C:232 Project:  139WNH                               *
     H*                Update all of the initial maintenance fields.  *
     H*                Populating RHIDT and RHDT with the current     *
     H*                system date.                                   *
     H*                                                               *
     H* 08/10/05 C:15  Support: WC114/48506                           *
     H*                Added code to prevent the remaining reserves   *
     H*                from being updated on closed claims.           *
     H*                                                               *
     H* 03/23/04 C:15  Project:  WC1170                               *
     H*                Added code to load the reserve history buckets *
     H*                according to the order in the claim file.      *
     H*                                                               *
     H* 05/14/02 C:15  Project: WC928                                 *
     H*                When updating the WDDDP amt recovered and amt  *
     H*                pended, only process records with the same rsv *
     H*                type as the recovery reversal record.          *
     H*                                                               *
     H* 01/02/02 C:15  Support: WC065/30371                           *
     H*               -Added code to load the correct seq# in WDPAP.  *
     H*               -Added code to load the chk# of the original    *
     H*                check into WADQP.                              *
     H*****************************************************************
     H* INDICATORS:                                                   *
     H*                                                               *
     H*   60  -  General work indicator                               *
     H*   61  -  General work indicator                               *
     H*****************************************************************
      * Deductible Payments Pending Recovery
     FWADQL     UF A E           K DISK
      * Payment detail file
     FWDPAL     UF A E           K DISK
      * Claim Master file
     FWMCML1    UF   E           K DISK
      * Reserve History file
     FWMRHP     O    E           K DISK
      * Employer Detail file
     FWDELL     UF   E           K DISK
      *  Worker's Comp Deductible Detail file
     FWDDDL     UF   E           K DISK
     F*

     D RC              S              2    DIM(10)
     D*** Begin add *** 03/23/04 **************************************
     D*** End add ***** 03/23/04 **************************************
     D*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D                 DS
     D  RT                     1     20
     D                                     DIM(10)
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
     D  AR                     1     90  2
     D                                     DIM(10)
     D  CMAR01                 1      9  2
     D  CMAR02                10     18  2
     D  CMAR03                19     27  2
     D  CMAR04                28     36  2
     D  CMAR05                37     45  2
     D  CMAR06                46     54  2
     D  CMAR07                55     63  2
     D  CMAR08                64     72  2
     D  CMAR09                73     81  2
     D  CMAR10                82     90  2
     D                 DS
     D  RR                     1     90  2
     D                                     DIM(10)
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
     D*
     D XXDESC          C                   CONST('DEDUCTIBLE RECOVERY -
     D                                     REVERSAL')
      //***  Begin Add  ***  08/15/17  ************************************
      /copy wPrClmRsv
      //***  End   Add  ***  08/15/17  ************************************
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     C**************************************************************************
     C*  Process initialization subroutine.
     C*
     C*
     C*  Entry parm list
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@FYR             3
     C                   PARM                    @@CASE            7
     C                   PARM                    @@SEQ#            5
     C                   PARM                    @@RAMT            9
     C                   PARM                    @@RTCD            1
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
     C*  Key list for the Pending Deductible Recovery file
     C*
     C     DQKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCHK#
     C*
     C*  Key list for the Claim Master file
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  Key list for the Deductible Detail file
     C*
     C     DDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  Key list for the Employer Detail file
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*  Key list for the Payment Detail file
     C*
     C     PAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXSEQ#
     C*
     C     PAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*  key list for the Claim Table files
     C*
     C     TBKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXKEY            10
     C*
     C     *LIKE         DEFINE    CMCO#         XXCO#
     C     *LIKE         DEFINE    CMFND         XXFND
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    CMEMP#        XXEMP#
     C     *LIKE         DEFINE    CMDIV         XXDIV
     C     *LIKE         DEFINE    DQCHK#        XXCHK#
     C     *LIKE         DEFINE    PAAMT         XXRAMT
     C     *LIKE         DEFINE    PAAMT         XXAMT
     C     *LIKE         DEFINE    PASEQ#        XXSEQ#
     C     *LIKE         DEFINE    PASEQ#        XXNXSQ
      *
      *
      *  Load the entry parms into the work fields.
      *
     C                   MOVE      @@CO#         XXCO#
     C                   MOVE      @@FND         XXFND
     C                   MOVE      @@FYR         XXFYR
     C                   MOVE      @@CASE        XXCASE
     C                   MOVE      @@SEQ#        XXSEQ#
     C                   MOVE      @@RAMT        XXRAMT
      *
      *  Update the payment detail file
     C                   EXSR      UPDPAP                                       WDPAP
      *
      *  Update the claim master file
      *
     C                   EXSR      UPDCLM                                       WMCMP
      *
      *  Update the Employer detail file
      *
     C                   EXSR      UPDELP                                       WDELP
      *
      * Update the pending deductible detail file
      *
     C                   EXSR      UPDDD                                        WDDDP
      *
      * Create a deductible post net recovery entry
      *
     C                   EXSR      UPDDQP                                       WADQP
      *
      *  End of Program.
      *
     C                   MOVE      *ON           *INLR
     C**************************************************************************
     C* UPDPAP - This subroutine updates the payment detail file
     C**************************************************************************
     C     UPDPAP        BEGSR
     C*
     C*  Get the next available payment seq#.
     C*
     C     PAKY1         SETGT     PPA01
     C     PAKY1         READPE    PPA01                                  60
     C*** Begin dlt *** 01/02/02 ***************************************
     C** 60 ************** Z-ADD1         XXNXSQ
     C******************** ADD  1         XXNXSQ
     C*** End dlt ***** 01/02/02 ***************************************
     C*** Begin add *** 01/02/02 ***************************************
     C   60              Z-ADD     1             PASEQ#
     C     PASEQ#        ADD       1             XXNXSQ
     C*** End add ***** 01/02/02 ***************************************
     C*
     C*  Retrieve selected payment detail record so a new record can
     C*  be created with the offset payment amount (-PAAMT) and
     C*  additional fileds as necessary
     C*
     C     PAKY          CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
      *
      * set variable PATRDT to system date
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PATRDT
     C*
     C* set PAAMT to a negative value to reverse the entered amount
     C*
     C     XXRAMT        MULT      -1            PAAMT
     C*
     C                   MOVE      XXNXSQ        PASEQ#
     C                   MOVE      Q1USER        PARQST
     C                   MOVE      Q1USER        PAUSER
     C                   MOVE      Q1PGM         PAPGM
     C                   MOVE      PATRDT        PADT
     C                   Z-ADD     DSTIME        PATIME
     C*** Begin add *** 08/29/11 ***************************************
     C                   MOVE      Q1USER        PAIUSR
     C                   MOVE      Q1PGM         PAIPGM
     C                   MOVE      PATRDT        PAIDT
     C                   Z-ADD     DSTIME        PAITIM
     C*** End add ***** 08/29/11 ***************************************
     C                   WRITE     PPA01
      *
     C*** Begin add *** 01/02/02 ***************************************
     C                   Z-ADD     PACHK#        XXCHK#
     C*** End add ***** 01/02/02 ***************************************
      *
     C                   ENDIF
      *
     C                   ENDSR
     C**************************************************************************
     C* UPDCLM - This subroutine updates the payment fields in the
     C*   Claim Master file.
     C**************************************************************************
     C     UPDCLM        BEGSR
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      '0'
     C*
     C                   Z-ADD     1             Y                 3 0
     C     PARECD        LOOKUP    RT(Y)                                  61
     C  N61              Z-ADD     1             Y
     C  N61*BLANK        LOOKUP    RT(Y)                                  61
      *
     C     *IN61         IFEQ      *ON
     C                   ADD       XXRAMT        AR(Y)
      *
     C     XXRAMT        IFLE      RR(Y)
     C*** Begin add *** 08/10/05 ***********************************************
     C     CMSTAT        ANDEQ     'O'
     C*** End add ***** 08/10/05 ***********************************************
     C                   SUB       XXRAMT        RR(Y)
     C                   ELSE
      *
      * Call Reserve History to Update record or write a new record as
      * needed.
      *
     C                   EXSR      RSVHST
     C                   ENDIF
      *
     C                   UPDATE    PCM01
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C* RSVHST - This subroutine creates an entry in the Reserve
     C*   History file since the remaining reserves are not updated.
     C**************************************************************************
     C     RSVHST        BEGSR
     C*
     C                   CLEAR                   PRH01
      *
     C                   Z-ADD     XXCO#         RHCO#
     C                   Z-ADD     XXFND         RHFND
     C                   Z-ADD     XXFYR         RHFYR
     C                   Z-ADD     XXCASE        RHCASE
     C*** Begin dlt *** 03/23/04 **************************************
     C**********           MOVE PARECD    RHRT01
     C**********           MOVE XXRAMT    RHSU01
     C*** End dlt ***** 03/23/04 **************************************
     C*** Begin add *** 03/23/04 **************************************
     C                   MOVE      PARECD        RRT(Y)
     C                   ADD       XXRAMT        RSU(Y)
     C*** End add ***** 03/23/04 **************************************
     C                   MOVEL     XXDESC        RHEXPL
     C                   MOVE      *BLANKS       RHSTAT
     C                   MOVE      CMSTAT        RHCLST
     C                   MOVE      Q1USER        RHUSER
     C                   MOVE      Q1PGM         RHPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RHDT
     C                   MOVE      RHDT          RHEFDT
     C                   Z-ADD     DSTIME        RHTIME
     C*** BEG ADD ***** 03/03/10 **************************************
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
     C*
     C                   WRITE     PRH01
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDDD  - This subroutine increases the amount pending and      t
     C*   decrease the amount recovered in the deductible detail file
     C**************************************************************************
     C     UPDDD         BEGSR

      *** beg add ***** 12/09/15 ***************************************
      /free
       If @@RtCd = 'Z';
          Leavesr;
       Endif;
      /end-free
      *** end add ***** 12/09/15 ***************************************

     C                   Z-ADD     XXRAMT        XXAMT
     C     DDKY          SETGT     PDD01                              60
     C     DDKY          READPE    PDD01                                  60
     C     *IN60         DOWEQ     *OFF
     C     XXAMT         ANDGT     0
     C*
     C*** Begin add *** 05/14/02 **************************************
     C     DDRECD        IFEQ      PARECD
     C*** End add ***** 05/14/02 **************************************
     C*
     C     XXAMT         IFGT      DDAMTR
     C                   SUB       DDAMTR        XXAMT
     C                   ADD       DDAMTR        DDAMTP
     C                   Z-ADD     0             DDAMTR
     C                   ELSE
     C                   SUB       XXAMT         DDAMTR
     C                   ADD       XXAMT         DDAMTP
     C                   Z-ADD     0             XXAMT
     C                   ENDIF
     C                   UPDATE    PDD01
     C*
     C*** Begin add *** 05/14/02 **************************************
     C                   ENDIF
     C*** End add ***** 05/14/02 **************************************
     C*
     C     DDKY          READPE    PDD01                                  60
     C                   ENDDO
      *
     C                   ENDSR
     C**************************************************************************
     C* UPDELP - This subroutine increases the Total claim payments    t
     C*   in the Employer Detail file.
     C**************************************************************************
     C     UPDELP        BEGSR
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
      *
     C     ELKY          CHAIN     PEL01                              60
     C     *IN60         IFEQ      '0'
     C*
     C*  Increase the total claim payment amount
     C*  Update the Emp Detail file.
     C*
     C                   ADD       XXRAMT        ELTCLP
     C                   MOVE      Q1USER        ELUSER
     C                   MOVE      Q1PGM         ELPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         ELDT
     C                   Z-ADD     DSTIME        ELTIME
     C                   UPDATE    PEL01
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
     C* UPDQDP - This subroutine increases the amount in the Pending
     C*   Deductible Recovery file.
     C**************************************************************************
     C     UPDDQP        BEGSR

      *** beg add ***** 12/09/15 ***************************************
      /free
       If @@RtCd = 'Z';
          Leavesr;
       Endif;
      /end-free
      *** end add ***** 12/09/15 ***************************************

     C*
     C*  Add the reversal amount into check# 9999999 in the Pending Ded
     C*    Recovery file.
     C*
     C*** Begin dlt *** 01/02/02 ***************************************
     C******************** Z-ADD9999999   XXCHK#
     C*** End dlt ***** 01/02/02 ***************************************
     C     DQKY          CHAIN     PDQ01                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     XXCO#         DQCO#
     C                   Z-ADD     XXFND         DQFND
     C                   Z-ADD     XXFYR         DQFYR
     C                   Z-ADD     XXEMP#        DQEMP#
     C                   Z-ADD     XXDIV         DQDIV
     C                   Z-ADD     XXCHK#        DQCHK#
     C                   MOVE      XXRAMT        DQAMT
     C                   MOVE      XXRAMT        DQAMTR
     C                   MOVE      Q1USER        DQUSER
     C                   MOVE      Q1PGM         DQPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         DQDT
     C                   Z-ADD     DSTIME        DQTIME
     C                   MOVE      *BLANK        DQSTAT
     C                   WRITE     PDQ01
     C                   ELSE
     C                   ADD       XXRAMT        DQAMTR
     C                   UPDATE    PDQ01
     C                   ENDIF
     C*
     C                   ENDSR
     C**************************************************************************
