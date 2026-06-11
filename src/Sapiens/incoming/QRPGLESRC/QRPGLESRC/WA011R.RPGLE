     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H/TITLE WA011R - API:  Reserve Edit
     H*****************************************************************
     H*  DESCRIPTION:
     H*
     H*    This routinue will check for all scenarios listed below in
     H*    the order listed below.
     H*
     H*    If Requested Amount passes ALL edits, the Return Code will
     H*    be Blank.  Otherwise, the first edit error encountered will
     H*    assign the letter to the Return Code and the API will exit.
     H*
     H* ------
     H* Return
     H* Code      Edit Description
     H* ------
     H* 01 -Group Master record check.
     H* 02 -Group Detail record check.
     H* 03 -If the Groups Expiration Date is less than or equal to system
     H*     date, checks cannot be released.
     H* 04 -Group Auxiliary record check.
     H* 05 -If the Check Processing flag in Group Auxiliary is 'N', then a
     H*     check cannot be released.
     H* 06 -Claim Master record check.
     H* 07 -User Parameter record check.
     H* 08 -Claim/DARTS Control record check.
     H* 09 -If the claim indicates a 'Y' in the Denied flag, then the users
     H*     security level and type must pass the security level and type
     H*     defined in the Claims/DARTS Control file.
     H*
     H*
     H*  CREATED: 07/02/97  C:30  Project:  WC660
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 05/20/08 C:180  SUPPORT 63210
     H*                 Fix delay authority security checking.
     H*
     H* 12/05/07 C:193  Project: 135WKJ                               *
     H*                 Change Denied Claim Payment Security Edit     *
     H*                                                               *
     H* 02/06/06 C:168  Project: 132WBU                               *
     H*                 Added extra check to Edit #9 to also check if *
     H*                 the user has authority to Delay the claim     *
     H*                                                               *
     H* 04/28/98 C:15 Project: WC577                                  *
     H*               Added 5 addl parms to SRCCSC.  These parms are  *
     H*               addl security types.                            *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*****************************************************************
      *  Group Master file
     FWMFNL     IF   E           K DISK
      *________________________________________________________________
      *  Group Detail file
     FWDF1L     IF   E           K DISK
      *________________________________________________________________
      *  Group Auxiliary file
     FWDF2L     IF   E           K DISK
      *________________________________________________________________
      *  Claim Master file
     FWMCML1    IF   E           K DISK
      *________________________________________________________________
      *  Claim/DARTS Control file
     FWMCXL     IF   E           K DISK
      *________________________________________________________________
      *  User Parameter file
     FWMUPL     IF   E           K DISK
      *________________________________________________________________
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *________________________________________________________________
      *
      *  User Parameter file key
      *

     C     UPKY          KLIST
     C                   KFLD                    Q1USER
      *
      *  Group Master, Group Auxiliary, Claim/DARTS file key
      *
     C     FNKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
      *
      *  Group Detail
      *
     C     F1KY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
      *
      *  Claim Master key
      *
     C     CMKY          KLIST
     C                   KFLD                    @@CO#
     C                   KFLD                    @@FND
     C                   KFLD                    @@FYR
     C                   KFLD                    @@CASE
      *________________________________________________________________
     C     *LIKE         DEFINE    CMCO#         @@CO#
     C     *LIKE         DEFINE    CMFND         @@FND
     C     *LIKE         DEFINE    CMFYR         @@FYR
     C     *LIKE         DEFINE    CMCASE        @@CASE
      *________________________________________________________________
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *________________________________________________________________
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@CO#                          Company
     C                   PARM                    @@FND                          Group
     C                   PARM                    @@FYR                          Fund Year
     C                   PARM                    @@CASE                         Claim
     C                   PARM                    @@RC              2            Return Code
     C                   PARM                    @@03              1            Skip error #03
     C                   PARM                    @@05              1            Skip error #05
     C                   PARM                    @@09              1            Skip error #09
     C*
     C*  NOTE:  @@03/@@05/@@09 SHOULD BE PASSED AS 'Y' IF ERRORS #03/
     C*         #05/#09 SHOULD BE SKIPPED.  THESE PARMS ARE NOT
     C*         NEEDED FOR THE PROGRAM TO EXECUTE NORMALLY.
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@03          SKIP03
     C                   ELSE
     C                   MOVE      'N'           SKIP03            1
     C                   ENDIF                                                  Q1PRMS>=6
     C*
     C     Q1PRMS        IFGE      7
     C                   MOVE      @@05          SKIP05
     C                   ELSE
     C                   MOVE      'N'           SKIP05            1
     C                   ENDIF                                                  Q1PRMS>=7
     C*
     C     Q1PRMS        IFGE      8
     C                   MOVE      @@09          SKIP09
     C                   ELSE
     C                   MOVE      'N'           SKIP09            1
     C                   ENDIF                                                  Q1PRMS>=8
      *________________________________________________________________
      *
     C                   MOVE      *BLANK        @@RC
      *________________________________________________________________
     C*** Beg Add ***** 12/05/07 *****************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    @@CLT#            3
     C     @@CLT#        IFEQ      '135'
     C                   MOVE      'Y'           SKIP09
     C                   ENDIF
     C*** End Add ***** 12/05/07 *****************************************
      *
      *  Edit 01:  Group Master record check
      *
      *  Access Group Master                CO#/FND
      *
     C     FNKY          CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '01'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 02:  Group Detail record check
      *
      *  Access Group Detail                CO#/FND
      *
     C     F1KY          CHAIN     PF101                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '02'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 03:  Group Master Expiration Date check
      *
     C     SKIP03        IFNE      'Y'
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT              7 0
      *
     C     FNEXDT        IFGT      *ZERO                                        Group Expiration DT
     C     FNEXDT        ANDLE     XXDT
     C                   MOVE      '03'          @@RC
     C                   RETURN
     C                   ENDIF
     C*
     C                   ENDIF                                                  SKIP03<>Y
      *________________________________________________________________
      *
      *  Edit 04:  Group Auxiliary record check
      *
      *  Access Group Auxiliary             CO#/FND
      *
     C     FNKY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '04'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 05:  Group Auxiliary Process Claims Flag check
      *
     C     SKIP05        IFNE      'Y'
     C*
     C     F2CLCK        IFNE      'Y'                                          Process Claims
     C                   MOVE      '05'          @@RC
     C                   RETURN
     C                   ENDIF                                                  F2CLCK<>Y
     C*
     C                   ENDIF                                                  SKIP05<>Y
      *________________________________________________________________
      *
      *  Edit 06:  Claim Master record check
      *
      *  Access Claim Master                CO#/FND/FYR/CASE
      *
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '06'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 07:  User Parameter record check
      *
      *  Access User Parameters             USRPRF
      *
     C     UPKY          CHAIN     PUP01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '07'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 08:  Claim/DARTS Control record check
      *
      *  Access Claim/DARTS Control file    CO#/FND
      *
     C     FNKY          CHAIN     PCX01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '08'          @@RC
     C                   RETURN
     C                   ENDIF
      *________________________________________________________________
      *
      *  Edit 09:  If Claim Denied Flag 'Y', Security Level & Type check
      *           with the Claim/DARTS Control file values.
      *
     C     SKIP09        IFNE      'Y'
     C*
     C     CMDNY         IFEQ      'Y'                                          Denied Flag
      *
      *  Security level and Type check
      *
     C                   MOVE      UPTYPE        SRUTYP            1            User Type
     C                   MOVE      UPLVL         SRULVL            3            User Level
     C                   MOVE      CXPDCT        SRCTYP            1            PyDenial Type
     C                   MOVE      CXPYOD        SRCLVL            3            PyDenial Level
     C                   MOVE      '1'           SREXEC            1            Execution Type
     C                   MOVE      *ON           SRRES             1            Result Flag
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM                    SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM                    SREXEC
     C                   PARM                    SRRES
     C*** BEGIN ADD *** 04/28/98 **************************************
     C                   PARM      CXPDC2        SRCTP2            1
     C                   PARM      CXPDC3        SRCTP3            1
     C                   PARM      CXPDC4        SRCTP4            1
     C                   PARM      CXPDC5        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
     C*** END ADD ***** 04/28/98 **************************************
      *
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '09'          @@RC
     C                   RETURN
     C                   ENDIF                                                  SRRES=*ON
     C*
     C                   ENDIF                                                  CMDNY=Y
      *
      ******  BEG  ADD  02/06/06 **************************************
      *  Edit 09: (CONTINUE)                                           k
      *           Checks if the user has authority to dalay the claim
      *
     C     CMDLY         IFEQ      'Y'                                          Delayed Flag
      *
      *  Security level and Type check
      ******* BEG ADD ******* 05/20/08 ********************************
     C                   MOVE      UPTYPE        SRUTYP                         User Type
     C                   MOVE      UPLVL         SRULVL                         User Level
      ******* END ADD ******* 05/20/08 ********************************
     C                   MOVE      CXDSLV        SRCLVL
      *
     C                   CALL      'SRCCSC'
     C                   PARM                    SRUTYP
     C                   PARM                    SRULVL
     C                   PARM      CXDS01        SRCTYP
     C                   PARM                    SRCLVL
     C                   PARM      '1'           SREXEC
     C                   PARM                    SRRES
     C                   PARM      CXDS02        SRCTP2            1
     C                   PARM      CXDS03        SRCTP3            1
     C                   PARM      CXDS04        SRCTP4            1
     C                   PARM      CXDS05        SRCTP5            1
     C                   PARM      ' '           SRCTP6            1
      *
     C     SRRES         IFEQ      *ON                                          NO Access
     C                   MOVE      '09'          @@RC
     C                   RETURN
     C                   ENDIF                                                  SRRES=*ON
     C*
     C                   ENDIF                                                  CMDLY=Y
      ******  BEG  ADD  02/06/06 **************************************
     C*
     C                   ENDIF                                                  SKIP09<>Y
      *________________________________________________________________
      *
      *  All Edits Passed - Return Code assigned Blank
      *
     C                   MOVE      *BLANK        @@RC
     C                   RETURN
