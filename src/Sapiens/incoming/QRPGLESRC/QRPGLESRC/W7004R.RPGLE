     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H/TITLE W7004R - EOMB PAYMENT DELETION PGM
     H*****************************************************************
     H* DESCRIPTION:                                                  *
     H*      THIS PROGRAM READS THE REQUEST FOR PAYMENT FILE AND FOR  *
     H*      EACH REQUEST WITH EOB# NOT EQUAL TO ZERO, CHECKS THE     *
     H*      FAEOP FILE FOR AN ASSOCIATING RECORD. IF THE EOMB RCD    *
     H*      HAS BEEN DELETED THEN THE REQUEST FOR PMT RCD WILL BE    *
     H*      UPDATED WITH A STAT = 'D' AND NO PAYMENT WILL BE MADE.   *
     H*      A REPORT WILL LIST THE DELETED RECORDS.                  *
     H*                                                               *
     H*  If the EOMB exists in the LIVE file, nothing is done to the  *
     H*  pmt request.  If the EOMB exists in the deleted file, the    *
     H*  pmt request is chgd to a deleted sts.  If the EOMB doesn't   *
     H*  exist in either file, the pmt request is pended.             *
     H*                                                               *
     H* CALLED FROM: W6000C                                           *
     H* CALLS TO:    NONE                                             *
     H* CREATED:     09/09/93              PROJECT: WC265             *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:307 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 10/13/10  C:062  Project: 73643                               *
     H*                  SWITCH PARMS TO MATCH CALL.                  *
     H*                                                               *
     H* 06/12/00 C:62  Project: MC230                                 *
     H*                SWAP OUT FA020R AND FA021R WITH FA031R AND     *
     H*                FA032R.                                        *
     H*                                                               *
     H* 02/25/00 C:15  Project: WC577                                 *
     H*                Added code to load the maint fields.           *
     H*                                                               *
     H* 08/09/99 C:118 PROJECT: WC577                                 *
     H*                Verbiage corrections.                          *
     H*                                                               *
     H* 08/02/99 C:58 Project: WC925                                  *
     H*               When a pmt is deleted, delete the diary also.   *
     H*                                                               *
     H* 03/16/99 C:94 PROJECT: 25MKL                                  *
     H*            1. CHANGE CALL TO FA021R TO BE BASED ON IF PROVIDER*
     H*               BILL IS = 'N'                                   *
     H*                                                               *
     H* 03/17/99 C:56 Project: MC152                                  *
     H*               Return code 2 will be passed back from FA020R if*
     H*               client has Provider Bills installed and the     *
     H*               status of the bill is 'deleted' (EOBLST = D).   *
     H*                                                               *
     H* 03/04/99 C:15 Support: WC060/16413                            *
     H*              -Fixed date on the report heading.               *
     H*              -Chgd pgm to NOT process deleted pmts.           *
     H*                                                               *
     H* 01/20/99 C:15 Project: WC659                                  *
     H*               Don't update the records if the pmt status does *
     H*               NOT change.                                     *
     H*                                                               *
     H* 08/24/98 C:15 Project: WC659                                  *
     H*               Added code to complete the DARTS diaries on     *
     H*               deleted pmts.                                   *
     H*                                                               *
     H* 07/20/98 C:15 Project: WC659                                  *
     H*               Added code to load RPSTS.                       *
     H*                                                               *
     H* 10/30/97 C:15 Project: WC659                                  *
     H*               Chg pgm to use APIs to get WCMC info.           *
     H*                                                               *
     H* 06/03/97 C:15 PROJECT: WC599                                  *
     H* 12/24/96 C:58 PROJECT: WC599                                  *
     H* 10/24/96 C:62 PROJECT: WC599                                  *
     H*****************************************************************
     H*  FILE USAGE:                                                  *
     H*   SMCOL   PCO01: CO                                   Company *
     H*   SMORL   POR01: CO/PGM                              Override *
     H*   WMUPL   PUP01: USER                               User Parm *
     H*   WARPL4  PRP01: CO/FND/FYR/CASE  OMIT(RPSTAT NE ' ')         *
     H*   FAEOL4  PEO01: CO/SUB#/FND/FYR/CASE/EOB#   EOMB TRANSACTION *
     H*   FDEOL1  PEO01: CO/SUB#/FND/FYR/CASE/EOB#   EOMB TRANSACTION *
     H*   FMFNL1  PFN01: CO/FND                    FUND MASTER (WCMC) *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*   21   RECORD DELETED/PENDED                                  *
     H*   40   READ OVERRIDE FILE (SMORP)                             *
     H*   50   READ REQUEST PAYMENT FILE                              *
     H*   60   GENERAL CHAINS & READS                                 *
     H*****************************************************************
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FWMUPL     IF   E           K DISK
     FWARPL     UF   E           K DISK
     F*** BEGIN ADD *** 07/20/98 **************************************
     FWTPSL     IF   E           K DISK
     F*** END ADD ***** 07/20/98 **************************************
     F*** BEGIN DLT *** 10/30/97 **************************************
     F*****FAEOL4  IF  E           K        DISK
     F*****FDEOL1  IF  E           K        DISK
     F*****            PEO01                             KRENAMEPEO02
     F*****FMFNL1  IF  E           K        DISK
     F*** END DLT ***** 10/30/97 **************************************
     F*** BEGIN ADD *** 03/24/98 **************************************
     FSDCMLA    UF   E           K DISK
     F*** END ADD ***** 03/24/98 **************************************
     F*** BEGIN ADD *** 11/17/98 **************************************
     FWMCML1    IF   E           K DISK
     F                                     RENAME(PCM01:PCM02)
     F*** END ADD ***** 11/17/98 **************************************
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOF)
     F*

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*** BEGIN ADD *** 03/24/98 **************************************
     D                 DS
     D  QCKEY                  1     40
     D  RPCO#                  1      3  0
     D  RPFND                  4      6  0
     D  RPFYR                  7      9  0
     D  RPCASE                10     16  0

     I*** END ADD ***** 03/24/98 **************************************
     I*

     C****************************************************************
     C*  KEY LISTING FOR PROGRAM FILES
     C****************************************************************
     C*
     C*  COMPANY FILE
     C*
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*  OVERRIDE FILE
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*  USER PARAMETER FILE
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*  REQUEST PAYMENT KEY LIST
     C*
     C     RPKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C*  Key list for the Payment Status table file
     C*
     C     PSKY          KLIST
     C                   KFLD                    XXCO#
     C*** END ADD ***** 07/20/98 **************************************
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*  DARTS FILE
     C*
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    RPDSEQ
     C*** END ADD ***** 03/24/98 **************************************
     C*** BEGIN ADD *** 11/17/98 **************************************
     C*  Key list for the Claim Master file
     C*
     C     C2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    RPFND
     C                   KFLD                    RPFYR
     C                   KFLD                    RPCASE
     C*** END ADD ***** 11/17/98 **************************************
     C*
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C*  EOMB TRANSACTION FILE
     C**********
     C********** EOKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXSUB#
     C**********           KFLD           XXFND
     C**********           KFLD           XXFYR
     C**********           KFLD           XXCASE
     C**********           KFLD           XXEOB#
     C**********
     C*  MANAGED CARE FUND MASTER FILE (FOR SUB#)
     C**********
     C********** FNKY      KLIST
     C**********           KFLD           XXCO#
     C**********           KFLD           XXWCF#
     C*** END DLT ***** 10/30/97 ***********************************************
     C*
     C****************************************************************
     C*  *LIKE DEFINES FOR PROGRAM VARIABLES
     C****************************************************************
     C*
     C     *LIKE         DEFINE    RPCO#         XXCO#
     C     *LIKE         DEFINE    RPFND         XXFND
     C     *LIKE         DEFINE    RPFYR         XXFYR
     C     *LIKE         DEFINE    RPCASE        XXCASE
     C     *LIKE         DEFINE    RPPGM         XXPGM
     C     *LIKE         DEFINE    RPEOB#        XXEOB#
     C*** BEGIN ADD *** 07/20/98 **************************************
     C     *LIKE         DEFINE    RPSTS         XXPSTS
     C     *LIKE         DEFINE    RPSTS         XXDSTS
     C*** END ADD ***** 07/20/98 **************************************
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C********** *LIKE     DEFN EOSUB#    XXSUB#
     C********** *LIKE     DEFN FNWCF#    XXWCF#
     C*** END DLT ***** 10/30/97 ***********************************************
     C*
     C                   Z-ADD     *ZERO         XXCNT             7 0          # DELETED RECS
     C                   MOVE      *BLANK        XXPRT             1
     C                   Z-ADD     *ZERO         XXCNTP            7 0          # PENDED  RECS
     C*
     C****************************************************************
     C*  PROGRAM ENTRY PARAMETERS
     C****************************************************************
     C*
     C     *ENTRY        PLIST
     C*** BEGIN CHG *** 10/13/10 **************************************
     C**********           PARM           QQDATE  8
     C**********           PARM           QQCO#   3
     C                   PARM                    QQCO#             3
     C                   PARM                    QQDATE            8
     C*** END   CHG *** 10/13/10 **************************************
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DT6MDY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANK        SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C                   MOVE      QQDATE        XXDT8             8 0
     C                   MOVE      QQCO#         XXCO#
     C*
     C*** BEGIN DLT *** 03/04/99 *************************************
     C******************** Z-ADDXXDT8     SRDTI
     C******************** CALL 'SRCVTDAT'DT6MDY
     C******************** MOVELSRDTO     XXDATE  60
     C*** END DLT ***** 03/04/99 *************************************
     C*
     C****************************************************************
     C*  START PROGRAM MAINLINE
     C****************************************************************
     C*** BEG ADD *** 03/16/99 ***************************************
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C*** END ADD *** 03/16/99 ***************************************
     C                   TIME                    DSDTTM
     C*
     C*  GET USER, COMPANY, AND OVERRIDE INFORMATION
     C*
     C     UPKY          CHAIN     PUP01                              60
     C     COKY          CHAIN     PCO01                              60
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              40
     C                   SETON                                        OF
     C*
     C*** BEGIN ADD *** 07/20/98 **************************************
     C*  Get valid 'Pended' and 'Deleted' status codes.
     C*
     C     PSKY          CHAIN     PPS01                              60
     C     *IN60         DOWEQ     '0'
     C*
     C     PSTYPE        IFEQ      'D'
     C     PSSTAT        ANDNE     'I'
     C     XXDSTS        ANDEQ     *BLANKS
     C                   MOVEL(P)  PSKEY         XXDSTS
     C                   ENDIF
     C*
     C     PSTYPE        IFEQ      'P'
     C     PSSTAT        ANDNE     'I'
     C     XXPSTS        ANDEQ     *BLANKS
     C                   MOVEL(P)  PSKEY         XXPSTS
     C                   ENDIF
     C*
     C     PSKY          READE     PPS01                                  60
     C                   ENDDO
     C*** END ADD ***** 07/20/98 **************************************
     C*
     C*
     C*  READ THE REQUEST FOR PAYMENT FILE
     C*
     C     RPKY          CHAIN     PRP01                              50
     C     *IN50         DOWEQ     '0'
     C*
     C*  ONLY CHECK PAYMENT RECORDS FOR AN ASSOCIATING EOMB RECORD
     C*  IF RPEOB# IS NOT EQUAL TO 0
     C*
     C     RPEOB#        IFNE      0
     C*** BEGIN ADD *** 03/04/99 *************************************
     C     RPSTAT        ANDNE     'D'
     C*** END ADD ***** 03/04/99 *************************************
     C*
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C*  CHAIN OUT FUND MASTER (WCMC) TO GET SUBSCRIBER NUMBER
     C**********
     C**********           Z-ADDRPFND     XXWCF#
     C********** FNKY      CHAINPFN01                60
     C********** *IN60     IFEQ '0'
     C**********           Z-ADDFNSUB#    XXSUB#
     C**********           Z-ADDFNFND     XXFND
     C*** END DLT ***** 10/30/97 ***********************************************
     C*
     C*  CHAIN OUT TO THE EOMB TRANSACTION FILE
     C*
     C                   Z-ADD     RPFYR         XXFYR
     C                   Z-ADD     RPCASE        XXCASE
     C                   Z-ADD     RPEOB#        XXEOB#
     C*** BEGIN ADD *** 01/20/99 **************************************
     C                   MOVE      RPSTAT        SVSTAT            1
     C*** END ADD ***** 01/20/99 **************************************
     C*
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C********** EOKY      CHAINPEO01                60
     C********** *IN60     IFEQ '1'
     C**********           MOVE *OFF      *IN21
     C********** EOKY      CHAINPEO02                60
     C*** END DLT ***** 10/30/97 ***********************************************
     C*
     C                   MOVEA     '00'          *IN(22)
     C     RPSTAT        IFEQ      'P'
     C                   MOVE      '1'           *IN22
     C                   ELSE
     C     RPSTAT        IFEQ      'D'
     C                   MOVE      '1'           *IN23
     C                   ENDIF                                                         =P
     C                   ENDIF                                                         =D
     C                   MOVEL     RPPGM         XXPGM
     C*
     C*** BEGIN ADD *** 10/30/97 ***********************************************
     C*  First, see if the EOMB exists in the active EOMB file (FAEOP)
     C*
     C*** BEGIN CHG *** 06/12/00 **************************************
     C**********           MOVE RPCO#     ##CO#
     C**********           MOVE RPFND     ##FND
     C**********           MOVE RPFYR     ##FYR
     C**********           MOVE RPCASE    ##CASE
     C**********           MOVE RPEOB#    ##EOB#
     C**********           CALL 'FA020R'
     C**********           PARM *BLANK    ##CALL  1
     C**********           PARM           ##CO#   3
     C**********           PARM           ##FND   3
     C**********           PARM           ##FYR   3
     C**********           PARM           ##CASE  7
     C**********           PARM           ##EOB#  9
     C**********           PARM *ZEROS    ##AMT   9
     C**********           PARM *BLANK    ##RC    1
     C*
     C*  If the EOMB exists in the active file, skip to the next pmt.
     C*
     C********** ##RC      CABEQ*BLANK    NEXT
     C*
     C*  Next, check the EOMB delete file for the EOMB.
     C*
     C*** BEG ADD *** 03/16/99 **************************************
     C********** ##PRVB    IFNE 'Y'
     C*** END ADD *** 03/16/99 **************************************
     C**********           CALL 'FA021R'
     C**********           PARM *BLANK    ##CALL  1
     C**********           PARM           ##CO#   3
     C**********           PARM           ##FND   3
     C**********           PARM           ##FYR   3
     C**********           PARM           ##CASE  7
     C**********           PARM           ##EOB#  9
     C**********           PARM *BLANK    ##RC    1
     C*** BEG ADD *** 03/16/99 **************************************
     C**********           ENDIF
     C*
     C********** ##RC      IFEQ '2'
     C**********           MOVE ' '       ##RC
     C**********           ENDIF                           ##RC = 2
     C*** END ADD *** 03/16/99 **************************************
     C     ##PRVB        IFEQ      'Y'
     C*
     C                   CALL      'FA031R'
1    C                   PARM      RPCO#         #@CO#             3 0          Company #
2    C                   PARM      RPFND         #@WCF#            3 0          WCMS Fund
3    C                   PARM      RPFYR         #@FYR             3 0          Policy Year
4    C                   PARM      RPCASE        #@CASE            7 0          Claim #
5    C                   PARM      RPEOB#        #@EOB#            9 0          EOMB #
6    C                   PARM      RPEOBS        #@RCN             3 0          RECON #
7    C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ELSE
     C*
     C                   CALL      'FA032R'
     C                   PARM      RPCO#         #@CO#             3 0          Company #
     C                   PARM      RPFND         #@WCF#            3 0          WCMS Fund
     C                   PARM      RPFYR         #@FYR             3 0          Policy Year
     C                   PARM      RPCASE        #@CASE            7 0          Claim #
     C                   PARM      RPEOB#        #@EOB#            9 0          EOMB #
     C                   PARM      *BLANKS       ##RC              1            Return Code
     C*
     C                   ENDIF
     C*
     C     ##RC          IFEQ      '2'
     C*
     C                   MOVEL     *BLANKS       ##RC
     C*
     C                   ELSE
     C*
     C     ##RC          IFNE      '1'
     C     ##RC          ANDNE     '3'
     C     ##RC          ANDNE     '5'
     C*
     C                   GOTO      NEXT
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   CHG *** 06/12/00 **************************************
     C*
     C*  If the EOMB exists in the delete file, delete the pmt.  Otherwise,
     C*    put it on a pend status.
     C*
     C     ##RC          COMP      *BLANK                                 21
     C     *IN21         IFEQ      '1'
     C*** END ADD ***** 10/30/97 ***********************************************
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C********** *IN60     IFEQ '0'
     C**********           MOVE *ON       *IN21
     C*** END DLT ***** 10/30/97 ***********************************************
     C                   MOVE      'D'           RPSTAT
     C*** BEGIN ADD *** 07/20/98 **************************************
     C                   MOVE      XXDSTS        RPSTS
     C*** END ADD ***** 07/20/98 **************************************
     C                   ADD       1             XXCNT
     C*** BEGIN ADD *** 11/17/98 **************************************
     C                   MOVE      '1'           QQTYPE
     C*** END ADD ***** 11/17/98 **************************************
     C*
     C*** BEGIN ADD *** 03/24/98 **************************************
     C*  DELETE   the diary which is attached to the pmt.
     C*
     C     CMKY          CHAIN     PCM01                              60
     C     *IN60         IFEQ      *OFF
     C     CMDCDT        ANDEQ     0
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CMDCDT
     C                   MOVEL     Q1PGM         CMPGM
     C                   MOVE      CMDCDT        CMDT
     C                   MOVE      *BLANKS       CMFL04
     C*** BEGIN ADD *** 08/02/99 **************************************
     C                   MOVEL     'I'           CMSTAT
     C*** END ADD ***** 08/02/99 **************************************
     C                   UPDATE    PCM01
     C                   ENDIF
     C*** END ADD ***** 03/24/98 **************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      'P'           RPSTAT
     C*** BEGIN ADD *** 07/20/98 **************************************
     C                   MOVE      XXPSTS        RPSTS
     C*** END ADD ***** 07/20/98 **************************************
     C                   ADD       1             XXCNTP
     C*** BEGIN ADD *** 11/17/98 **************************************
     C                   MOVE      'P'           QQTYPE
     C*** END ADD ***** 11/17/98 **************************************
     C*
     C                   ENDIF                                                  END PEO02
     C*
     C*** BEGIN ADD *** 01/20/99 **************************************
     C     RPSTAT        IFNE      SVSTAT
     C*** END ADD ***** 01/20/99 **************************************
     C                   MOVE      Q1PGM         RPPGM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RPDT
     C*** BEGIN ADD *** 02/25/00 **************************************
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        RPTIME
     C                   MOVEL     Q1USER        RPUSER
     C*** END ADD ***** 02/25/00 **************************************
     C                   UPDATE    PRP01
     C*
     C*** BEGIN ADD *** 11/17/98 **************************************
     C*  If the claim is closed and the payment was released, process
     C*  reserve cushion.
     C*
     C     C2KY          CHAIN     PCM02                              60
     C     *IN60         IFEQ      *OFF
     C     CMSTAT        ANDEQ     'C'
     C*** BEGIN ADD *** 01/20/99 **************************************
     C     SVSTAT        ANDEQ     ' '
     C*** END ADD ***** 01/20/99 **************************************
     C*
     C                   CALL      'SRPRCC'
     C                   PARM                    CMCO#
     C                   PARM                    CMFND
     C                   PARM                    CMFYR
     C                   PARM                    CMCASE
     C                   PARM                    RPRECD
     C                   PARM                    QQTYPE            1
     C                   PARM                    RPAMT
     C*
     C                   ENDIF
     C*** END ADD ***** 11/17/98 **************************************
     C*
     C*  LOAD DATES FOR REPORT
     C*
     C                   Z-ADD     RPTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTRDT            6 0
     C                   Z-ADD     RPFRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFRDT            6 0
     C                   Z-ADD     RPTODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXTODT            6 0
     C*
     C*  PRINT REPORT. PRINT ROUTING PAGE IF FIRST TIME
     C*
     C     *IN11         IFEQ      '0'
     C     *IN40         ANDEQ     '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   SETON                                        11OF
     C                   END
     C   OF              EXCEPT    HEAD
     C*
     C                   EXCEPT    DET
     C*
     C*** BEGIN ADD *** 01/20/99 **************************************
     C                   END                                                    RPSTAT <> SVSTAT
     C*** END ADD ***** 01/20/99 **************************************
     C*** BEGIN DLT *** 10/30/97 ***********************************************
     C******************** END                             PEO01: *IN60 = '0'
     C******************** END                             PFN01: *IN60 = '0'
     C*** END DLT ***** 10/30/97 ***********************************************
     C*
     C                   END                                                    RPEOB# <> 0
     C*
     C*  READ NEXT REQUEST RECORD
     C*
     C*** BEGIN ADD *** 10/30/97 ***********************************************
     C     NEXT          TAG
     C*** END ADD ***** 10/30/97 ***********************************************
     C     RPKY          READE     PRP01                                  50
     C                   END                                                    50 DOWEQ '0'
     C*
     C*  END PROGRAM
     C*
     C     *IN11         IFEQ      '0'
     C     *IN40         ANDEQ     '0'
      ***** Beg Del ***** 08/26/14 ************************************
     C**********         EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   SETON                                        OF
     C                   END
     C     XXCNT         IFGT      *ZERO
     C     XXCNTP        ORGT      *ZERO
     C   OF              EXCEPT    HEAD
     C                   EXCEPT    TOTAL
     C                   END
     C*
     C*  END OF PROGRAM
     C*
      ***** Beg Del ***** 08/26/14 ************************************
     C                   EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 ************************************
     C                   MOVE      '1'           *INLR
     C*
     C**************************************************************************
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********E            ROUTE          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********E            ROUTE          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********E            ROUTE          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********E            ROUTE          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********E            ROUTE          1
     O**********                                 15 'ROUTE TO:'
     O**********             ORDIS1              60
     O**********E            ROUTE          1
     O**********             ORDIS2              60
     O**********E            ROUTE          1
     O**********             ORDIS3              60
     O**********E            ROUTE          2
     O**********             ORDIS4              60
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O                       ORDIS1              60
     O          E            ROUTE          1
     O                       ORDIS2              60
     O          E            ROUTE          1
     O                       ORDIS3              60
     O          E            ROUTE          2
     O                       ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********E            ROUTE          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             ORPNAM              30
     O**********                                 36 'FILE:'
     O**********             ORFIL#              38
     O**********                                 46 'COPIES:'
     O**********             ORCOPY        Z     50
     O**********                                 56 'HOLD:'
     O**********             ORHOLD              61
     O**********E            ROUTE          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             OROUTQ              30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 ************************************
     O          E            ROUTE            60
     O                                           23 '(C) COPYRIGHT PROGRESSI'
     O                                           46 'VE DATA SOLUTIONS. INC,'
     O                                           51 '.1996'
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              86
     O                       DSDATE        Y    130
     O          E            HEAD           1
     O                       Q1PGM               10
     O                                           71 'DELETED PAYMENT REQUESTS'
     O                                           75 'FOR'
     O*** BEGIN DLT *** 03/04/99 *************************************
     O************************ XXDATEY   84
     O*** END DLT ***** 03/04/99 *************************************
     O*** BEGIN ADD *** 03/04/99 *************************************
     O                       XXDT8               86 '  /  /    '
     O*** END ADD ***** 03/04/99 *************************************
     O                                          124 'PAGE:'
     O                       PAGE          Z    130
     O***** BEGIN CHG **** 08/09/99 ***********************************
     O********E**1             HEAD
     O          E            HEAD        2  1
     O***** END   CHG **** 08/09/99 ***********************************
     O                                           49 'TRANSACTION'
     O                                           59 'TRANS'
     O                                          112 'LAST'
     O                                          132 'PRV  CUR '
     O          E            HEAD           1
     O***** BEGIN CHG **** 08/09/99 ***********************************
     O**********                         07 'FUND'
     O                                           08 'GROUP'
     O**********                         13 'FYR'
     O                                           14 'YEAR'
     O***** END   CHG **** 08/09/99 ***********************************
     O                                           23 'CASE #'
     O                                           34 'EOMB #'
     O                                           49 'CODE   TYPE'
     O                                           58 'DATE'
     O                                           74 'AMOUNT'
     O                                           84 'FED ID#'
     O                                           89 'MOD'
     O                                          104 'SERVICE DATES'
     O                                          115 'PROGRAM'
     O                                          132 'STAT STAT'
     O          E            DET            1
     O                       RPFND         Z     07
     O                       RPFYR         Z     13
     O                       RPCASE        Z     23
     O                       XXEOB#        4     35
     O                       RPTRCD              41
     O                       RPTYPE              48
     O                       XXTRDT        Y     60
     O                       RPAMT         J     76
     O                       RPFEIN              85
     O                       RPMOD               89
     O                       XXFRDT        Y     98
     O                       XXTODT        Y    107
     O                       XXPGM              118
     O              N22N23                      126 'RLS'
     O               22                         126 'PND'
     O               23                         126 'DLT'
     O               21                         132 'DLT'
     O              N21                         132 'PND'
     O          E            TOTAL       3  1
     O                                           21 'NUMBER OF RECORDS'
     O                                           30 'DELETED:'
     O                       XXCNT         4     39
     O          E            TOTAL          1
     O                                           21 'NUMBER OF RECORDS'
     O                                           30 'PENDED :'
     O                       XXCNTP        4     39
