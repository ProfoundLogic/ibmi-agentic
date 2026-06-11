     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE FIXRENEW - Delete renewal records
     H*=================================================================
     H*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*!!! VERY IMPORTANT THIS PROGRAM BE CALLED ONLY FROM FIXRENEWC !!!
     H*!!! OTHERWISE DUPLICATE RECORDS WILL BE CREATED IN LIVE DATA. !!!
     H*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     H*
     H*  FIXRENEWC - Overrides physical files to archive library prior
     H*              to calling this program.
     H*=================================================================
     H*  DESCRIPTION:
     H*    Delete records created from a renewal process submitted
     H*    in error.
     H*
     H*  CALLED FROM: FIXRENEWC
     H*
     H*  GIVEN:
     H*    co#   - Company number
     H*    fnd   - Group number
     H*    user  - User login name
     H*    date  - Date renewal was run
     H*    dart  - 'Y'es, delete policy page DARTS remarks created on
     H*            (cdat) by (user) for selected employer divisions.
     H*            'N'o, do not delete Policy Page DARTS remarks.
     H*    cdat  - Date used to locate policy page DARTS remark for
     H*            deletion.
     H*
     H*  PERFORMS:
     H*  - Policy records purged from files:
     H*                  WDELP, WMRPP, WDLDP, WMAHP, WABLP, WMEPP
     H*                  WDEHP, WMEAP, WDDSL, WDILP, WDMPP.
     H*  - Spool output of policies removed.
     H*  - Deletion of selected policy page DARTS remarks.
     H*  - Archive of all records deleted in library FIXRENEW.
     H*
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/13/10 C:136 SUPPORT: WA133 74437                           *
     H*                ADD FILES                                      *
     H*                                                               *
     H* 03/02/04 C:61 SUPPORT: WA072 40285                            *
     H*             - Changed to archive records before delete.       *
     H*             - Changed to select policies for deletion based   *
     H*               on job run date and user who ran job.           *
     H*             - Changes not commented - this was effectively    *
     H*               a rewrite.                                      *
     H*                                                               *
     H* 01/30/04 C:33 Support: WA081/41110                            *
     H*               Remove records from DARTS.                      *
     H*                                                               *
     H* 02/27/02 C:65 PROJECT: SUPPORT                                *
     H*               DELETE OUT ADDITIONAL RECORDS CREATED FROM      *
     H*               RUNNING A BILL ON THE RENEWALS.                 *
     H*                                                               *
     H*  CREATED: 02/16/02  PROGRAMMER: C:61  SUPPORT: WA114 30005    *
     H*****************************************************************
      *
      * Employer Detail
     FWDELL1    UF   E           K DISK
     FWDELP     O    E             DISK
     F                                     RENAME(PEL01:PEL02)
      *
      * Carrier/State Reporting Master
     FWMRPL     UF   E           K DISK
     FWMRPP     O    E             DISK
     F                                     RENAME(PRP01:PRP02)
      *
      * Additional Entity / Location Detail
     FWDLDL1    UF   E           K DISK
     FWDLDP     O    E             DISK
     F                                     RENAME(PLD01:PLD02)
      *
      * Agent History
     FWMAHL     UF   E           K DISK
     FWMAHP     O    E             DISK
     F                                     RENAME(PAH01:PAH02)
      *
      * Billing Work
     FWABLL     UF   E           K DISK
     FWABLP     O    E             DISK
     F                                     RENAME(PBL01:PBL02)
      *
      * Employer Payroll
     FWMEPL     UF   E           K DISK
     FWMEPP     O    E             DISK
     F                                     RENAME(PEP01:PEP02)
     F*** BEGIN ADD ***  02/27/02  ************************************
      *
      * Employer Detail History
     FWDEHL     UF   E           K DISK
     FWDEHP     O    E             DISK
     F                                     RENAME(PEH01:PEH02)
      *
      * Accounts / Receivable
     FWMEAL     UF   E           K DISK
     FWMEAP     O    E             DISK
     F                                     RENAME(PEA01:PEA02)
      *
      * Employer Discount
     FWDDSL     UF   E           K DISK
     FWDDSP     O    E             DISK
     F                                     RENAME(PDS01:PDS02)
      *
      * Increased Limits Detail
     FWDILL     UF   E           K DISK
     FWDILP     O    E             DISK
     F                                     RENAME(PIL01:PIL02)
      *
      * Premium Detail
     FWDMPL     UF   E           K DISK
     FWDMPP     O    E             DISK
     F                                     RENAME(PMP01:PMP02)
     F*** END ADD *****  02/27/02  ************************************
     F***  Begin Add  ***  01/30/04  **************************************
      *
      * DARTS
     FSDCMLD    UF   E           K DISK
     FSDCMP     O    E             DISK
     F                                     RENAME(PCM01:PCM02)
      *
      * Admin / DARTS Control Definition
     FWMEXL     IF   E           K DISK
     F***  End   Add  ***  01/30/04  **************************************
     F*** BEG ADD ***** 10/13/10 ******************************************
      * Renewal Tracking
     FWDRTL     UF   E           K DISK
     FWDRTP     O    E             DISK
     F                                     RENAME(PRT01:PRT02)
      * Vendor History
     FWDVHL     UF   E           K DISK
     FWDVHP     O    E             DISK
     F                                     RENAME(PVH01:PVH02)
      * Excluded States
     FWDXSL     UF   E           K DISK
     FWDXSP     O    E             DISK
     F                                     RENAME(PXS01:PXS02)
     F*** END ADD ***** 10/13/10 ******************************************
      *
      * Output File
     FQSYSPRT   O    F  132        PRINTER OFLIND(*INOA)
      *
      *
      *

     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *
     D***  Begin Add  ***  01/30/04  **************************************
     D                 DS
     D  QCKEY                  1     40
     D  ELCO#                  1      3  0
     D  ELFND                  4      6  0
     D  ELEMP#                 7     15  0
     D  ELDIV                 16     20  0

     I***  End   Add  ***  01/30/04  **************************************
      *
      *
      *=================================================================
      *  KLIST
      *=================================================================
      *
      *  Employer Detail
      *

     C     ELKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
      *  Deletion of Policy Year Related Files
      *
     C     DLTKY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
      *
     C     DLTKY2        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELFYR
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
      *
      *** BEG ADD ***** 10/13/10 ******************************************
     C     DLTKY3        KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELFYR
      *** END ADD ***** 10/13/10 ******************************************
      *
      ***  Begin Add  ***  01/30/04  **************************************
     C     EXKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     CMKY          KLIST
     C                   KFLD                    QCKEY
     C                   KFLD                    XXRMCD
     C                   KFLD                    X7CDAT
      ***  End   Add  ***  01/30/04  **************************************
      *=================================================================
      *  *LIKE DEFN
      *=================================================================
     C     *LIKE         DEFINE    ELIDT         X7DATE
     C     *LIKE         DEFINE    AHCO#         KKCO#
     C     *LIKE         DEFINE    AHFND         KKFND
     C     *LIKE         DEFINE    ELPFDT        KKDATE
      ***  Begin Add  ***  01/30/04  **************************************
     C     *LIKE         DEFINE    CMRMCD        XXRMCD
     C     *LIKE         DEFINE    CMCDAT        X7CDAT
      ***  End   Add  ***  01/30/04  **************************************
      *=================================================================
      *  PLIST
      *=================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CL              1
     C                   PARM                    @@CO#             3
     C                   PARM                    @@FND             3
     C                   PARM                    @@USER           10
     C                   PARM                    @@DATE            8
      ***  Begin Add  ***  01/30/04  **************************************
     C                   PARM                    @@DART            1
     C                   PARM                    @@CDAT            8
      ***  End   Add  ***  01/30/04  **************************************
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *=================================================================
      *  BEGIN
      *=================================================================
      *
      *  !!! ONLY RUN / CALL THIS PROGRAM FROM: FIXRENEWC !!!
      *
      *  Otherwise duplicate records will be created in LIVE data.
      *
     C     @@CL          IFNE      'C'
     C                   SETON                                        LR
     C                   RETURN
     C                   ENDIF
      *
     C                   TIME                    DSDTTM
     C                   Z-ADD     *ZERO         XXCNT             7 0
     C                   SETON                                        OA
     C                   MOVE      @@DATE        X8DATE            8 0
      *
     C                   Z-ADD     X8DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7DATE
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVE      @@CO#         KKCO#
     C                   MOVE      @@FND         KKFND
      *
     C     ELKY          CHAIN     PEL01                              60
      *
     C     *IN60         DOWEQ     *OFF
      *
     C     ELIDT         IFEQ      X7DATE
     C     ELIPGM        ANDEQ     'W7820R'
     C     ELIUSR        ANDEQ     @@USER
     C   OA              EXCEPT    HEAD
     C   OA              SETOFF                                       OA
     C                   EXCEPT    DET
     C                   EXSR      SRDEL
     C                   WRITE     PEL02
     C                   DELETE    PEL01
     C                   ADD       1             XXCNT
     C                   ENDIF                                                  ELPFDT GE DATE
      *
     C     ELKY          READE     PEL01                                  60
     C                   ENDDO
      *
     C     XXCNT         IFNE      *ZERO
     C                   EXCEPT    LRTOT
     C                   ENDIF
      *
     C                   ENDIF                                                  SRERR
      *
     C                   SETON                                        LR
      *
      *
      *
      *===================================================================
      * SRDEL - Delete policy year renewal records
      *===================================================================
     C     SRDEL         BEGSR
      *
      *  Carrier / State Reporting
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PRP01                              60
     C  N60              WRITE     PRP02
     C  N60              DELETE    PRP01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Additional Entity Location Detail
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PLD01                              60
     C  N60              WRITE     PLD02
     C  N60              DELETE    PLD01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer Agent History
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PAH01                              60
     C  N60              WRITE     PAH02
     C  N60              DELETE    PAH01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Billing Work File
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        CHAIN     PBL01                              60
     C  N60              WRITE     PBL02
     C  N60              DELETE    PBL01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer Payroll
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PEP01                              60
     C  N60              WRITE     PEP02
     C  N60              DELETE    PEP01
     C                   ENDDO                                                  *IN60=*ON
      *
      *** BEGIN ADD ***  02/27/02  ************************************
      *
      *  Employer HISTORY
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PEH01                              60
     C  N60              WRITE     PEH02
     C  N60              DELETE    PEH01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer A/R
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PEA01                              60
     C  N60              WRITE     PEA02
     C  N60              DELETE    PEA01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer DISCOUNT FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        CHAIN     PDS01                              60
     C  N60              WRITE     PDS02
     C  N60              DELETE    PDS01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer INCR LIMITS FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        CHAIN     PIL01                              60
     C  N60              WRITE     PIL02
     C  N60              DELETE    PIL01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Employer MINIMUM PREM FILE
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY2        CHAIN     PMP01                              60
     C  N60              WRITE     PMP02
     C  N60              DELETE    PMP01
     C                   ENDDO                                                  *IN60=*ON
      *
      *** END ADD *****  02/27/02  ************************************
      ***  Begin Add  ***  01/30/04  **************************************
     C     Q1PRMS        IFGE      7
      *
     C     @@DART        IFEQ      'Y'
     C     @@DART        OREQ      'y'
     C                   EXSR      DARTS
     C                   ENDIF
      *
     C                   ENDIF
      ***  End   Add  ***  01/30/04  **************************************
      *** BEG ADD ***** 10/13/10 ******************************************
      *
      *  Renewal Tracking
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        CHAIN     PRT01                              60
     C  N60              WRITE     PRT02
     C  N60              DELETE    PRT01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Excluded States
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY3        CHAIN     PXS01                              60
     C  N60              WRITE     PXS02
     C  N60              DELETE    PXS01
     C                   ENDDO                                                  *IN60=*ON
      *
      *  Excluded States
      *
     C     *IN60         DOUEQ     *ON
     C     DLTKY         CHAIN     PVH01                              60
     C  N60              WRITE     PVH02
     C  N60              DELETE    PVH01
     C                   ENDDO                                                  *IN60=*ON
      *** END ADD ***** 10/13/10 ******************************************
      *
     C                   ENDSR                                                  SRDEL
      *
      *
      ***  Begin Add  ***  01/30/04  **************************************
      *====================================================================
      * DARTS
      *====================================================================
     C     DARTS         BEGSR
      *
     C                   MOVE      @@CDAT        X8CDAT            8 0
      *
     C                   Z-ADD     X8CDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         X7CDAT
      *
     C     SRERR         IFEQ      *OFF
     C                   CLEAR                   EXPPCD
     C     EXKY          CHAIN     PEX01                              60
     C  N60              MOVEL     EXPPCD        XXRMCD
     C   60              MOVEL     *BLANKS       XXRMCD
      *
     C     CMKY          SETGT     PCM01
      *
     C     *IN60         DOUEQ     *ON
     C     CMKY          READPE    PCM01                                  60
      *
     C     CMIUSR        IFEQ      @@USER
     C     *IN60         ANDEQ     *OFF
     C  N60              WRITE     PCM02
     C  N60              DELETE    PCM01
     C                   MOVE      *ON           *IN60
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDIF                                                  X7CDAT
      *
     C                   ENDSR                                                  DARTS
      ***  End   Add  ***  01/30/04  **************************************
      *
      *
      *
     OQSYSPRT   E            HEAD           1 06
     O                       DSTIME               8 '  :  :  '
     O                       DSDATE        Y    132
      *
     O          E            HEAD           2
     O                       Q1PGM               10
     O                                           50 'Employer Renewal'
     O                                         +001 'Records Deleted'
     O                                          126 'PAGE:'
     O                       PAGE          Z    132
      *
     O          E            HEAD           3
     O                                         +000 'CO#'
     O                                         +002 'FND'
     O                                         +002 'FYR'
     O                                         +002 'EMPLOYER '
     O                                         +002 'DIV  '
      *
     O          E            DET            1
     O                       ELCO#         Z   +000
     O                       ELFND         Z   +002
     O                       ELFYR         Z   +002
     O                       ELEMP#        Z   +002
     O                       ELDIV         Z   +002
      *
     O          E            LRTOT       2  1
     O                                         +002 'TOTAL:'
     O                       XXCNT         Z   +001
      *
      *
