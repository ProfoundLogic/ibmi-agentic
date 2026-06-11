     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      ************************************************************************/
      * SCI110R1- PDS POLICY  KEY, IMPORT PROCESSING PROGRAM                 */
      *                                                                      */
      * Description: CALLED FROM SCI110R - PROCESSES FILE IN IFS             */
      *                                                                      */
      * Created:    12/26/06                                                 */
      * Programmer: 002                                                      */
      * Project:    OBJIMPT1                                                 */
      ************************************************************************/
      * Revisions:                                                           */
      *
      *
      * -------------------------------------------------------------------- */
      *  Note: Please add comments to the top of revisions.                  */
      * -------------------------------------------------------------------- */
      ***************************************************************  *******/
      *
     FSXFLP     IF   E           K DISK
     FSMI1P     IF   E             DISK
     F*****************************************************************
     FQSYSPRT1  O    F  198        PRINTER OFLIND(*INOA)
     F                                     USROPN
     FQSYSPRT2  O    F  198        PRINTER OFLIND(*INOB)
     F                                     USROPN
     F*  QSYSPRT1 IS THE REPROT FOR ERRORS, QSYSPRT2 IS THE REPORT FOR
     F*    SUCCESSFUL

     D FND             S              3  0 DIM(132)                             FOUND ARRAY
     D R2              S              2    DIM(20)                              RTN CODES (2)ERR
     D R3              S              1    DIM(20)                              RTN CODES (3)SUCC
      *
      *
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      *
     D                 DS
     D  XXHKEY                 1     40
     D  #$CO#                  1      3
     D  #$FND                  4      6
     D  #$EMP#                 7     15
     D  #$DIV                 16     20
     D  #$CLR                 21     40
      *
     D*
     D                 DS
     D  NWKEY                  1     40
     D  #@CO#                  1      3
     D  #@FND                  4      6
     D  #@EMP#                 7     15
     D  #@DIV                 16     20
     D*
     D*
     D                 DS
     D  @3ARMK                 1    256
     D  @3AR1                  1     75
     D  @3AR2                 76    150
     D  @3AR3                151    225
     D  @3AR4                226    256
     D*
     D @3FOBJ          DS
     D  FOBJ1                  1    256
     D  FOBJ2                257    512
     D  FOBJ3                513    768
     D  FOBJ4                769   1024
     D*
     D @3TOBJ          DS
     D  TOBJ1                  1    256
     D  TOBJ2                257    512
     D  TOBJ3                513    768
     D  TOBJ4                769   1024
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D SCI110         UDS
     D  X#DYS                  1      3
     D*
     D @VR             C                   CONST('Error validating key-
     D                                      extracted from the -
     D                                     file name.          ')
     D*
     D*
     D @ER             C                   CONST('Error copying file t-
     D                                     o the Error director-
     D                                     y.                  ')
     D*
     D*
     D*
     D*
     D DLTOVR          C                   CONST('DLTOVR FILE(SMI-
     D                                     MP)')
     D*
     D VALIFS          C                   CONST('CHKIFSOBJ')
      *
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')

     C*
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     FLKY          KLIST
     C                   KFLD                    @@TYPE           12
      *
      *
     C     DTMDY8        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     *DTAARA       DEFINE                  SCI110
      *
      *
     C     *ENTRY        PLIST
     C                   PARM                    @@RRN             9 0
     C                   PARM                    RTNCD             2
     **
     C                   CALL      'SA001R'
01   C                   PARM      *BLANKS       ##WCIF            1            WC INST
02   C                   PARM      *BLANKS       ##MCIF            1            MC INST
03   C                   PARM      *BLANKS       ##WCIN            1            WC INTERFACE
04   C                   PARM      *BLANKS       ##AR              1            MC AR INST
05   C                   PARM      *BLANKS       ##NTWK            1            NETWORK INST
06   C                   PARM      *BLANKS       ##UR              1            PRECERT INST
07   C                   PARM      *BLANKS       ##MED             1            MEDMGMT INST
08   C                   PARM      *BLANKS       ##EDI             1            EDI INST
09   C                   PARM      *BLANKS       ##WCDK            1            WC DARTS KEY
10   C                   PARM      *BLANKS       ##RHAB            1            REHAB INST
11   C                   PARM      *BLANKS       ##MSS             1            MS SUPPORT
12   C                   PARM      *BLANKS       ##FSCH            1            FEE SCHEDULE
13   C                   PARM      *BLANKS       ##PRVB            1            PROVIDER BILLS
14   C                   PARM      *BLANKS       ##NPWC            1            NON-PDS WC  LS
15   C                   PARM      *BLANKS       ##OS              1            OUTOME/SAVINGS
16   C                   PARM      *BLANKS       ##ARLS            1            Auto Release Flag
17   C                   PARM      *BLANKS       ##CCOD            1            Correct Coding Flag
18   C                   PARM      *BLANKS       ##CRVW            1            Code Review Flag
19   C                   PARM      *BLANKS       ##PROM            1            ProMatch Flag
20   C                   PARM      *BLANKS       ##MCRV            1            Medicare CR Flag
21   C                   PARM      *BLANKS       ##FREQ            1            Frequency Flag
22   C                   PARM      *BLANKS       ##CRAC            1            CR Auto Complete
23   C                   PARM      *BLANKS       ##PREO            1            CR Auto Complete
24   C                   PARM      *BLANKS       ##PARN            1            Par/Non-Par/Both
25   C                   PARM      *BLANKS       ##MGRI            1            Multi Group Restr
26   C                   PARM      *BLANKS       ##KCIF            1            Keep Claimant info
27   C                   PARM      *BLANKS       ##CURS            1            PBILLS entry cursor
28   C                   PARM      *BLANKS       ##PCIB            1            Prov Check in Batch
29   C                   PARM      *BLANKS       ##CLT#            3            Client Number
30   C                   PARM      *BLANKS       ##UCPF            1            UCR Percentile
31   C                   PARM      *BLANKS       ##DCF             1            Data Center
32   C                   PARM      *BLANKS       ##AUTB            1            Auto Batch   Flag
33   C                   PARM      *BLANKS       ##DEIF            1            DE/Image Flag
     C*
     C                   MOVEL     '*STMF   '    @@TYPE           12
     C                   MOVEL     'OI'          @@WSID            2            DFLT WORK STN ID
********* DEFAULT IS 99999, NOTING IS DELETED
     C                   MOVE      99999         #OFDAY            5 0
      *** RETRIEVE THE DATAAREA THAT HOLDS THE #OF DAYS
     C                   IN        SCI110                               12
      *** IF THE VALUE EXISTS IN THE DATAAREA, USE IT, ELSE LEAVE THE DFT VALUE
     C     X#DYS         IFNE      *BLANKS
     C     *IN12         ANDEQ     *OFF
     C                   MOVE      *ZEROS        #OFDAY
     C                   MOVE      X#DYS         #OFDAY            5 0
     C                   ENDIF
      *
     **** END ADD 08/30/05 ***************************************************
     C*
     C                   TIME                    DSDTTM
     C*
      *
      *
     C     @@RRN         CHAIN     PI101                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      '01'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
     C*
     C     REDO1         TAG
     C     FLKY          CHAIN     PFL01                              61
     C     *IN61         IFEQ      *ON
     C     @@TYPE        IFEQ      '*STMF   '
     C                   GOTO      REDO2
     C                   ENDIF
     C                   MOVE      '02'          RTNCD
     C                   GOTO      ENDIT
     C                   ENDIF
      *
     C     *IN61         DOWEQ     *OFF
      *
      ***   OPEN PRINT FILES
      *
     C     XXPRT1        IFNE      '1'
     C                   OPEN      QSYSPRT1
     C                   MOVE      '1'           XXPRT1            1
     C                   EXCEPT    HEADE
     C                   ENDIF
      *
     C     XXPRT2        IFNE      '1'
     C                   OPEN      QSYSPRT2
     C                   MOVE      '1'           XXPRT2            1
     C                   EXCEPT    HEADS
     C                   ENDIF
      *
      *** CLEAR WORK FIELDS
      *
     C                   MOVEA     *BLANKS       R3
     C                   MOVEA     *BLANKS       R2
     C                   MOVEA     *ZEROS        FND
     C                   MOVEL     *BLANKS       @@FULL
     C                   MOVE      *ZEROS        E2               30
     C                   MOVE      *ZEROS        E3               30
     C                   MOVE      *ZEROS        E4               30
     C                   MOVE      *ZEROS        S2               30
     C                   MOVE      *ZEROS        S3               30
     C                   MOVE      *ZEROS        S4               30
     C                   MOVE      *ZEROS        L2               30
     C                   MOVE      *ZEROS        L3               30
     C                   MOVE      *ZEROS        L4                3 0
     C                   MOVEA     '00000000'    *IN(90)
     C                   MOVEA     '00'          *IN(98)
     C                   MOVEL     *BLANKS       XXHKEY
     C                   MOVEL     *BLANKS       NWKEY
     C                   MOVEL     *BLANKS       XXHDT             8
     C                   MOVEL     *BLANKS       XXHTM             6
     C                   MOVEL     *BLANKS       XXHDSC          132
      *** ADDTIONAL VARIABLES
     C                   MOVEL     *BLANKS       INRD              8
     C                   MOVEL     *BLANKS       INDS             40
     C                   MOVEL     *BLANKS       INDD              8
     C                   MOVEL     *BLANKS       INRC              5
     C                   MOVEL     *BLANKS       INDU             10
     C                   MOVEL     *BLANKS       @@TXCD            2
     C                   MOVEL     *BLANKS       @@OPCD            2
     C                   MOVEL     *BLANKS       @@IPCD            2
     C                   MOVEL     *BLANKS       @@ADCD            2
     C                   MOVEL     *BLANKS       @@NDCD            2
     C                   MOVEL     *BLANKS       @@A2CD            2
      *
      *
     C                   EXSR      PRCFIL
      *
     C     FLTAG         TAG
      ***   LOOP TO NEXT SXFL RECORD
     C     FLKY          READE     PFL01                                  61
      *
     C                   ENDDO
      *
     C     @@TYPE        IFEQ      '*STMF   '
     C     REDO2         TAG
     C                   MOVEL     '*DSTMF  '    @@TYPE
     C                   GOTO      REDO1
     C                   ENDIF
     C*
      *
      * End the program
     C     ENDIT         TAG
     C                   MOVE      *ON           *INLR
      *
      *===================* END OF MAINLINE CODE *====================*
      *
      *****************************************************************
      * * *     SUBROUTINE - PRCFIL - PROCESS FILES FROM IFS      * * *
      *****************************************************************
      *
     C     PRCFIL        BEGSR
      *
      *
      ***   FOR EACH RCD,
      ***     SUBROUTINE TO EXTRACT DATE STAMP FROM FILE NAME
      *** MOVEL INTO 6 BYTE FIELD, VALIDATE THE DATE IN THIS FIELD (SB MDY)
      ***  CHECK TODAY'S DATE AGAINST THIS DATE TO SEE IF IT IS WITHIN THE
      ***   #OFDAYS VALUE (KEEPIT), ELSE DELETE IT
      ***
      *** EXAMPLE NAME :
      ***
      ** 1208061845177770051060003686 PROGRESS SUMMARY REPORT DATED 12-07-06.PDF
      ** ------
      ***
      *
     C                   MOVEL     FLOBJN        @@FULL          132
     C                   MOVEL     @@FULL        XXDAT1            6            EXTRACT DATE
     C*
     C* VALIDATE THE DATE, THEN MOVE TO
     C                   MOVE      *ZEROS        SRDTI
     C                   MOVE      *ZEROS        XX8CH             8
     C                   MOVEL     XXDAT1        XX8CH
     C                   TESTN                   XX8CH                21
     C*
     C     *IN21         IFEQ      *ON                                                         *IN21
     C                   MOVE      XXDAT1        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C*** ERROR ON DATE CONVERSION?  (BAD DATE?)
     C     SRERR         IFEQ      '0'                                                     SRERR
     C                   MOVEL     SRDTO         XXFROM            8 0
     C                   ELSE
     C*** ERROR OCCURED *** PUT ITEM ON ERROR LOG -CAN'T CLEANUP-
      ***        NOT VALID
     **
     C     *INOA         IFEQ      *ON                                                      *INOA
     C                   EXCEPT    HEADE
     C                   ENDIF                                                            *INOA
     C*
     C                   EXCEPT    ERR1
     C                   EXCEPT    ERR1A
     C                   GOTO      EPRCS
     C*
     C                   ENDIF                                                         SRERR
     C*
     C                   ELSE                                                            *IN21
     C*
     *** ERROR - DATA NOT NUMERIC
      ***        NOT VALID
     **
     C     *INOA         IFEQ      *ON                                               *INOA
     C                   EXCEPT    HEADE
     C                   ENDIF                                                       (*INOA
     C*
     C                   EXCEPT    ERR1
     C                   EXCEPT    ERR1D
     C                   GOTO      EPRCS
     C*
     C                   ENDIF                                                     *IN21
     C*
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY8
     C                   MOVEL     SRDTO         XXTO              8 0
     C*
     C                   MOVE      XXFROM        QQFROM            8 0
     C                   MOVE      XXTO          QQTO              8 0
     C                   Z-ADD     *ZERO         QQDAYS            5 0
     C                   MOVE      *BLANK        QQERR             1
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRDDIF8'
     C                   PARM                    QQFROM
     C                   PARM                    QQTO
     C                   PARM                    QQDAYS
     C                   PARM                    QQERR
     C                   PARM                    QQTERM
     C*** ERROR ON DATE DIFF ROUTINE (BAD DATE?)
     C     QQERR         IFNE      '0'
     C*** ERROR OCCURED *** PUT ITEM ON ERROR LOG -CAN'T CLEANUP-
      ***        NOT VALID
     **
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
     C*
     C                   EXCEPT    ERR1
     C                   EXCEPT    ERR1B
     C                   GOTO      EPRCS
     C*
     C                   ENDIF
     C*  IF # DAYS IS <= # DAYS SINCE FILE CREATED
     C*     VALUE IS PURGED.
     C*
     C     QQDAYS        IFGE      #OFDAY
     C*** VALID FOR DELETION * DELETE AND RECORD ****
     **
     **
     C     *INOB         IFEQ      *ON
     C                   EXCEPT    HEADS
     C                   ENDIF
     C*
     C                   EXCEPT    SUCCS1
     **
     ** DELETE THE FILE FROM THE ARCHIVE DIRECTORY
     C                   CALL      'DELOBJC'
     C                   PARM      I1ADIR        @DIR             80
     C                   PARM      FLOBJN        @OBJN           132
     C                   PARM      *BLANKS       RCD               2
     C*
     C     RCD           IFNE      *BLANKS
     C*
     C     *INOA         IFEQ      *ON
     C                   EXCEPT    HEADE
     C                   ENDIF
     C*
     C                   EXCEPT    ERR1
     C                   EXCEPT    ERR1C
     C                   GOTO      EPRCS
     C*
     C*
     C                   ENDIF
     **
     C                   ELSE
     *** NO DELETION, SIMPLY BYPASS
     C*
     C                   ENDIF
     C*
     C     EPRCS         ENDSR
      *
     O*
     OQSYSPRT1  E            HEADE          1 06
     O                                          111 'OBJIMPT CLEANUP ERRORS  '
     O                       DSDATE        Y     12
     O                       DSTIME             196 '  :  :  '
     O          E            HEADE          3
     O                                           14 'DIRECTORY:'
     O                       I1ADIR              95
     O*
      *
     O          E            HEADE          1
     O                                           10 'FILE NAME'
     O*
     O          E            ERR1           1
     O                       FLOBJN             133
     O          E            ERR1A          1
     O                                            8 '     ***'
     O                                           11 '1A'
     O                                           12 '-'
     O                                           30 'DATE CONVERT ERROR'
     O          E            ERR1B          1
     O                                            8 '     ***'
     O                                           11 '1B'
     O                                           12 '-'
     O                                           34 'DATE DIFFERENCE ERROR'
     O          E            ERR1C          1
     O                                            8 '     ***'
     O                                           11 '1C'
     O                                           12 '-'
     O                                           34 'CAN NOT DELETE FILE *'
     O          E            ERR1D          1
     O                                            8 '     ***'
     O                                           11 '1D'
     O                                           12 '-'
     O                                           35 'DATE NOT IN FILE NAME'
     OQSYSPRT2  E            HEADS          1 06
     O                                          111 'OBJIMPT CLEANUP DELETED '
     O                       DSDATE        Y     12
     O                       DSTIME             196 '  :  :  '
     O          E            HEADS          3
     O                                           14 'DIRECTORY:'
     O                       I1ADIR              95
     O*
      *
     O          E            HEADS          1
     O                                           10 'FILE NAME'
     O          E            SUCCS1         1
     O                       FLOBJN             133
