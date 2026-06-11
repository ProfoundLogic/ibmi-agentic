     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8053R - Employer Eligibility File Extract
     H*****************************************************************
     H*  Program Description:                                         *
     H*     Carries out the Employer Eligibility File Extract         *
     H*     Process                                                   *
     H*                                                               *
     H*  Called from: W8153C - Employer Eligibility File Extract      *
     H*  Calls to:    None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 08/07/2007 by c:62 for Project MC301              *
     H*****************************************************************
     H*  Revisions:                                                   *
      *
      * 12/16/09 C:79  135WNW
      *                For client 135 omit employers where the NCCI
      *                Carrier # = 68666
      *
      * 03/31/08 C:47  114MQN
      *                CHECK WMEMP:EMDCRX FLAG TO DETERMINE WHETHER
      *                AN EMPLOYER SHOULD BE EXCLUDED OR NOT
      *                FROM PROCESSING BASED ON THE VALUE OF THIS
      *                DCRX-EXPORT-EXCLUDE FLAG.
     H*                                                               *
     H*****************************************************************
     FWDELL     IF   E           K DISK    USROPN
     FWMEML     IF   E           K DISK    USROPN
     FFMEML     IF   E           K DISK    USROPN
     F                                     RENAME(PEM01:PEM02)
     FWMFNL     IF   E           K DISK    USROPN
     FFMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:PFN02)
     FWMRSL     IF   E           K DISK    USROPN
     FWMAHL     IF   E           K DISK    USROPN
     FWMAAL1    IF   E           K DISK    USROPN
     FWMAGL     IF   E           K DISK    USROPN
     FWAEEP2    O    E             DISK
     F*** BEG ADD *** 12/16/09   **************************************
     FWDF2L     IF   E           K DISK    USROPN
     F*** END ADD *** 12/16/09   **************************************

     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D                 DS
     D  EMP#                   1     15
     D  FND                    1      3  0
     D  FYR                    4      6  0
     D  CASE                   7     13  0
     D*
     D*****************************************************************
     D* CONSTANTS
     D*****************************************************************
     D*
     D XTITL           C                   CONST('Employer Eligibili-
     D                                     ty Summary Report')
     D DIGITS          C                   CONST('0123456789')
     D PPCDOC          C                   CONST('CH0000076WCAS99210')
      *****************************************************************

     C*   KEY LISTS
     C*****************************************************************
     C*
     C*   Agent Assignment
     C*
     C     AAKY          KLIST
     C                   KFLD                    AHCO#
     C                   KFLD                    AHFND
     C                   KFLD                    AHAGT#
     C*
     C*   Agent Master
     C*
     C     AGKY          KLIST
     C                   KFLD                    AACO#
     C                   KFLD                    AAFEIN
     C                   KFLD                    AAMOD
     C*
     C*   Agent History
     C*
     C     AHKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C                   KFLD                    KKEFDT
     C*
     C     AHKY2         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C*   Employer Master
     C*
     C     EMKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*   Group
     C*
     C     FNKY1         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*
     C     FNKY2         KLIST
     C                   KFLD                    EMCO#
     C                   KFLD                    EMSUB#
     C                   KFLD                    EMFND
     C*
     C*  Output Routing
     C*
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    KKPRT             1
     C*
     C*   Rate Set Code file
     C*
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    KKRTCD
     C*
     C*   User Parameter
     C*
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*** BEG ADD *** 12/16/09   **************************************
     C*
     C*   Group Auxiliary
     C*
     C     F2KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C*** END ADD *** 12/16/09   **************************************
     C*
     C*****************************************************************
     C*   *LIKE Define Variables
     C*****************************************************************
     C     *LIKE         DEFINE    RSRTCD        KKRTCD
     C     *LIKE         DEFINE    AHEFDT        KKEFDT
     C*****************************************************************
     C*   PARAMETER LISTS INCLUDING ENTRY PARMS
     C*****************************************************************
     C     *ENTRY        PLIST
     C                   PARM                    @@DCID           12
     C                   PARM                    @@DCI2           50
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
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
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
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*YYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*****************************************************************
     C*   Initialize
     C*****************************************************************
     C*
     C*  PGMSTR executes first
     C*
     C                   EXSR      PGMSTR
     C*
     C*  Read 1st record
     C*
     C     WCMS          IFEQ      *ON
     C                   OPEN      WMRSL
     C                   OPEN      WMFNL
     C                   OPEN      WMEML
     C                   OPEN      WDELL
     C                   OPEN      WMAHL
     C                   OPEN      WMAAL1
     C                   OPEN      WMAGL
     C*** BEG ADD *** 12/16/09   **************************************
     C                   OPEN      WDF2L
     C*** END ADD *** 12/16/09   **************************************
     C                   READ      PEL01                                  99
     C                   ELSE
     C                   OPEN      FMFNL
     C                   OPEN      FMEML
     C                   READ      PEM02                                  99
     C                   ENDIF
     C*
     C     *IN99         DOWEQ     '0'
     C                   EXSR      SUBSET
     C     MEETSS        IFEQ      *ON
     C                   ADD       1             COUNTW
     C                   EXSR      WRITER
     C                   ENDIF                                                  MEETSS
     C*
     C*  Read next record
     C*
     C     WCMS          IFEQ      *ON
     C                   READ      PEL01                                  99
     C                   ELSE
     C                   READ      PEM02                                  99
     C                   ENDIF
     C                   ENDDO                                                  WRK RECORD
     C*
     C                   EXSR      CRTDOC
     C*
     C     WCMS          IFEQ      *ON
     C                   CLOSE     WMRSL
     C                   CLOSE     WMFNL
     C                   CLOSE     WMEML
     C                   CLOSE     WDELL
     C                   ELSE
     C                   CLOSE     FMFNL
     C                   CLOSE     FMEML
     C                   ENDIF
     C*
     C*  End of Processing
     C*
     C                   SETON                                        LR
     C*****************************************************************
     C* CRTDOC - CREATE THE DOC NAME
     C*****************************************************************
     CSR   CRTDOC        BEGSR
     C*
     C     HIT           IFEQ      *ON
     C*
     C                   TIME                    DSDTTM
     C*
     C     DSTIME        IFLE      120000
     C                   CALL      'SRPAST8'
     C     DSDATE        PARM      DSDATE        ##DATE            8 0
     C                   PARM      1             ##DAYS            5 0
     C                   PARM      *BLANKS       ##ERR             1
     C                   ENDIF
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDAT7            7
     C                   MOVE      XXDAT7        XXDAT6            6
     C*
     C     'EE'          CAT(P)    XXDAT6:0      @@DCID
     C                   CAT       '.TXT':0      @@DCID
     C*
     C                   SELECT
     C     XXCLT#        WHENEQ    '135'
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXDAT6
     C                   MOVE      DSTIME        XXTIME            6
     C     PPCDOC        CAT(P)    XXDAT6:0      @@DCI2
     C                   CAT       XXTIME:0      @@DCI2
     C                   CAT       '.EMP':0      @@DCI2
     C                   OTHER
     C                   CLEAR                   @@DCI2
     C                   ENDSL
     C*
     C                   ELSE
     C                   CLEAR                   @@DCID
     C                   CLEAR                   @@DCI2
     C                   ENDIF
     C*
     C                   ENDSR
     C*****************************************************************
     C* PGMSTR - Set-up and Initialize
     C*****************************************************************
     CSR   PGMSTR        BEGSR
     C*
     C                   Z-ADD     *ZEROS        COUNTE            9 0
     C                   Z-ADD     *ZEROS        COUNTW            9 0
     C                   TIME                    DSDTTM
     C                   MOVE      *OFF          HIT               1
     C*
     C*  CALL TO SA001R TO FIND OUT WHETHER WCMS OR NOT....
     C*
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
     C                   PARM                    ##PRVB            1            PROVIDER BILL
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM      *BLANKS       XXCLT#            3
     C*
     C                   MOVE      *OFF          WCMS              1
     C     ##WCIF        IFEQ      'Y'
     C                   MOVE      *ON           WCMS
     C                   ENDIF
     C*
     C                   ENDSR                                                  PGMSTR
     C*****************************************************************
     C* SUBSET - Check if member of records processed subset
     C*****************************************************************
     CSR   SUBSET        BEGSR
     C*
     C                   MOVE      *ON           MEETSS            1
     C*
     C*  MEETSS = *ON, if to be processed
     C*
     C* IS THIS A TEST GROUP
     C*
     C     WCMS          IFEQ      *ON
     C                   CLEAR                   PFN01
     C     FNKY1         CHAIN     PFN01                              95
     C                   ELSE
     C                   CLEAR                   PFN02
     C     FNKY2         CHAIN     PFN02                              95
     C                   ENDIF
     C     *IN95         IFEQ      *OFF
     C     FNTEST        IFEQ      'Y'
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C                   ELSE
     C                   MOVEL     *OFF          MEETSS
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/16/09   **************************************
     C     WCMS          IFEQ      *ON
     C     MEETSS        ANDEQ     *ON
     C     XXCLT#        ANDEQ     '135'
     C*
     C     F2KY          CHAIN     PF201                              95
     C*
     C     *IN95         IFEQ      *OFF
     C     F2CAR#        ANDEQ     68666
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END ADD *** 12/16/09   **************************************
     C*
     C     WCMS          IFEQ      *ON
     C     MEETSS        ANDEQ     *ON
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKEFDT
     C*
     C                   CLEAR                   PAG01
     C     AHKY1         SETGT     PAH01
     C     AHKY2         READPE    PAH01                                  99
     C     *IN99         IFEQ      *OFF
     C     AAKY          CHAIN     PAA01                              99
     C     *IN99         IFEQ      *OFF
     C     AGKY          CHAIN     PAG01                              99
     C                   ENDIF
     C                   ENDIF
     C*
     C     AGEEXP        IFEQ      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
      ***** BEG ADD ***** 03/31/08 ************************************
     C     WCMS          IFEQ      *ON
     C     EMKY          CHAIN     PEM01                              99
     C     *IN99         IFEQ      *OFF
     C     EMDCRX        IFEQ      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
      ***** END ADD ***** 03/31/08 ************************************
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
     C*****************************************************************
     C* WRITER - WRITE WAEEP2
     C*****************************************************************
     CSR   WRITER        BEGSR
     C*
     C                   CLEAR                   PEE01
     C*
     C     WCMS          IFEQ      *ON
     C     EMKY          CHAIN     PEM01                              99
     C                   ENDIF
     C*
     C                   Z-ADD     EMEMP#        EEEMP#
     C                   Z-ADD     EMDIV         EEDIV
     C                   MOVEL(P)  EMNAME        EEDBA
     C                   MOVEL(P)  EMMAD1        EEMAD1
     C                   MOVEL(P)  EMMAD2        EEMAD2
     C                   MOVEL(P)  EMMCTY        EEMCTY
     C                   MOVEL(P)  EMMST         EEMST
     C                   Z-ADD     EMMZIP        EEMZIP
     C                   MOVEL(P)  EMPAD1        EEPAD1
     C                   MOVEL(P)  EMPAD2        EEPAD2
     C                   MOVEL(P)  EMPCTY        EEPCTY
     C                   MOVEL(P)  EMPST         EEPST
     C                   Z-ADD     EMPZIP        EEPZIP
     C                   Z-ADD     EMPHN#        EEPHN
     C*
     C                   Z-ADD     EMIDT         SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EEICDT
     C*
     C     WCMS          IFEQ      *ON
     C                   Z-ADD     EMCAD1        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         EECXDT
     C                   MOVE      ELFYR         EEFYR
     C                   MOVEL(P)  ELRTCD        KKRTCD
     C     RSKY          CHAIN     PRS01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      RSJRST        EERST
     C                   ENDIF
     C                   ENDIF
     C*
     C                   WRITE     PEE01
     C                   MOVE      *ON           HIT
     C*
     C                   ENDSR                                                  WRITER
