     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H* TITLE APB041 - PRINT ACCOUNTS PAYABLE CHECKS (Laser MICR)     *
     H*****************************************************************
     H*  DESCRIPTION:                                                 *
     H*                                                               *
     H*    Print AP Checks using PCL5 commands (laser printer).       *
     H*    Bank MICR and Signature data is obtained from the Bank     *
     H*    Information file.                                          *
     H*                                                               *
     H*  CALLED FROM: APB041C                                         *
     H*  CALLS TO   : NONE                                            *
     H*                                                               *
     H*  CREATED:  12/08/97  C:30  Project: WC500                     *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/30/03 C:15 Project: WC1133                                 *
     H*               Moved the bank name and added the bank address. *
     H*                                                               *
     H*  10/13/00 C:15  Project: WC577                                *
     H*                -Added the fund name to the check stub.        *
     H*                -Fixed the condition which prints the second   *
     H*                 signature line.                               *
     H*                                                               *
     H*  12/14/98 C:15  Project:                                      *
     H*                 Changed the filecopies to pull paper from the *
     H*                 second drawer of the printer.                 *
     H*                                                               *
     H*  10/29/98 C:15  Project:                                      *
     H*                 Lowered the routing upper and lower numbers.  *
     H*                                                               *
     H* 02/02/98  47  PROJECT: 91WBN                                  *
     H*           PRINT GROUP (FUND) NAME/ADDRESS IN UPPER LEFT       *
     H*           CORNER OF CHECK AND FILECOPY.  THIS IS A FREE FORM  *
     H*           ENTRY IN THE BANK INFO MAINT IF BILOGO = N.         *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*    INDICATOR            FUNCTION DESCRIPTION                  *
     H*                                                               *
     H*       OF      OVERFLOW FOR PRINTING FOR REPORT                *
     H*                                                               *
     H*       08      BATCH MODE                                      *
     H*       09      DEMAND MODE                                     *
     H*       50      READ/DOWEQ OF OPEN QUERY REMARKS DETAIL FILE    *
     H*       60      GENERAL USAGE (CHAIN)                           *
      *****************************************************************
     FAPCSL3    UF   E           K DISK
     FAPCSL2    IF   E           K DISK
     F                                     RENAME(PCS01:PCS02)
     FAPBKL1    UF   E           K DISK
     FAPADL1    IF   E           K DISK
     FAPVML1    IF   E           K DISK
     FBKCRL2    UF A E           K DISK
     FAPBIL1    IF   E           K DISK
0008  *
     FCHECKS    O    F  145        PRINTER INFDS(PRTDS)
     F                                     OFLIND(*INOF)
     FFILECOPY  O    F  145        PRINTER OFLIND(*INOA)
     F                                     INFDS(PIDS01)
0008  *

0009 D*                         **************************************
0010 D*                         * NAME CONCATENATION ARRAYS          *
0011 D*                         **************************************
     D SIGMS1          S              4    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 1
     D SIGMS2          S             26    DIM(1) CTDATA PERRCD(1)              SIGNATURE MSG PT 2
     D*
     D                 DS
     D  XXTRAN                 1     11
     D  XXAMPR                 1      1
     D  BITRNS                 2      5
     D  BIBNK#                 6      9
     D  BICKDG                10     10
     D  XXAMP2                11     11
     D                 DS
     D  XXCNBR                 1     10
     D  XXSLSH                 1      1
     D  XXCHK#                 2      8  0
     D  XXSLS2                 9      9
     D  XXBLK1                10     10
     D                 DS
     D  VMCONT                 1     40
     D  XXAGNT                14     18
     D*                             ************************************
0141 D*                         * DEFINE THE DATA STRUCTURE USED    *
0142 D*                         * FOR NAME CONCATENATION.           *
0143 D*                         *************************************
0144 D                 DS
0145 D  XXLNAM                 1     38
0146 D  XXL                    1     38
     D                                     DIM(38)
0147 D  XXFNAM                39     76
0148 D  XXF                   39     76
     D                                     DIM(38)
0149 D  XXCNAM                77    152
0150 D  XXCN                  77    152
     D                                     DIM(76)
     F*
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      4  0
     D  DSDATE                 7     12  0
0143 D*
0144 D                 DS
0145 D  XXACCT                 1      9  0
0146 D  XXGLD                  1      3  0
0147 D  XXACT                  4      6  0
0148 D  XXSUB                  7      9  0
     D                 DS
     D  DSINV#                 1     13
     D  DSFND                  1      3
     D  DSFYR                  4      5
     D  DSEMP                  6     10
     D  DSDIV                 11     13
     D PRTDS           DS
     D  FILL1                  1    200
     D  FILL2                201    366
     D  XXLINE               367    368B 0
     D  XXPAGE               369    372B 0
      *
     D XCAPC           C                   CONST('ACCOUNTS PAYABLE -
     D                                     CHECK')
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D PIDS01        E DS                  EXTNAME(SZQ4P)

0144 IPCS02
0145 I              CSCO#                       C2CO#
     I              CSFAC#                      C2FAC#
     I              CSVDR                       C2VDR
     I              CSINV#                      C2INV#
     I              CSSEQ1                      C2SEQ1
     I              CSBNK#                      C2BNK#
     I              CSAMT                       C2AMT
     I              CSDISC                      C2DISC
     I              CSPODS                      C2PODS
     I              CSPART                      C2PART
     I              CSCHK#                      C2CHK#
     I              CSCHKD                      C2CHKD
     I              CSSEP                       C2SEP
     I              CSIPGM                      C2IPGM
     I              CSCTIM                      C2CTIM
     I              CSCDT                       C2CDT
     I              CSCUSR                      C2CUSR
     I              CSFUSR                      C2FUSR
     I              CSITIM                      C2ITIM
     I              CSIDT                       C2IDT
     I              CSSTAT                      C2STAT
      *
      *   KEY LIST FOR THE CHECK RECONCILIATION FILE
      *

     C     CRKEY         KLIST
     C                   KFLD                    XXAPP             2
     C                   KFLD                    XXBNK
     C                   KFLD                    SCCHK#
      *
      *   KEY LIST FOR THE VOUCHER FILE
      *
     C     ADKEY         KLIST
     C                   KFLD                    CSCO#
     C                   KFLD                    CSFAC#
     C                   KFLD                    CSVDR
     C                   KFLD                    CSSEQ1
      *
     C     A2KEY         KLIST
     C                   KFLD                    C2CO#
     C                   KFLD                    C2FAC#
     C                   KFLD                    C2VDR
     C                   KFLD                    C2SEQ1
      *
      *   KEY LIST FOR THE VOUCHER FILE
      *
     C     X2KEY         KLIST
     C                   KFLD                    X2BNK             3 0
     C                   KFLD                    X2CHK#            7 0
      *
      *  Bank Information file
      *
     C     BIKY1         KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXAPBK
     C                   KFLD                    XXEFDT
      *
     C     BIKY2         KLIST
     C                   KFLD                    XXFND
     C                   KFLD                    XXAPBK
      *
      *   DEFINE THE PARM LIST.
      *
     C     *ENTRY        PLIST
     C                   PARM                    XXBNK             3 0
     C                   PARM                    QQDATE            8
     C                   PARM                    XXCHK1
     C                   PARM                    XXVOID            1
     C                   PARM                    QQMODE            1
     C                   PARM                    QQFND             3
     C                   PARM                    QQACCT           20
      *
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTMDY         PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*MDY'        SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *  FIRST CYCLE - INITIALIZE WORK FIELDS
      *
     C     *LIKE         DEFINE    CSCHK#        XXCHK1
     C     *LIKE         DEFINE    CSCHK#        SCCHK#
     C     *LIKE         DEFINE    BIFND         XXFND
     C     *LIKE         DEFINE    BIAPBK        XXAPBK
     C     *LIKE         DEFINE    BIEFDT        XXEFDT
      *
     C                   TIME                    DSDTTM
     C                   MOVE      'AP'          XXAPP
     C                   MOVE      'AP'          XXTYP             2
     C                   MOVE      *ZEROS        XXGLD
     C                   MOVE      *ZEROS        XXACT
     C                   MOVE      *ZEROS        XXSUB
     C                   MOVE      '/'           XXSLSH            1
     C                   MOVE      '/'           XXSLS2            1
     C                   MOVE      '&'           XXAMPR            1
     C                   MOVE      '&'           XXAMP2            1
     C                   MOVE      *BLANKS       XXBLK2            2
     C                   MOVE      *BLANKS       XXBLK1            1
      *
     C                   MOVE      QQDATE        XXDT8             8 0
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXCHKD            6 0
      *
      *
      ****** PERFORM CVTHEX TO GET ATRN VALUE TO PASS TO THE PRINTER **
      *****                                                          **
     C                   MOVEL     '03011B'      CHR             256
     C                   MOVE      *BLANKS       HEX             128
     C                   MOVE      *BLANK        RTCODE            1
     C                   CALL      'CVTHEX'
     C                   PARM                    CHR
     C                   PARM                    HEX
     C                   PARM                    RTCODE
     C                   MOVEL     HEX           ESC               3
      *
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
      *
      *  ASSIGN FIRST CHECK NUMBER
      *  Note:  The Starting Check # was assigned in API040R from the
      *         WCMS Bank Account Master (WSCKP).  No adjustments nor
      *         update is necessary at this time.
      *
     C     XXBNK         CHAIN     PBK01                              60
      *
     C*********************ADD  1         BKLCHK
     C*********************UPDATPBK01
      *
     C                   MOVE      BKLCHK        XXCHK#
      *
      *** POSITION IN FILE
      *
     C     XXBNK         SETLL     PCS01
      *
      *** BEGIN READING RECORDS
      *
     C     READ          TAG
     C     XXBNK         READE     PCS01                                  50
      *
     C     *IN50         IFEQ      '0'
      *
      *** FIRST CHECK FOR RESTART
      *
     C     XXCHK1        IFGT      *ZERO
     C     CSCHK#        ANDNE     *ZERO
     C     CSCHK#        IFLT      XXCHK1
     C                   GOTO      READ
     C                   ELSE
     C                   MOVE      CSCHK#        SCCHK#
     C     CRKEY         CHAIN     PCR01                              60
     C     *IN60         IFEQ      '0'
     C     XXVOID        IFEQ      'Y'
     C                   MOVE      'V'           CRCLR
     C                   MOVE      Q1USER        CRCUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C                   Z-ADD     DSTIME        CRCTIM
     C                   UPDATE    PCR01
     C                   ELSE
     C                   DELETE    PCR01
     C                   END
     C                   END
     C                   END
     C                   END
      *****************************************************************
      *
      *  IF SEPARATE CHECK, FORCE LEVEL BREAK
      *
     C     CSSEP         IFEQ      'Y'
     C                   MOVE      *ZEROS        XXVDR
     C                   END
      *
      *  LEVEL BREAK ON VENDOR - OUTPUT LAST CHECK
      *
     C     CSVDR         IFNE      XXVDR
      *
     C     FIRST         IFEQ      'Y'
     C     VMHOLD        ANDNE     'Y'
     C                   EXSR      PRINT
     C                   END
      *
     C                   MOVE      'Y'           FIRST             1
     C                   MOVE      CSVDR         XXVDR             7 0
     C                   MOVE      *BLANKS       XXADD3           30
     C     CSVDR         CHAIN     PVM01                              98
     C   98              MOVE      *BLANKS       VMNAME
     C   98              MOVEL     '*INVALID'    VMNAME
     C   98              MOVE      *BLANKS       VMADR1
     C   98              MOVE      *BLANKS       VMADR2
     C   98              MOVE      *BLANKS       VMCITY
     C   98              MOVE      *BLANKS       VMST
     C   98              MOVE      *BLANKS       VMZIP1
     C   98              MOVE      *BLANKS       VMZIP2
     C   98              MOVE      *BLANKS       VMHOLD
      *
     C                   MOVEL     VMNAME        XXNAME           30
     C                   MOVEL     VMADR1        XXADD1           30
     C                   MOVEL     VMADR2        XXADD2           30
     C                   MOVEL     VMCITY        XXLNAM
     C                   MOVE      VMGLD#        XXGLD
     C                   MOVE      VMACC#        XXACT
     C                   MOVE      VMSUB#        XXSUB
     C                   MOVE      *BLANKS       XXFNAM
     C                   MOVEL     VMST          XXST              8
     C                   MOVE      VMZIP1        XXST
     C     VMZIP2        IFGT      *BLANKS
     C                   MOVEL     '-'           XXZIP             5
     C                   MOVE      VMZIP2        XXZIP
     C                   MOVEL     XXST          XXST13           13
     C                   MOVE      XXZIP         XXST13
     C                   MOVEL     XXST13        XXFNAM
     C                   ELSE
     C                   MOVEL     XXST          XXFNAM
     C                   END
     C                   EXSR      AUFLN
     C     VMADR2        IFGT      *BLANKS
     C                   MOVEL     VMADR2        XXADD2
     C                   MOVEL     XXCNAM        XXADD3           30
     C                   ELSE
     C                   MOVEL     XXCNAM        XXADD2
     C                   END
      *
      *****                EXCPTHEAD
      *
     C                   END
      *
      *  IF VENDOR IS ON HOLD, DO NOT PRINT CHECKS
      *
     C     VMHOLD        IFEQ      'Y'
     C     CSCHK#        IFGT      *ZERO                                        RE-PRINT
     C                   MOVE      *ZERO         CSCHK#
     C                   MOVE      *ZERO         CSCHKD
     C                   UPDATE    PCS01
     C                   END
     C                   GOTO      READ
     C                   END
      *
      *  IF SEPARATE CHECK, FORCE LEVEL BREAK FOR NEXT CHECK
      *
     C     CSSEP         IFEQ      'Y'
     C                   MOVE      *ZEROS        XXVDR
     C                   END
      *
      *  PRINT STUB DETAIL
      *
     C     CSAMT         SUB       CSDISC        XXNET            11 2
     C                   ADD       XXNET         XXCHKA           11 2
      *
      *
      *  UPDATE DETAIL RECORD
      *
     C                   Z-ADD     XXCHK#        CSCHK#
      *
     C                   MOVE      QQDATE        XXDT8             8 0
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CSCHKD
     C                   UPDATE    PCS01
      *
      *  WRITE CHECK RECONCILIATION
      *
     C                   MOVE      'AP'          XXAPP
     C                   Z-ADD     XXCHK#        SCCHK#
     C     CRKEY         CHAIN     PCR01                              40
      *
     C                   MOVE      'AP'          CRAPP
     C                   MOVE      CSVDR         CRVDR
     C                   MOVE      XXBNK         CRBNK#
     C                   MOVE      CSCO#         CRCO#
     C                   MOVE      CSFAC#        CRFAC#
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCHKD
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     *ZERO         CRSS#
     C                   MOVE      *BLANK        CRCLR
     C                   Z-ADD     XXCHK#        CRCHK#
      *
     C                   MOVE      Q1USER        CRCUSR
     C                   MOVE      Q1PGM         CRIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRCDT
     C                   Z-ADD     DSTIME        CRCTIM
      *
     C     *IN40         IFEQ      '0'
     C                   UPDATE    PCR01
     C                   ELSE
     C                   MOVE      Q1USER        CRFUSR
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CRIDT
     C                   Z-ADD     DSTIME        CRITIM
     C                   WRITE     PCR01
     C                   END
      *
      *  READ NEXT RECORD
      *
      *** BEGIN DEL *** 12/08/97 *****************************************
     C*********************MOVE '1'       *IN77
      *** END   DEL *** 12/08/97 *****************************************
      *
     C                   GOTO      READ
     C                   END                                                    *IN50 = 0
      *
      *** PRINT LAST CHECK
      *
     C     XXCHKA        IFNE      *ZERO
     C                   EXSR      PRINT
     C                   END
      *
      *  BACK OUT UNUSED CHECK NUMBER
      *
     C     XXBNK         CHAIN     PBK01                              60
     C                   SUB       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
      *
      *** END PROGRAM
      *
     C     TERM          TAG
     C                   MOVE      '1'           *INLR
      *
     C/COPY AUFLN
      *************************************************************************
      * PRINT CHECK
      *************************************************************************
      *
     C     PRINT         BEGSR
      *
      *  DO NOT WRITE CHECK IF AMOUNT IS LESS/EQUAL ZERO
      *
     C     XXCHKA        IFLE      *ZERO
     C                   EXSR      VOID
     C                   GOTO      NOCHK
     C                   END
      *
      *
      *  GET CHECK AMOUNT IN WORDS
      *
     C                   Z-ADD     XXCHKA        CRAMT
     C                   Z-ADD     XXCHKA        DOLLAR            9 0
     C                   MOVE      XXCHKA        CENTS             2 0
     C                   Z-ADD     XXCHKA        XXAMT#            9 2
     C                   MOVE      XXCHKA        QQAMT             9
     C                   MOVE      *BLANKS       QQAMT1           60
     C                   MOVE      *BLANKS       QQAMT2           60
     C                   MOVE      '1'           QQTERM            1
     C                   CALL      'SRAMNT'
     C                   PARM                    QQAMT
     C                   PARM                    QQAMT1
     C                   PARM                    QQAMT2
     C                   PARM                    QQTERM
     C                   MOVE      *BLANKS       XXAMT1           60
     C                   MOVE      *BLANKS       XXAMT2           60
     C                   MOVE      QQAMT1        XXAMT1
     C                   MOVE      QQAMT2        XXAMT2
      *                    EXSR WORDS
      *
      **** GET BANK INFORMATION TO PRINT ON CHECK ****
      *
     C                   MOVE      QQFND         XXFND
     C                   MOVEL     QQACCT        XXACCT
     C                   MOVEL     XXBNK         XXAPBK
      *
     C                   Z-ADD     XXDT8         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEFDT
      *
     C     BIKY1         SETLL     PBI01
     C     BIKY2         READE     PBI01                                  76
      *
     C     *IN76         DOWEQ     *OFF
     C     BISTAT        ANDEQ     'I'
     C     BIKY2         READE     PBI01                                  76
     C                   ENDDO
      *
     C     *IN76         IFEQ      '1'
     C                   GOTO      TERM
     C                   END
      *
     C**N73*73*************MOVE '1'       *IN73
      *
      *  Assign MACRO to PCL5 control characters with Bank Information Macro ID
      *
     C                   MOVE      BIMACR        TEMP4             4
     C     '&f'          CAT       TEMP4         TEMP6             6
     C     TEMP6         CAT       'y3X'         MACRO             9
      *
     C     BIAXOU        IFEQ      *BLANK
     C                   MOVE      *BLANKS       XXAXOU
     C                   MOVE      XXCNBR        XXAXOU           20
     C                   ELSE
     C                   MOVE      BIAXOU        XXAXOU
     C                   END
      *
     C     BIBNK#        CAT(P)    '/'           XXBT#5            5
     C     XXBT#5        CAT(P)    BITRNS        XXBT#             9
      *
     C*** BEGIN DLT *** 10/13/00 **************************************
      * CHECK FOR AN AMOUNT IN BI$LMT
     C********** BI$LMT    IFGT 0
     C**********           MOVE '1'       *IN85
     C**********           END
     C*** END DLT ***** 10/13/00 **************************************
      *
      *  Check LOGO indicator assigned
      *
     C                   MOVE      '0'           *IN73
     C     BILOGO        IFEQ      'N'
     C                   MOVE      '1'           *IN73
     C                   ENDIF
      *
      *  Determine if Second Signature Verbage should Print
      *
     C                   MOVE      *OFF          *IN85
     C                   MOVE      *OFF          *IN83
     C     BI$LMT        IFGT      0
     C*** BEGIN ADD *** 10/13/00 **************************************
     C     XXAMT#        ANDGT     BI$LMT
     C*** END ADD ***** 10/13/00 **************************************
     C                   MOVE      *ON           *IN85
     C                   MOVE      *ON           *IN83
     C                   ENDIF
      *
      * Check for First Signature entry
      *
     C     BI10A1        IFNE      *BLANK
     C                   MOVEL     BI10A1        CH1               7
     C                   MOVE      '1'           *IN86
     C                   ENDIF
      *
      * Check for Second Signature entry
      *
     C                   MOVE      *OFF          *IN87
     C     BI10A2        IFNE      *BLANK
     C                   MOVEL     BI10A2        CH2               7
     C                   MOVE      *ON           *IN87
     C                   MOVE      *ON           *IN83
     C                   ENDIF
      *
      *  PRINT CHECK
      *
      *
     C  N77              EXCEPT    BANNER
     C  N77              EXCEPT    BANNEF
      *
     C                   EXCEPT    CHECK
     C                   EXCEPT    CHECKF
      *
      *** BEGIN ADD *** 12/08/97 *******************************************
     C                   MOVE      '1'           *IN77
      *** END   ADD *** 12/08/97 *******************************************
      *
      *  PRINT STUB DETAIL
      *
     C                   MOVE      XXBNK         X2BNK
     C                   MOVE      XXCHK#        X2CHK#
     C     X2KEY         SETLL     PCS02
     C                   Z-ADD     *ZERO         COUNT             5 0
     C     OVER2         TAG
     C     X2KEY         READE     PCS02                                  60
     C   60              GOTO      DONE2
     C     A2KEY         CHAIN     PAD01                              60
     C   60              GOTO      OVER2
     C                   ADD       1             COUNT             5 0
     C     C2AMT         SUB       C2DISC        XXNET            11 2
     C                   Z-ADD     ADINVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDATE            8 0
     C     ADSEQ2        IFGT      1
     C                   MOVE      '1'           *IN68
     C                   MOVE      C2INV#        DSINV#
     C                   END
     C*
     C*** BEGIN ADD *** 10/13/00 **************************************
     C*  Retrieve the WCMS company #.
     C*
     C                   CALL      'WA001R'
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##NAME           40
     C*
     C*  Retrieve the WCMS Fund name.
     C*
     C                   MOVE      DSFND         ##FND
     C                   CALL      'WA038R'
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##RTCD            1
     C                   PARM      *BLANKS       ##FNAM           60
     C                   PARM      *BLANKS       ##SHNM           30
     C*** END ADD ***** 10/13/00 **************************************
     C*
     C     STBFLG        IFNE      'Y'
     C                   EXCEPT    DETH
     C                   EXCEPT    DETG
     C                   MOVE      'Y'           STBFLG            1
     C                   END
      *
     C                   EXCEPT    DET
     C                   EXCEPT    DETF
      *
     C                   MOVE      '0'           *IN68
     C     COUNT         IFLT      12
     C                   GOTO      OVER2
     C                   ELSE
     C                   EXSR      VOID
     C                   GOTO      OVER2
     C                   END
      *
     C     DONE2         TAG
     C                   MOVE      *BLANK        STBFLG
      *
      *  ASSIGN NEXT CHECK NUMBER
      *
     C     XXBNK         CHAIN     PBK01                              60
     C                   ADD       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
      *
      *  CLEAR TOTALS
      *
     C     NOCHK         TAG
     C                   Z-ADD     *ZERO         XXCHKA
     C                   Z-ADD     *ZERO         COUNT
      *
     C                   ENDSR
      *************************************************************************
      * VOID CHECK
      *************************************************************************
      *
     C     VOID          BEGSR
      *
     C     XXCHKA        IFLE      *ZERO
     C                   Z-ADD     *ZERO         XXCHKA
     C                   END
      *****                EXCPTPVOID
      *****                EXCPTHEAD
      *
      *  ASSIGN NEXT CHECK NUMBER
      *
     C     XXBNK         CHAIN     PBK01                              60
     C                   ADD       1             BKLCHK
     C                   UPDATE    PBK01
     C                   MOVE      BKLCHK        XXCHK#
      *
     C                   Z-ADD     *ZERO         COUNT
     C                   ENDSR
      *
     OCHECKS    E   N77      BANNER      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O                                              '&l1H'
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'CHECKS FOR BANK: '
     O                       XXBNK
     OCHECKS    E            CHECK       0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O                                              '&l1H'
     O                       ESC
     O                       MACRO
      ********************************************************************
     O**** MICR BANK INFO *****
     O*
     O*                                        MICR Font Change
     O          E            CHECK       0  0
     O                       ESC
     O                                              '(8M'
     O                       ESC
     O                                              '(s0p8h8v0s0b80T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p20x980Y'
     O                       XXAXOU
     O*
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p770x980Y'
     O                       XXTRAN
     O*
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1185x980Y'
     O                       BIONUS
     O*
      ********************************************************************
     O          E            CHECK       0  0
      *                                                Font Change
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O*** BEGIN CHG *** 10/28/98 **************************************
     O************************************* '*p2250x100Y'
     O                                              '*p2250x120Y'
     O*** END CHG ***** 10/28/98 **************************************
     O                       ESC
     O                                              '&d1D'
     O                       BIRTUP
     O                       ESC
     O                                              '&d@'
     O          E            CHECK       0  0
      *                                                  FUNKY
     O                       ESC
     O*** BEGIN CHG *** 10/28/98 **************************************
     O************************************* '*p2250x135Y'
     O                                              '*p2250x155Y'
     O*** END CHG ***** 10/28/98 **************************************
     O                       BIRTLW
     O          E            CHECK       0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p1850x174Y'
     O                       XXCHK#
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00             CHECK
      *** Font 159 Times Roman Med 12pt
     O***                      ESC
     O***                                   '(0U'
     O***                      ESC
     O***                                   '(s1p12vsb5T'
      *** END DEL *** 02/02/98 ****************************************
      *** BEG ADD *** 02/02/98 ****************************************
     O          E            CHECK       0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
      *** END ADD *** 02/02/98 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00     73      CHECK
      ***                                              ACCOUNT NAME
     O***                      ESC
     O***                                   '*p125x200Y'
     O***                      BICKH1
      *** END DEL *** 02/02/98 ****************************************
      *** BEG ADD *** 02/02/98 ****************************************
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECK       0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECK       0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      *** END ADD *** 02/02/98 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG ADD *** 02/02/98 ****************************************
     O          E            CHECK       0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *** END ADD *** 02/02/98 ****************************************
     O          E            CHECK       0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x174Y'
     O                       XXCHKD        Y
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     O***                      ESC
     O***                                   '*p1850x312Y'
     O***                      PACCT
     O          E            CHECK       0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00     73      CHECK
      ***                                              TEXT
     O***                      ESC
     O***                                   '*p125x260Y'
     O***                      XCAPC
      *** END DEL *** 02/02/98 ****************************************
     O          E            CHECK       0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECK       0  0
      *                                                BANK NAME
     O                       ESC
     O*** Begin chg *** 06/30/03 **************************************
     O*********************************     '*p350x600Y'
     O                                              '*p1700x225Y'
     O*** End chg ***** 06/30/03 **************************************
     O                       BIAI01
     O*** Begin add *** 06/30/03 **************************************
     O          E            CHECK       0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1700x260Y'
     O                       BIAI02
      *
     O          E            CHECK       0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1700x295Y'
     O                       BIAI03
     O*** End add ***** 06/30/03 **************************************
     O*********************************************************************
     O          E            CHECK       0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       XXAMT#                 '  ,   , $0.  '
     O*
     O          E            CHECK       0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECK       0  0
      *  Courier 10, using 12 point, 10 pitch  "Bitmapped" font
     O                       ESC
     O                                              '(8U'
     O                       ESC
     O                                              '(s0p10h12v0s0b3T'
      *                                           CHECK NAME/ADDRESS
     O                       ESC
     O                                              '*p250x650Y'
     O                       XXNAME
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       XXADD1
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x750Y'
     O                       XXADD2
     O*
     O          E            CHECK       0  0
     O                       ESC
     O                                              '*p250x800Y'
     O                       XXADD3
      *
      *  Print underline for Second Signature in 2 situations:
      *     1) Second Signature is entered.
      *     2) Zero < $$$ Limit < Check Amount.
      *
     O          E    83      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                                              '(s0p2.0h71.94v0s0b255T'
     O                                              '!"#$%&'''
      *
      *********************************************************************
      *  Signatures and $$$ limit
      *
     O          E    85      CHECK       0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*       E 00     85      CHECK
     O                       ESC
     O                                              '*p1400x605Y'
     O*       E 00     85      CHECK
     O                       SIGMS1
     O                       BI$LMT                 ' ,   ,   , $0'
     O                       SIGMS2
      *
     O          E    87      CHECK       0  0
     O                       ESC
     O                                              '*p1400x740Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD2
     O                       CH2
      *
     O          E    86      CHECK       0  0
     O                       ESC
     O                                              '*p1400x840Y'
     O                       ESC
     O                                              '(0Y'
     O                       ESC
     O                       BICMD1
     O                       CH1
      *
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECK       0  0
     O******                                '|(252X'
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECK       0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
0065 O          E            DETH           2 28
     O                                           11 '==DATE=='
     O                                           25 '==ACCOUNT=='
     O                                           60 '===DESCRIPTION==='
0073 O                                           84 '====AMOUNT===='
0065 O          E            DET            1
     O                       XXDATE        Y     11
     O               68      DSFYR               18
     O               68                          19 '-'
     O               68      DSEMP               24
     O              N68      C2INV#              27
     O                       ADPODS              60
0073 O                       XXNET         M     84
     O*** BEGIN ADD *** 10/13/00 **************************************
     O          E            DET         2  1
     O                                            7 'GROUP:'
     O                       ##FND         4     11
     O                       ##FNAM              73
     O*** END ADD ***** 10/13/00 **************************************
      *
     **====================================================================*
     OFILECOPY  E   N77      BANNEF      0  3 01
      *                                           RESET PRINTER TO DFT
     O              N77      ESC
     O              N77                             'E'
      *                                           Portriat Orientation
     O              N77      ESC
     O              N77                             '&l0O'
     O              N77      ESC
     O              N77                             '&l0E'
     O              N77      ESC
     O              N77                             '&a0L'
     O*
     O                       ESC
     O*** BEGIN CHG *** 12/14/98 **************************************
     O************************************* '&l1H'
     O                                              '&l4H'
     O*** END CHG ***** 12/14/98 **************************************
     O*** Font 158 Helv. Bold 14.4
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O*
     O                       ESC
     O                                              '*p1000x1000Y'
     O                                              'FILECOPY FOR BANK: '
     O                       XXBNK
      * ==============================================================
     OFILECOPY  E            CHECKF      0  0 01
      *                                           Portriat Orientation
     O                  77   ESC
     O                  77                          '&l0O'
     O                       ESC
     O                                              '&l0E'
     O                  77   ESC
     O                  77                          '&a0L'
     O*                                           PORTRAIT FONT HERE!
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                           LOAD OVERLAY (CHECK Mac#1)
     O                       ESC
     O*** BEGIN CHG *** 12/14/98 **************************************
     O************************************* '&l1H'
     O                                              '&l4H'
     O*** END CHG ***** 12/14/98 **************************************
     O                       ESC
     O                       MACRO
     O          E            CHECKF      0  0
      *                                                Font Change
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p14.4vs3b4T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1850x174Y'
     O                       XXCHK#
     O          E            CHECKF      0  0
      *                                                Font Change
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00             CHECKF
      *** Font 159 Times Roman Med 12pt
     O***                      ESC
     O***                                   '(0U'
     O***                      ESC
     O***                                   '(s1p12vsb5T'
      *** END DEL *** 02/02/98 ****************************************
      *** BEG ADD *** 02/02/98 ****************************************
     O          E            CHECKF      0  0
      *   Change Font to COURIER 12 BOLD to print group name
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p12h10v0s3b3T'
     O** ITALIC, NOT BOLD                   '(s0p12h10v1s0T'
      *** END ADD *** 02/02/98 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00     73      CHECKF
      ***                                              ACCOUNT NAME
     O***                      ESC
     O***                                   '*p125x200Y'
     O***                      BICKH1
      *** END DEL *** 02/02/98 ****************************************
      *** BEG ADD *** 02/02/98 ****************************************
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 1
     O                       ESC
     O                                              '*p050x150Y'
     O                       BI40A1
      **
     O          E    73      CHECKF      0  0
      *   Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      **                                               Font Change
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 2
     O                       ESC
     O                                              '*p050x190Y'
     O                       BI40A2
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 3
     O                       ESC
     O                                              '*p050x230Y'
     O                       BI40A3
     O          E    73      CHECKF      0  0
      *                                                UPPER LEFT 4
     O                       ESC
     O                                              '*p050x270Y'
     O                       BI40A4
      *** END ADD *** 02/02/98 ****************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *** BEG ADD *** 02/02/98 ****************************************
     O          E            CHECKF      0  0
      **  Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *** END ADD *** 02/02/98 ****************************************
     O          E            CHECKF      0  0
      *                                                Check Date
     O                       ESC
     O                                              '*p1250x174Y'
     O                       XXCHKD        Y
     O          E            CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
      *                                                Font Change
      *** BEG DEL *** 02/02/98 ****************************************
     O***     E 00     73      CHECKF
      ***                                              WORK COMP LINE
     O***                      ESC
     O***                                   '*p125x260Y'
     O***                      XCAPC
      *** END DEL *** 02/02/98 ****************************************
     O          E            CHECKF      0  0
      *                                                Font Change
     O**                                               8 PT.
     O*** Font 164 Times Roman Light 8pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p08vs-3b5T'
     O          E            CHECKF      0  0
      *                                                BANK NAME
     O                       ESC
     O*** Begin chg *** 06/30/03 **************************************
     O*********************************     '*p350x600Y'
     O                                              '*p1700x225Y'
     O*** End chg ***** 06/30/03 **************************************
     O                       BIAI01
     O*** Begin add *** 06/30/03 **************************************
     O          E            CHECKF      0  0
      *                                                Bank address
     O                       ESC
     O                                              '*p1700x260Y'
     O                       BIAI02
      *
     O          E            CHECKF      0  0
      *                                                Bank city, st, zip
     O                       ESC
     O                                              '*p1700x295Y'
     O                       BIAI03
     O*** End add ***** 06/30/03 **************************************
     O*********************************************************************
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
      *                                                Font Change 12Pt
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT NUMERIC
     O                       ESC
     O                                              '*p2020x450Y'
     O                       XXAMT#                 '  ,   , $0.  '
     O*
     O          E            CHECKF      0  0
      *                                           CHECK AMOUNT ALPHA
     O                       ESC
     O                                              '*p200x500Y'
     O                       XXAMT1
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p200x550Y'
     O                       XXAMT2
     O          E            CHECKF      0  0
      *                                           CHECK NAME/ADDRESS
     O                       ESC
     O                                              '*p250x650Y'
     O                       XXNAME
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x700Y'
     O                       XXADD1
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x750Y'
     O                       XXADD2
     O*
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p250x800Y'
     O                       XXADD3
     O*
     O*
     O          E    85      CHECKF      0  0
     O*** Font 217 Line Printer 19Cpi
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p19hsb0T'
     O*       E 00     85      CHECKF
     O                       ESC
     O                                              '*p1400x605Y'
     O*
     O          E            CHECKF      0  0
     O*** Font 159 Times Roman Med 12pt
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s1p12vsb5T'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1600x700Y'
     O                                              'FILE COPY'
     O          E            CHECKF      0  0
     O                       ESC
     O                                              '*p1500x830Y'
     O                                              'NON-NEGOTIABLE'
     O*
      ***************************************************************
      *    REST OF PAGE IS COURIER 16.66 PITCH (17)
      ***************************************************************
     O          E            CHECKF      0  0
     O*** Font 226 Line Printer 16.66
     O                       ESC
     O                                              '(0U'
     O                       ESC
     O                                              '(s0p16.66hsb0T'
      *                                            FONT MED/COR. 8.5PT/16.66
     O          E            CHECKF      0  0
      *                                            Position at 0"
     O                       ESC
     O                                              '*p00x00Y'
     O*
     O          E            DETG           2 40
     O                                           11 '==DATE=='
     O                                           25 '==ACCOUNT=='
     O                                           60 '===DESCRIPTION==='
0073 O                                           84 '====AMOUNT===='
     O          E            DETF           1
     O                       XXDATE        YB    11
     O               68      DSFYR               18
     O               68                          19 '-'
     O               68      DSEMP               24
     O              N68      C2INV#              27
     O                       ADPODS              60
0073 O                       XXNET         MB    84
     O*** BEGIN ADD *** 10/13/00 **************************************
     O          E            DETF        2  1
     O                                            7 'GROUP:'
     O                       ##FND         4     11
     O                       ##FNAM              73
     O*** END ADD ***** 10/13/00 **************************************
** SIGNATURE MESSAGE PART ONE
OVER
** SIGNATURE MESSAGE PART TWO
 REQUIRES SECOND SIGNATURE
