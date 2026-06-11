     HCopyright('(c) 2023 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
    H* StoneRiver is a trademark of StoneRiver, Inc.                 *
     H*****************************************************************
     H/TITLE F9950R - DISPLAY CHARGE LISTING
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program produces the Detail Charge Listing         **
     H**   CALLED FROM:                                              **
     H**     F9950C - Detail Charge Listing Override                 **
     H**   CALLS TO:                                                 **
     H**     SRCENTER - Subprogram to center a field                 **
     H**     SRCVTDAT - Convert date from one format to another      **
     H**     SRNAME   - Subprogram to build a combined name          **
     H**                                                             **
     H**   PROGRAMMER: C:31                                          **
     H**   CREATED:   01/11/99      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:                                                   *
     H*                                                               *
     H* 07/18/23 C:342 Project: NY060                                 *
     H*                NY Compliance: CARC/RARC Update                *
     H*                                                               *
     H* 06/09/20 C:212 Project : SY420                                *
     H*                Mask SSN                                       *
     H*                recompile only                                 *
     H*                                                               *
     H* 05/10/19 C:180 Support: 79254                                 *
     H*                Mask SS#                                       *
     H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H*  10/07/03 C:62  PROJECT: FS267
     H*                 FIX-UP THE PBILLS PROCESSING
     H*
     H*  01/07/03 C:79  PROJECT: 114MDC
     H*                 WHEN SELECTING PENDED LINES INCLUDE CODEREVIEW
     H*
     H*  02/26/01 C:62  PROJECT: MC152
     H*                 MAKE PBILLS CAPABLE
     H*
     H*  08/15/00 C:62  PROJECT: MC215
     H*                 REPLACE SFBPF REFERENCES WITH APIS.
     H*
     H*  10/25/99 C:117 Project: FSMNT
     H*                 Added a line containing RULE # & Description
     H*                 on the report.
     H*
     H*  07/18/00 C:117 Project: MCMNT
     H*                 Added a condition around the record selection
     H*                 for FDCAP; had to add a LF to the pgm
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      08      Batch Mode                                     **
     H**      09      Demand Mode                                    **
     H**      10      Print Service Code (XXCMSV)                    **
     H**     N10      Print CPT4 Code    (CACPT4)                    **
     H**      25                                                     **
     H**      40      Select by FEIN/MOD                             **
     H**      41      All charges requested                          **
     H**      42      Current charges requested                      **
     H**      43      Pended charges requested                       **
     H**      44      History charges requested                      **
     H**      83      Current line is pended                         **
     H**      84      Current line is history                        **
     H**      86      First error line has been printed              **
     H**      60      General Chain/Usage                            **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FADCL    (PDC01): Diagnosis Code/Detail Charge File    **
     H**                        (CO#/SUB#/FND/FYR/CASE/FEIN/MOD/DCS#)**
     H**      FDCALF   (PCA01): Detail Charge File                   **
     H**                        (CO#/SUB#/FND/FYR/CASE/DSEQ)         **
     H**      FDDEL1   (PDE01): EOMB Detail EOB/Notes Transaction    **
     H**                        (CO#/SUB#/FND/FYR/CASE/EOB#/DESQ)    **
     H**      FDHCL    (PHC01): Detail Charge Auxiliary File         **
     H**                        (CO#/SUB#/FND/FYR/CASE/DSEQ)         **
     H**      FDMGL1   (PMG01): Prov Bills specific errors           **
     H**                        (CO#/SUB#/FND/FYR/CASE/EOB#/DESQ)    **
     H**      FMCDL    (PCD01): Claimant Demographics (SS#)          **
     H**      FTMIL    (PMI01): Bill Entry Message ID Table          **
     H**                        (CO#/SUB#/FND/FYR/CASE/EOB#/DESQ)    **
     H**      SMCOL    (PCO01): System Company File (CO#)            **
     H**      SMORL    (POR01): Output Routing File (PNAM/FIL#)      **
     H**      SMUPL    (PUP01): User Parameters File (USR)           **
     H**      SSYLF             System & Application Control File    **
     H**               (PFB01): Fee Schedule Base Control File (KEY) **
     H**      FDFRL2   (PFR01): Fee Schedule rules (ST/RULE)         **
     H*** BEG ADD *** 07/18/00 ****************************************
     H**      FAEOLG   (PEO01): EOMB Transaction File                **
     H*** END ADD *** 07/18/00 ****************************************
     H*****************************************************************
     H*
     FFADCL     IF   E           K DISK
     FFDCALF    IF   E           K DISK
     C**** Beg Chg *** 07/18/23 **************************************
     F**** FDDEL1    IF   E           K DISK
     FFDDEL6    IF   E           K DISK
     C**** End Chg *** 07/18/23 **************************************
     FFDMGL1    IF   E           K DISK
     FFDHCL     IF   E           K DISK
     FFMCDL     IF   E           K DISK
     FFTMIL     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSMORL     IF   E           K DISK
     FSMUPL     IF   E           K DISK
     F*** BEGIN DEL *** 08/15/00 ***********************************************
     F**********SSYLF   IF  E           K        DISK
     F*** END   DEL *** 08/15/00 ***********************************************
     F*** BEG ADD *** 10/25/99 ****************************************
     FFDFRL2    IF   E           K DISK
     F*** END ADD *** 10/25/99 ****************************************
     F*** BEG ADD *** 07/18/00 ****************************************
     FFAEOLG    IF   E           K DISK
     F*** END ADD *** 07/18/00 ****************************************
     F*** BEGIN ADD *** 02/26/01 ***********************************************
     FFDDHL     IF   E           K DISK
     FFDDPL     IF   E           K DISK
     FFDDUL     IF   E           K DISK
     FFDDDL     IF   E           K DISK
     F*** END   ADD *** 02/26/01 ***********************************************
     F*** BEGIN ADD *** 10/07/03 ***********************************************
     FFDDAL4    IF   E           K DISK
     F*** END   ADD *** 10/07/03 ***********************************************
     FQSYSPRT   O    F  198        PRINTER OFLIND(*INOA)

     D*
     D* ---------------------------------------------------------------
     D* External system information data structures
     D* ---------------------------------------------------------------
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D* ---------------------------------------------------------------
     D* System date and time
     D* ---------------------------------------------------------------
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D* ---------------------------------------------------------------
     D* Code work area
     D* ---------------------------------------------------------------
     D                 DS
     D  XXCMSV                 1     11
     D  XXRVCD                 1      3  0
     D  XXHCPC                 4      8
     D  XXUNTS                 9     11  0
     D*
     D* ---------------------------------------------------------------
     D* Units work area
     D* ---------------------------------------------------------------
     D                 DS
     D  XX10A1                 1     10
     D  X110A1                 1      3
     D  X210A1                 4      4
     D*** BEG ADD *** 10/25/99 ****************************************
     D* ---------------------------------------------------------------
     D* Constants
     D* ---------------------------------------------------------------
     D CHGNUM          C                   CONST('SA020R')

     I*** END ADD *** 10/25/99 ****************************************
     I*
     I*

     C*
     C* ---------------------------------------------------------------
     C* Parm lists
     C* ---------------------------------------------------------------
     C     *ENTRY        PLIST
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQSS#             9
     C                   PARM                    QQACDT            8
     C                   PARM                    QQFEIN            9
     C                   PARM                    QQMOD             3
     C                   PARM                    QQTYPE            1
     C                   PARM                    QQQUE            10
     C                   PARM                    QQMODE            1
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
     C     DTMDY1        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*** BEG ADD *** 10/25/99 ****************************************
     C*
     C* PARMs for CHGNUM API
     C*
     C     CNUMPL        PLIST
     C                   PARM                    ##NUM            80
     C                   PARM      *BLANKS       ##MASK           10
     C*** END ADD *** 10/25/99 ****************************************
     C*
     C* ---------------------------------------------------------------
     C*        KEY LISTS
     C* ---------------------------------------------------------------
     C*
     C*          PDC01 - Diagnosis Code/Detail Charge File
     C     DCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    CADCS#
     C*
     C*          PDE01 - EOMB Detail EOB/Notes Transaction File
     C     DEKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*          PMG01 - Prov Bills specific errors
     C     MGKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C*          PMI01 - Bill Entry Message ID Table
     C     MIKY          KLIST
     C                   KFLD                    MGMSG#
     C*
     C*          PCA01 - Detail Charge File
     C     CAKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C     CAKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C                   KFLD                    XXDSEQ
     C*** BEGIN ADD *** 02/26/01 **************************************
     C     GENKY         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*** END   ADD *** 02/26/01 **************************************
     C*
     C*          PHC01 - Detail Charge Auxiliary File
     C     HCKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CADSEQ
     C*
     C*          PCD01 - Claimant Demographics
     C     CDKY1         KLIST
     C                   KFLD                    XXSS#
     C*
     C*          PCO01 - System Company File
     C     COKY          KLIST
     C                   KFLD                    XXCO#
     C*
     C*          POR01 - Output Routing File
     C     ORKY          KLIST
     C                   KFLD                    Q1PGM
     C                   KFLD                    XXPRT
     C*
     C*          PUP01 - User Parameters File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C*          System & Application Control File
     C*          PFB01 - Fee Schedule Base Control File
     C*
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C********** FBKY      KLIST
     C**********           KFLD           XSKEY   2
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEG ADD *** 10/25/99 ****************************************
     C*          PFR01 - Fee Schedule Rules File
     C     FRKY          KLIST
     C                   KFLD                    XXST              2
     C                   KFLD                    XXRULE            9 0
     C*** END ADD *** 10/25/99 ****************************************
     C*** BEG ADD *** 07/18/00 ****************************************
     C*          PEO01 - EOMB Transaction File
     C     EOKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C*** END ADD *** 07/18/00 ****************************************
     C*** BEGIN ADD *** 10/07/03 **************************************
     C     DAKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    EORCN
     C                   KFLD                    CADSEQ
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C* ---------------------------------------------------------------
     C*        Defines
     C* ---------------------------------------------------------------
     C     *LIKE         DEFINE    CACO#         XXCO#
     C     *LIKE         DEFINE    CASUB#        XXSUB#
     C     *LIKE         DEFINE    CAFND         XXFND
     C     *LIKE         DEFINE    CAFYR         XXFYR
     C     *LIKE         DEFINE    CACASE        XXCASE
     C     *LIKE         DEFINE    CASS#         XXSS#
     C     *LIKE         DEFINE    CAFEIN        XXFEIN
     C     *LIKE         DEFINE    CAMOD         XXMOD
     C     *LIKE         DEFINE    ORFIL#        XXPRT
     C     *LIKE         DEFINE    HCNDC         XXNDC
     C     *LIKE         DEFINE    MGMSG#        XXMSG#
     C     *LIKE         DEFINE    MIDESC        XXDES1
     C     *LIKE         DEFINE    MIDESC        XXDES2
     C*** BEGIN ADD *** 02/26/01 **************************************
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    DHPOS         XXPOS
     C*** END   ADD *** 02/26/01 **************************************
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          M A I N L I N E
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*
     C*          Initialization
     C*          ------------------------------------------------------
     C                   EXSR      SRINIT
     C*
     C*          Get first record - if no match print error message
     C*          ------------------------------------------------------
     C     CAKY1         SETLL     PCA01
     C     CAKY          READE     PCA01                                  60
     C     *IN60         IFEQ      *ON
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    ERRMSG
     C                   ELSE                                                   *IN60 = On
     C*** BEGIN ADD *** 10/07/03 **************************************
     C*
     C* Check to see if Standalone Group
     C*
     C                   MOVE      CACO#         #1CO#
     C                   MOVE      CASUB#        #1SUB#
     C                   MOVE      CAFND         #1FND
     C*
     C                   CALL      'FA701R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM      *BLANKS       ##OUTS            1
     C*** END   ADD *** 10/07/03 **************************************
     C*
     C*          Loop: Detemine if record is to be selected
     C*          Selection is based on 2 conditions: FEIN/MOD and
     C*          match to selection criteria.
     C*             If 40: FEIN/MOD must match
     C*             If 41: All records selected
     C*             If 42: Current charges selected
     C*             If 43: Pended charges selected
     C*             If 44: History charges selected
     C*          ------------------------------------------------------
     C     *IN60         DOWEQ     *OFF
     C                   MOVE      *BLANK        XXOK1             1
     C                   MOVE      *BLANK        XXOK2             1
     C*** BEG ADD *** 07/18/00 ****************************************
     C     EOKY          CHAIN     PEO01                              59
     C*** BEGIN ADD *** 02/26/01 **************************************
     C                   Z-ADD     *ZEROS        XXUNTS
     C                   SELECT
     C*
     C     EOBTYP        WHENEQ    'H'
     C     GENKY         CHAIN     PDU01                              99
     C                   Z-ADD     DURVCD        XXRVCD
     C                   MOVE      DURATE        XXHCPC
     C  N99              Z-ADD     DUUNTS        XXUNTS
     C   99              Z-ADD     *ZEROS        XXUNTS
     C                   MOVEL     *BLANKS       XXPMOD
     C                   MOVEL(P)  DUPOS         XXPOS
     C                   SETON                                        1112
     C*
     C     EOBTYP        WHENEQ    'N'
     C     GENKY         CHAIN     PDH01                              99
     C                   SETON                                        11
     C                   SETOFF                                       12
     C  N99              Z-ADD     DHUNTS        XXUNTS
     C   99              Z-ADD     *ZEROS        XXUNTS
     C                   MOVEL(P)  DHMOD1        XXPMOD
     C                   MOVEL(P)  DHPOS         XXPOS
     C*
     C     EOBTYP        WHENEQ    'D'
     C     GENKY         CHAIN     PDD01                              99
     C  N99              Z-ADD     DDUNTS        XXUNTS
     C   99              Z-ADD     *ZEROS        XXUNTS
     C                   MOVEL     *BLANKS       XXPMOD
     C                   MOVEL(P)  DDPOS         XXPOS
     C                   SETOFF                                       11
     C                   SETON                                        12
     C*
     C     EOBTYP        WHENEQ    'X'
     C     GENKY         CHAIN     PDP01                              99
     C  N99              Z-ADD     DPUNTS        XXUNTS
     C   99              Z-ADD     *ZEROS        XXUNTS
     C                   MOVEL     *BLANKS       XXPMOD
     C                   MOVEL(P)  *BLANKS       XXPOS
     C                   SETOFF                                       1112
     C*
     C                   ENDSL
     C*** END   ADD *** 02/26/01 **************************************
     C     EOBLST        IFNE      'X'
     C*** END ADD *** 07/18/00 ****************************************
     C     *IN40         IFEQ      *ON
     C     CAFEIN        IFEQ      XXFEIN
     C     CAMOD         ANDEQ     XXMOD
     C                   MOVE      'Y'           XXOK1
     C                   ENDIF                                                  FEIN = FEIN
     C                   ELSE                                                   *IN40 = On
     C                   MOVE      'Y'           XXOK1
     C                   ENDIF                                                  *IN40 = On
     C*** BEG CHG *** 07/18/00 ****************************************
     C***41******          MOVE 'Y'       XXOK2
     C*
     C*** BEGIN ADD ** 10/07/03 ***************************************
     C*
     C* CHECK TO SEE IF THIS IS A DELETED BILL AND IF A PAYMENT
     C* HAS BEEN MADE
     C*
     C                   MOVE      *ON           XXOK3             1
     C     EOBLST        IFEQ      'D'
     C     ##OUTS        IFNE      'Y'
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOSUB#        #1SUB#
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C                   MOVE      EOCO#         #CCO#
     C                   MOVE      EOFYR         #CFYR
     C                   MOVE      EOCASE        #CCASE
     C                   MOVE      EOEOB#        #CEOB#
     C                   MOVE      CARCN         #CRCN
     C*
     C                   CALL      'WA047R'
     C                   PARM                    #CCO#             3
     C                   PARM      #1WCF#        #CFND             3
     C                   PARM                    #CFYR             3
     C                   PARM                    #CCASE            7
     C                   PARM                    #CEOB#            9
     C                   PARM      *BLANKS       #CRTCD            1
     C                   PARM                    #CRCN             3
     C*
     C     #CRTCD        IFEQ      'F'
     C                   MOVE      *OFF          XXOK3
     C                   ENDIF
     C*
     C                   ELSE
     C     CARCN         IFEQ      EORCN
     C                   MOVE      *OFF          XXOK3
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*** END   ADD ** 10/07/03 ***************************************
     C*** BEGIN CHG ** 10/07/03 ***************************************
     C********** *IN41     IFEQ *ON
     C********** *IN59     IFEQ *OFF
     C********** CAFLAG    ANDEQ'P'
     C********** EOBLST    ANDEQ'P'
     C**********           MOVE 'Y'       XXOK2
     C**********           ELSE
     C********** CAFLAG    IFEQ ' '
     C**********           MOVE 'Y'       XXOK2
     C**********           ENDIF                           CAFLAG=' '
     C**********           ENDIF                           *IN59=OFF
     C**********           ENDIF                           *IN41=OFF
     C                   MOVEL     *BLANKS       XXOK2
     C     *IN41         IFEQ      *ON
     C     XXOK3         ANDEQ     *ON
     C                   MOVE      'Y'           XXOK2
     C                   ENDIF
     C*** END   CHG ** 10/07/03 ***************************************
     C*** END CHG *** 07/18/00 ****************************************
     C*
     C*** BEGIN DEL *** 10/07/03 **************************************
     C********** *IN42     IFEQ *ON
     C********** CAEOB#    IFEQ 0
     C********** CAFLAG    ANDEQ*BLANKS
     C**********           MOVE 'Y'       XXOK2
     C**********           ENDIF                           EOB# = 0
     C**********           ENDIF                           *IN42 = On
     C*** END   DEL *** 10/07/03 **************************************
     C*
     C*** BEGIN CHG *** 10/07/03 **************************************
     C********** *IN43     IFEQ *ON
     C********** CAFLAG    IFEQ 'P'
     C*** BEG ADD *** 07/18/00 ****************************************
     C********** EOBLST    ANDEQ'P'
     C*** END ADD *** 07/18/00 ****************************************
     C*** BEG ADD *** 01/07/03 ****************************************
     C********** CAFLAG    OREQ ' '
     C********** EOBLST    ANDEQ'P'
     C*** END ADD *** 01/07/03 ****************************************
     C**********           MOVE 'Y'       XXOK2
     C**********           ENDIF                           FLAG = 'P'
     C**********           ENDIF                           *IN43 = On
     C     *IN43         IFEQ      *ON
     C     EOBLST        ANDEQ     'P'
     C     EORCN         IFEQ      CARCN
     C                   MOVEL     'Y'           XXOK2
     C                   ELSE
     C     DAKY          CHAIN     PDA01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     'Y'           XXOK2
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*** END   CHG *** 10/07/03 **************************************
     C*
     C*** BEGIN CHG *** 10/07/03 **************************************
     C********** *IN44     IFEQ *ON
     C********** CAEOB#    IFNE 0
     C**********           MOVE 'Y'       XXOK2
     C**********           ENDIF                           EOM# = 0
     C**********           ENDIF                           *IN44 = On
     C*
     C* WE ARE LOOKING AT HISTORY
     C*
     C     *IN44         IFEQ      *ON
     C                   SELECT
     C     EOBLST        WHENEQ    'C'
     C                   MOVE      'Y'           XXOK2
     C     EOBLST        WHENEQ    'D'
     C     XXOK3         ANDEQ     *ON
     C                   MOVE      'Y'           XXOK2
     C     EOBLST        WHENEQ    'P'
     C     EORCN         IFGT      CARCN
     C                   MOVE      'Y'           XXOK2
     C                   ENDIF
     C                   ENDSL
     C                   ENDIF                                                  *IN44 = On
     C*** END   CHG *** 10/07/03 **************************************
     C*
     C*          If both flags OK then process record
     C*          ------------------------------------------------------
     C     XXOK1         IFEQ      'Y'
     C     XXOK2         ANDEQ     'Y'
     C*
     C*             Move Detail Charge data to output
     C*             ---------------------------------------------------
     C*** BEGIN DEL *** 02/26/01 **************************************
     C**********           Z-ADDCASVCD    XXRVCD  30
     C**********           MOVE CACPT4    XXHCPC  5
     C**********           Z-ADDCAUNTS    XXUNTS  30
     C*** END   DEL *** 02/26/01 **************************************
     C     CAPRTY        COMP      '4'                                    10
     C                   Z-ADD     CARDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXRDAT            6 0
     C                   Z-ADD     CAFDAT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY
     C                   MOVEL     SRDTO         XXFDAT            6 0
     C*
     C*** BEG ADD *** 07/18/00 ****************************************
     C** CONDITION THE FOLOWING LOGIC ON WHETHER OR NOT PBILLS IS
     C** INSTALLED; IF NOT, PRINT LTR# RATHER THAN EOB; IF SO, PRINT
     C** EOB# RATHER THAN THE LTR#
     C*
     C                   CALL      'SA001R'
01   C                   PARM                    @@WCIF            1            WC INST
02   C                   PARM                    @@MCIF            1            MC INST
03   C                   PARM                    @@WCIN            1            WC INTERFACE
04   C                   PARM                    @@AR              1            MC AR INST
05   C                   PARM                    @@NTWK            1            NETWORK INST
06   C                   PARM                    @@UR              1            PRECERT INST
07   C                   PARM                    @@MED             1            MEDMGMT INST
08   C                   PARM                    @@EDI             1            EDI INST
09   C                   PARM                    @@WCDK            1            WC DARTS KEY
10   C                   PARM                    @@RHAB            1            REHAB INST
11   C                   PARM                    @@MSS             1            MS SUPPORT
12   C                   PARM                    @@FSCH            1            FEE SCHEDULE
13   C                   PARM                    @@PRVB            1            PROVIDER BILLS
     C*
     C     @@PRVB        IFNE      'Y'
     C*** END ADD *** 07/18/00 ****************************************
     C                   SETOFF                                       8384
     C     *IN41         IFEQ      *ON
     C     CAFLAG        COMP      'P'                                    83
     C  N83CAEOMB        COMP      *ZEROS                             8484
     C                   ELSE                                                   *IN41 = On
     C   43              SETON                                        83
     C   44              SETON                                        84
     C                   ENDIF                                                  *IN41 = On
     C*** BEG ADD *** 07/18/00 ****************************************
     C                   ELSE
     C                   SETON                                        84
     C                   SETOFF                                       83
     C                   ENDIF                                                  @@PRVB='Y'
     C*** END ADD *** 07/18/00 ****************************************
     C*
     C*             Move Detail Charge Auxiliary data to output
     C*             ---------------------------------------------------
     C                   SETOFF                                       25
     C                   MOVE      *BLANKS       XX10A1
     C     HCKY          CHAIN     PHC01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     HC10A1        XX10A1
     C     CARCFL        IFEQ      'X'
     C                   SETON                                        25
     C                   MOVEL     HCNDC         XXNDC
     C*** BEGIN DEL *** 02/26/01 **************************************
     C**********           Z-ADDHCRXMQ    XXUNTS
     C*** END   DEL *** 02/26/01 **************************************
     C                   ELSE                                                   RCFL = 'X'
     C*** BEGIN DEL *** 02/26/01 **************************************
     C**********           Z-ADDCAUNTS    XXUNTS
     C*** END   DEL *** 02/26/01 **************************************
     C                   ENDIF                                                  RCFL = 'X'
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*             Move Diagnosis Code data to output
     C*             ---------------------------------------------------
     C     DCKY          CHAIN     PDC01                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     6             X                 5 0
     C     3             SUBST     DC10A1:X      XCTYPE            3    65
     C                   ELSE                                                   *IN60 = Off
     C                   MOVE      *BLANKS       XCTYPE
     C                   ENDIF                                                  *IN60 = Off
     C*
     C*             Print detail record and update totals
     C*             ---------------------------------------------------
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET
     C                   ADD       1             TOTCNT
     C                   ADD       CACHRG        TOTCHR
     C                   ADD       CAMRA         TOTDIS
     C                   ADD       CAOTHR        TOTOTH
     C                   ADD       CAOTH2        TOTOT2
     C                   ADD       CANDIS        TOTNDS
     C                   ADD       CAPAID        TOTPAD
     C*** BEG ADD *** 10/25/99 ****************************************
     C*             Move Rule # & Description to output
     C*             ---------------------------------------------------
     C                   MOVEL     CAAST         XXST
     C                   MOVEL     CARULE        XXRULE
     C     FRKY          CHAIN     PFR01                              60
     C                   MOVEL(P)  CARULE        ##NUM
     C                   CALL      CHGNUM        CNUMPL
     C                   MOVEL(P)  ##NUM         RULE#             9 0
     C  N60              MOVEL(P)  FRDESC        RULEDS           60
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET1A
     C*** END ADD *** 10/25/99 ****************************************
     C*
     C*             Move EOMB EOB data to output
     C*             ---------------------------------------------------
     C     DEKY          CHAIN     PDE01                              60
     C     *IN60         DOWEQ     *OFF
     C**** Beg Chg *** 07/18/23 **************************************
     C**** DEEOB         IFNE      *BLANKS
     C     DEEOBCD       IFNE      *BLANKS
     C**** End Chg *** 07/18/23 **************************************
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET2A
     C                   ENDIF                                                  EOB <> blanks
     C     DEKY          READE     PDE01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*             Move error message data to output
     C*             ---------------------------------------------------
     C                   MOVE      *BLANKS       XXMSG#
     C     MGKY          CHAIN     PMG01                              60
     C     *IN60         DOWEQ     *OFF
     C     MGMSG#        IFNE      XXMSG#
     C     MIKY          CHAIN     PMI01                              61
     C                   Z-ADD     0             XXFLIP            1 0
     C                   SETOFF                                       86
     C     *IN61         DOWEQ     *OFF
     C                   ADD       1             XXFLIP
     C     XXFLIP        IFEQ      1
     C                   MOVEL(P)  MIDESC        XXDES1
     C                   MOVEL(P)  *BLANKS       XXDES2
     C                   ENDIF                                                  FLIP = 1
     C     XXFLIP        IFEQ      2
     C                   Z-ADD     0             XXFLIP
     C                   MOVEL(P)  MIDESC        XXDES2
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET3A
     C                   SETON                                        86
     C                   ENDIF                                                  FLIP = 2
     C     MIKY          READE     PMI01                                  61
     C                   ENDDO                                                  *IN61 = Off
     C     XXFLIP        IFEQ      1
     C   OA              EXCEPT    HEAD
     C                   EXCEPT    DET3A
     C                   ENDIF                                                  FLIP = 1
     C                   ENDIF                                                  MSG# <> MSG#
     C                   MOVE      MGMSG#        XXMSG#
     C     MGKY          READE     PMG01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C                   ENDIF                                                  OK = 'Y'
     C*** BEG ADD *** 07/18/00 ****************************************
     C                   ENDIF                                                  EOBLST='X'
     C*** END ADD *** 07/18/00 ****************************************
     C*
     C*          Read next record
     C*          ------------------------------------------------------
     C     CAKY          READE     PCA01                                  60
     C                   ENDDO                                                  *IN60 = Off
     C*
     C*          Print totals or error if no totals
     C*          ------------------------------------------------------
     C   OA              EXCEPT    HEAD
     C     TOTCNT        IFNE      0
     C                   EXCEPT    TOT
     C                   ELSE                                                   TCNT <> 0
     C                   EXCEPT    ERRMSG
     C                   ENDIF                                                  TCNT <> 0
     C*
     C*          End of Program
     C*          ------------------------------------------------------
     C                   ENDIF                                                  *IN60 = On
     C                   SETON                                        LR
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxprt = '0';
       Chain Orky Por01;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*          S U B R O U T I N E S
     C*----------------------------------------------------------------
     C*----------------------------------------------------------------
     C*****************************************************************
     C*          SRINIT - INITIALIZATION
     C*****************************************************************
     C     SRINIT        BEGSR
     C*** BEGIN ADD *** 08/15/00 ***********************************************
     C*
     C                   CALL      'SA001R'
     C                   PARM      *BLANK        ##WCIF            1            WC INST
     C                   PARM      *BLANK        ##MCIF            1            MC INST
     C                   PARM      *BLANK        ##WCIN            1            WC INTERFACE
     C                   PARM      *BLANK        ##AR              1            MC AR INST
     C                   PARM      *BLANK        ##NTWK            1            NETWORK INST
     C                   PARM      *BLANK        ##UR              1            PRECERT INST
     C                   PARM      *BLANK        ##MED             1            MEDMGMT INST
     C                   PARM      *BLANK        ##EDI             1            EDI INST
     C                   PARM      *BLANK        ##WCDK            1            WC DARTS KEY
     C                   PARM      *BLANK        ##RHAB            1            REHAB INST
     C                   PARM      *BLANK        ##MSS             1            MS SUPPORT
     C                   PARM      *BLANK        ##FSCH            1            FEE SCHEDULE
     C                   PARM      *BLANK        ##PRVB            1            PROVIDER BILL
     C                   PARM      *BLANK        ##NPWC            1            NON-PDS WC  LS
     C*** END   ADD *** 08/15/00 ***********************************************
     C*
     C*          Clear fields
     C*          ------------------------------------------------------
     C                   Z-ADD     *ZEROS        TOTCNT            7 0
     C                   Z-ADD     *ZEROS        TOTCHR           11 2
     C                   Z-ADD     *ZEROS        TOTDIS           11 2
     C                   Z-ADD     *ZEROS        TOTOTH           11 2
     C                   Z-ADD     *ZEROS        TOTOT2           11 2
     C                   Z-ADD     *ZEROS        TOTNDS           11 2
     C                   Z-ADD     *ZEROS        TOTPAD           11 2
     C*
     C*          Check if network installed
     C*          ------------------------------------------------------
     C*** BEGIN DEL *** 08/15/00 ***********************************************
     C**********           MOVEL'FB'      XSKEY     P
     C********** FBKY      CHAINSFB01                60
     C*** END   DEL *** 08/15/00 ***********************************************
     C*** BEGIN CHG *** 08/15/00 ***********************************************
     C***60***** FBFL06    COMP 'Y'                      98
     C     ##NTWK        COMP      'Y'                                    98
     C*** END   CHG *** 08/15/00 ***********************************************
     C*
     C*          Initialize key fields
     C*          ------------------------------------------------------
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQSUB#        XXSUB#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      QQFYR         XXFYR
     C                   MOVE      QQCASE        XXCASE
     C                   MOVE      QQSS#         XXSS#
      *** Begin Add *** 05/10/19 *********************************************
     C                   MOVE      XXSS#         XXSS#4            4
     C                   MOVE      'XXX'         XXSS#3            3
     C                   MOVE      'XX'          XXSS#2            2
      *** End   Add *** 05/10/19 *********************************************
     C                   MOVE      QQACDT        XXACDT            8 0
     C                   Z-ADD     XXACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         X1ACDT            8 0
     C                   MOVE      QQFEIN        XXFEIN
     C                   MOVE      QQMOD         XXMOD
     C                   MOVE      QQTYPE        XXTYPE            1
     C                   MOVE      *LOVAL        XXDSEQ            5 0
     C*
     C*          Get date and determine mode
     C*          ------------------------------------------------------
     C                   TIME                    DSDTTM
     C     QQMODE        IFEQ      'D'
     C                   SETON                                        09        DEMAND MODE
     C                   ELSE
     C                   SETON                                        08        BATCH MODE
     C                   END
     C*
     C*          Determine run type
     C*            40 - Select by FEIN/MOD
     C*            41 - All charges requested
     C*            42 - Current charges requested
     C*            43 - Pended charges requested
     C*            44 - History charges requested
     C*          ------------------------------------------------------
     C     XXFEIN        COMP      0                                  4040
     C     XXTYPE        COMP      'A'                                    41
     C*** BEGIN DEL *** 10/07/03 **************************************
     C********** XXTYPE    COMP 'C'                      42
     C*** END   DEL *** 10/07/03 **************************************
     C     XXTYPE        COMP      'P'                                    43
     C     XXTYPE        COMP      'H'                                    44
     C*
     C*          Get User, Company and Claimant names
     C*          ------------------------------------------------------
     C     UPKY          CHAIN     PUP01                              60
     C   60              MOVE      *BLANK        UPUSNM
     C*
     C     COKY          CHAIN     PCO01                              60
     C                   MOVEL(P)  CONAME        ##DATA
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        CONAME
     C*
     C*** BEG ADD ***** 01/28/10 **************************************
     C                   CALL      'RTVBRND'
     C                   PARM      '@'           #BTYPE            1
     C                   PARM      *BLANKS       #BRND1           80
     C                   PARM      *BLANKS       #BRND2           80
     C*** END ADD ***** 01/28/10 **************************************
     C*
     C     CDKY1         CHAIN     PCD01                              60
     C                   MOVEL(P)  CDLSN         QQLAST           56
     C                   MOVEL(P)  CDFSN         QQFRST           15
     C                   MOVE(P)   '1'           QQTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    QQLAST
     C                   PARM                    QQFRST
     C                   PARM                    QQTERM
     C                   MOVEL(P)  QQLAST        XXNAME           56
     C*
     C*          Get distribution info and print routing and header
     C*          ------------------------------------------------------
     C                   MOVE      '0'           XXPRT
     C     ORKY          CHAIN     POR01                              60
      ***** Beg Del ***** 08/26/14 *********************************************
     C**********  N60              EXCEPT    ROUTE
      ***** End Del ***** 08/26/14 *********************************************
     C                   EXCEPT    HEAD
     C                   ENDSR
     O*
     O*----------------------------------------------------------------
     O*----------------------------------------------------------------
     O*          D E T A I L   C H A R G E   L I S T I N G
     O*----------------------------------------------------------------
     O*----------------------------------------------------------------
     OQSYSPRT   E            ROUTE          1 06
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********     08      ORDIS1              60
     O**********        09   UPUSNM              67
     O**********E            ROUTE          1
     O**********     08      ORDIS2              60
     O**********E            ROUTE          1
     O**********     08      ORDIS3              60
     O**********E            ROUTE          2
     O**********     08      ORDIS4              60
      ***** End Del ***** 08/26/14 *********************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          E            ROUTE          1
     O                                           15 'ROUTE TO:'
     O               08      ORDIS1              60
     O                  09   UPUSNM              67
     O          E            ROUTE          1
     O               08      ORDIS2              60
     O          E            ROUTE          1
     O               08      ORDIS3              60
     O          E            ROUTE          2
     O               08      ORDIS4              60
      ***** End Add ***** 08/05/15 ************************************
     O          E            ROUTE          2
     O                                           19 'REPORT TITLE:'
     O                       ORTITL              80
      ***** Beg Del ***** 08/26/14 *********************************************
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
     O**********     08      OROUTQ              30
     O**********     09      QQQUE               30
     O**********                                 37 'FORMS:'
     O**********             ORFORM              48
     O**********                                 56 'SAVE:'
     O**********             ORSAVE              61
      ***** End Del ***** 08/26/14 *********************************************
     O          E            ROUTE          2 33
     O                                           18 'REPORT SELECTIONS:'
     O          E            ROUTE          2
     O                                           26 'COMPANY        :'
     O                       XXCO#               38
     O          E            ROUTE          2
     O                                           26 'SUBSCRIBER     :'
     O                       XXSUB#              38
     O          E            ROUTE          2
     O                                           26 'FUND           :'
     O                                           26 'GROUP          :'
     O                       XXFND               38
     O          E            ROUTE          2
     O                                           26 'FUND YEAR      :'
     O                                           26 'POLICY YEAR    :'
     O                       XXFYR         Z     38
     O          E            ROUTE          2
     O                                           26 'CASE NUMBER    :'
     O                       XXCASE        Z     38
     O          E            ROUTE          2
     O                                           26 'SS NUMBER      :'
      *** Begin Change *** 05/10/19 ****************************************
     O**                     XXSS#               38
     O                       XXSS#3            +001
     O                                              '-'
     O                       XXSS#2
     O                                              '-'
     O                       XXSS#4
      *** End   Change *** 05/10/19 ****************************************
     O          E            ROUTE          2
     O                                           26 'ACCIDENT DATE  :'
     O                       X1ACDT              38 '  /  /    '
     O          E            ROUTE          2
     O                                           26 'PROVIDER/MOD   :'
     O               40      XXFEIN        Z     38
     O               40      XXMOD         Z     42
     O              N40                          38 'ALL'
     O          E            ROUTE          2
     O                                           26 'CHARGE TYPE    :'
     O               41                          40 'ALL CHARGES'
     O*** BEGIN DEL *** 10/07/03 **************************************
     O**********       42                44 'CURRENT CHARGES'
     O*** END   DEL *** 10/07/03 **************************************
     O               43                          43 'PENDED CHARGES'
     O               44                          44 'HISTORY CHARGES'
     O          E            ROUTE          2
     O                                           26 'RUN DATE       :'
     O                       DSDATE              38 '  /  /  '
      ****** BEG CHG ***** 01/28/10 **********************************
     O****    E  160           ROUTE
     O****                               20 '(C) COPYRIGHT PROGRE'
     O****                               41 'SSIVE DATA SOLUTIONS,'
     O****                               63 'INC. 1999            '
     O          E            ROUTE          1 59
     O                       #BRND1
     O          E            ROUTE          1
     O                       #BRND2
      ****** END CHG ***** 01/28/10 **********************************
     O          E            HEAD           1 06
     O                       DSTIME              08 '  :  :  '
     O                       CONAME              97
     O                       DSDATE        Y    197
     O          E            HEAD           1
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          191 'PAGE:'
     O                       PAGE          Z    197
     O          E            HEAD           1
     O                                           17 'CO#'
     O                                           25 'SUB#'
     O                                           35 'GROUP'
     O                                           43 'YEAR'
     O                                           53 'CASE'
     O                                           67 'SOC.SEC.#'
     O                                           82 'ACCIDENT DT'
     O                                           99 'PROVIDER/MOD'
     O                                          116 'CHARGE TYPE'
     O                                          135 'CLAIMANT NAME'
     O          E            HEAD           2
     O                       XXCO#               17
     O                       XXSUB#              26
     O                       XXFND               34
     O                       XXFYR         Z     42
     O                       XXCASE        Z     54
      *** Begin Change *** 05/10/19 **********************************
     O***                    XXSS#               67
     O                       XXSS#3              59
     O                                              '-'
     O                       XXSS#2
     O                                              '-'
     O                       XXSS#4
      *** End   Change *** 05/10/19 **********************************
     O                       X1ACDT              82 '  /  /    '
     O               40      XXFEIN        Z     95
     O               40      XXMOD         Z     99
     O              N40                          94 'ALL'
     O               41                         116 'ALL CHARGES'
     O*** BEGIN DEL *** 10/03/03 **************************************
     O**********       42               118 'CURRENT CHARGES'
     O*** END   DEL *** 10/03/03 **************************************
     O               43                         117 'PENDED CHARGES'
     O               44                         118 'HISTORY CHARGES'
     O                       XXNAME             178
     O          E            HEAD           1
     O                                           25 'TRANS'
     O                                           46 'PROV'
     O                                           52 'GEO'
     O                                           58 'SPEC'
     O                                           62 'ADJ'
     O                                           66 'CLM'
     O                                           73 'SERV'
     O                                           83 'SUB'
     O                                          100 'PROC'
     O                                          111 'PART'
     O                                          123 'TOTAL'
     O                                          133 'MRA'
     O                                          150 'OTHER 1'
     O                                          164 'OTHER 2'
     O                                          178 'NETWORK'
     O                                          193 'TOTAL'
     O          E            HEAD           1
     O                                           05 'DTL'
     O                                           15 'EOMB#'
     O                                           24 'DATE'
     O                                           40 'PROVIDER  MOD'
     O                                           45 'ZIP'
     O                                           52 'ZIP'
     O                                           58 'CODE'
     O                                           61 'ST'
     O                                           66 'STS'
     O                                           73 'DATE'
     O                                           79 'POS'
     O                                           83 'TYP'
     O                                           95 'CODE'
     O                                          100 'MOD'
     O                                          106 'UNITS'
     O                                          111 'CODE'
     O                                          123 'CHARGED'
     O                                          137 'REDUCTIONS'
     O                                          151 'REDUCTIONS'
     O                                          165 'REDUCTIONS'
     O                                          179 'REDUCTIONS'
     O                                          193 'PAID'
     O          E            DET            1
     O                       CADSEQ        Z     05
     O               83      CALTR#        Z     15
     O               84      CAEOB#        Z     15
     O               83                          17 'L'
     O                       XXRDAT              26 '  /  /  '
     O                       CAFEIN              36
     O                       CAMOD         Z     40
     O                       HC6N1         Z     47
     O                       CAZIPA              52
     O                       X110A1              56
     O                       X210A1              58
     O                       CAAST               62
     O                       CACSCD              66
     O                       XXFDAT              75 '  /  /  '
     O*** BEGIN CHG *** 02/26/01 **************************************
     O**********               CAIPOP    78
     O                       XXPOS               79
     O*** END   CHG *** 02/26/01 **************************************
     O                       XCTYPE              83
     O*** BEGIN CHG *** 02/26/01 **************************************
     O**********       10N25   XXCMSV    95
     O**********      N10N25   CACPT4    95
     O**********      N10 25   XXNDC     95
     O               11 12   XXCMSV              95
     O               11N12   DHPRCD              95
     O              N11 12   DPNDC               95
     O              N11N12   DDPRCD              95
     O*** END   CHG *** 02/26/01 **************************************
     O*** BEGIN CHG *** 02/26/01 **************************************
     O**********               CAPMOD    99
     O                       XXPMOD              99
     O*** END   CHG *** 02/26/01 **************************************
     O                       XXUNTS        4    106
     O                       CAPART             110
     O                       CACHRG        K    124
     O                       CAMRA         K    138
     O                       CAOTHR        K    152
     O                       CAOTH2        K    166
     O                       CANDIS        K    180
     O                       CAPAID        K    194
     O*** BEG ADD *** 10/25/99 ****************************************
     O          E            DET1A          1
     O                                           27 'RULE #:'
     O                       RULE#         Z     37
     O                       RULEDS             100
     O*** END ADD *** 10/25/99 ****************************************
     O          E            DET2A          1
     O                                           27 'EOB CODE:'
     C**** Beg Chg *** 07/18/23 **************************************
     O****                   DEEOB               31
     O                       DEEOBCD             34
     C**** End Chg *** 07/18/23 **************************************
     O                       DENOT1             110
     O                       DENOT2             181
     O          E            DET3A          1
     O              N86                          27 'ERROR CODE:'
     O              N86      MIMSG#              35
     O                       XXDES1             110
     O                       XXDES2             181
     O          E            TOT         1
     O                                           17 'TOTAL # OF ITEMS:'
     O                       TOTCNT        Z     25
     O                                           90 'TOTAL AMOUNT:'
     O                       TOTCHR        K    124
     O                       TOTDIS        K    138
     O                       TOTOTH        K    152
     O                       TOTOT2        K    166
     O                       TOTNDS        K    180
     O                       TOTPAD        K    194
     O          E            TOT         3
     O               98                          08 '*'
     O               98                          33 'TOTAL PAID REFLECTS ADDI'
     O               98                          55 'TIONAL REDUCTIONS FROM'
     O               98                          79 '"OTHER" AND "NETWORK DI'
     O               98                         101 'SCOUNTS".  USE THE NET'
     O               98                         125 'WORK CHARGES LISTING FOR'
     O               98                         150 'A FULL DISPLAY OF AMOUNT'
     O               98                         152 'S.'
     O          E            ERRMSG      3
     O                                           19 '*** NO RECORDS MEET'
     O                                           41 'SELECTED CRITERIA ***'
