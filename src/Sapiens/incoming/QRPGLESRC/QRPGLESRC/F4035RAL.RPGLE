     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H*    PROGRAM: F4033RAL
     H*    DESCRIPTION: DENTAL ADJUDICATION (ALABAMA RULES - 03/15/99)
     H*   This program is called from F0070R (Claim Adjudication) and will
     H* perform the actual adjudication of the detail claim records writ-
     H* ten by F0070R.  This program was derrived from the Alabama State
     H* Fee Schedule Reimbursment Manual issued on 03/15/98
     H* This Sub-program is intended to be used to calculate the amount
     H* that is due a Provder of services, by the Carrier of the Work comp
     H* insurance.  It should be used for any serice provided on or after
     H* 03/15/99.
     H*****************************************************************
     H*  CREATED: 07/07/99  C:79   REQ/PROJECT: AL036
     H*****************************************************************
     H* Information needed for each year's adjudication program rules:
     H*    1.  Update the effective date range for reading the detail charges
     H*        (found in the mainline section)
     H*****************************************************************
     H*  SPECIALTY CODES USED IN THIS PROGRAM:
     H*****************************************************************
     H*  REVISIONS:
     H*****************************************************************
     H*  INDICATOR USAGE:
     H*  54    Use with Primary FDCAP file (i.e. DOWEQ, etc on reading charges)
     H*  60    General Use
     H*  61    General Use
     H*  62    General Use
     H*****************************************************************
     H*  SECTION                                 CHPT   SUBR         CODES
     H*  Dental                                         DENT    Prefixed w "D"
     H*****************************************************************
     F*  FDCAL7 is the Primary File Read (PCA01)
     FFDCAL7    UF   E           K DISK
     FFDCALP    IF   E           K DISK
     F                                     RENAME(PCA01:PCA0P)
     FFDCAL6    IF   E           K DISK
     F                                     RENAME(PCA01:PCA06)
     FFMCML1    IF   E           K DISK
     FFMGZL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFMPRLAL   IF   E           K DISK
     FFMRCL     IF   E           K DISK
     FFTABLAL   IF   E           K DISK
     F                                     IGNORE(PRC01)
     FSMUPL     IF   E           K DISK

     D*
     D* "Entry" Arrays
     D QQS             S              5    DIM(30)                              DETAIL SEQ #
     D QQC             S              2    DIM(30)                              ERROR MESSAGES
     D QQF             S              6    DIM(50)                              FROM DATES
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*  Date/Time Data Structure.
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0

     I*
     I*
     IPCA0P
     I              CACO#                       PXCO#
     I              CASUB#                      PXSUB#
     I              CAFND                       PXFND
     I              CAFYR                       PXFYR
     I              CACASE                      PXCASE
     I              CAFDAT                      PXFDAT
     I              CADSEQ                      PXDSEQ
     I              CAFEIN                      PXFEIN
     I              CAMOD                       PXMOD
     I              CACPT4                      PXCPT4
     I              CATOTA                      PXTOTA
     I              CATOTD                      PXTOTD
     I              CATOTP                      PXTOTP
     I              CATOTC                      PXTOTC
     I              CAPMOD                      PXPMOD
     I              CAUSER                      PXUSER
     I              CAPGM                       PXPGM
     I              CADT                        PXDT
     I              CATIME                      PXTIME
     I              CACHK#                      PXCHK#
     I              CAEOB#                      PXEOB#
     I              CADIFF                      PXDIFF
     I              CAPART                      PXPART
     I              CADXR                       PXDXR
     I              CAFLAG                      PXFLAG
     I*
     I*  Rename the duplicate field names.
     IPCA06
     I              CACPT4                      X6CPT4
     I              CACO#                       X6CO#
     I              CASUB#                      X6SUB#
     I              CAFND                       X6FND
     I              CAFYR                       X6FYR
     I              CACASE                      X6CASE
     I              CAFDAT                      X6FDAT
     I              CADSEQ                      X6DSEQ
     I              CAFEIN                      X6FEIN
     I              CAMOD                       X6MOD
     I              CATOTA                      X6TOTA
     I              CATOTC                      X6TOTC
     I              CATOTD                      X6TOTD
     I              CATOTP                      X6TOTP
     I              CAPMOD                      X6PMOD
     I              CAUSER                      X6USER
     I              CAPGM                       X6PGM
     I              CADT                        X6DT
     I              CATIME                      X6TIME
     I              CACHK#                      X6CHK#
     I              CAEOB#                      X6EOB#
     I              CADIFF                      X6DIFF
     I              CAPART                      X6PART
     I              CADXR                       X6DXR
     I              CAFLAG                      X6FLAG

     C*
     C*  Claim Detail File
     C     CAKY1         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C*
     C*  CLAIM DETAIL FILE
     C     CAKY1A        KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C                   KFLD                    SVPRTY
     C                   KFLD                    SVDSEQ
     C                   KFLD                    SVEOB#
     C                   KFLD                    SVCHK#
     C*
     C     CAKYZ         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    HLFEIN
     C                   KFLD                    HLMOD
     C                   KFLD                    HLPRTY
     C                   KFLD                    HLDSEQ
     C                   KFLD                    HLEOB#
     C                   KFLD                    HLCHK#
     C*
     C     CAKY9         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C*
     C     CAKYP         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C     CAKY6         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C                   KFLD                    XXYMD
     C                   KFLD                    CAFEIN
     C                   KFLD                    CAMOD
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXPMOD
     C*
     C*  Claim Master File
     C     CMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQSUB#
     C                   KFLD                    QQFND
     C                   KFLD                    QQFYR
     C                   KFLD                    QQCASE
     C*
     C*  Procedure Code Modifier File
     C     PMKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  Claim Status Code Table File
     C     TCSKY         KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    TABKEY
     C*
     C*  Dummy key used for release of all CA records
     C     DUMMY         KLIST
     C                   KFLD                    QQCO#
     C*
     C*** Geographical Zip Area FILE
     C     GZKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXFRZP
     C*
     C*  Procedure Code file
     C     PRKY          KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    XXCPT4
     C                   KFLD                    XXEFDT
     C*
     C*  Provider Code File
     C     PDKY          KLIST
     C                   KFLD                    SVFEIN
     C                   KFLD                    SVMOD
     C*
     C* Reasonable & Customary File
     C     RCKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    XXZIPA
     C                   KFLD                    XXCPCD
     C                   KFLD                    XXYMD
     C*
     C*  User Parameter File
     C     UPKY          KLIST
     C                   KFLD                    Q1USER
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQFYR             3 0
     C                   PARM                    QQCASE            7 0
     C                   PARM                    QQFDT           150
     C                   PARM                    QQFDT2          150
     C                   PARM                    QQS
     C                   PARM                    QQC
     C                   PARM                    QQRTCD            1
     C                   PARM                    QQSS#             9 0          CHAIN TO EE
     C                   PARM                    QQACDT            8 0          (MMDDYY ORD)
     C                   PARM                    QQSPCD            3
     C                   PARM                    QQCO#             3 0
     C                   PARM                    QQSUB#            5 0
     C                   PARM                    QQFND             3 0
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
     C     QQRTCD        CABEQ     'P'           TERM                           PRE-OPEN FILES
     C*
     C                   MOVEA     QQFDT         QQF(1)
     C                   MOVEA     QQFDT2        QQF(26)
     C*
     C* = Initialization Section ======================================
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         SYDATE            7 0
     C*
     C     *LIKE         DEFINE    CACHK#        HLCHK#
     C     *LIKE         DEFINE    CADSEQ        HLDSEQ
     C     *LIKE         DEFINE    CAEOB#        HLEOB#
     C     *LIKE         DEFINE    CAFEIN        HLFEIN
     C     *LIKE         DEFINE    CAFEIN        HDFEIN
     C     *LIKE         DEFINE    CAFDAT        HLFDAT
     C     *LIKE         DEFINE    CAFDAT        HDYMD
     C     *LIKE         DEFINE    CAFDAT        SVYMD
     C     *LIKE         DEFINE    CAMOD         HLMOD
     C     *LIKE         DEFINE    CAMOD         HDMOD
     C     *LIKE         DEFINE    CAPRTY        HLPRTY
     C*
     C     *LIKE         DEFINE    CADSEQ        H1DSEQ
     C*
     C     *LIKE         DEFINE    CAFDAT        H2FDAT
     C     *LIKE         DEFINE    CAPMOD        H2PMOD
     C*
     C     *LIKE         DEFINE    PRANAM        SXANAM
     C     *LIKE         DEFINE    CAFEIN        SXFEIN
     C     *LIKE         DEFINE    CAMOD         SXMOD
     C     *LIKE         DEFINE    PRMRA         SXMRA
     C     *LIKE         DEFINE    PRPROF        SXPROF
     C     *LIKE         DEFINE    PRRPT         SXRPT
     C     *LIKE         DEFINE    PRSUPP        SXSUPP
     C     *LIKE         DEFINE    PRSURG        SXSURG
     C     *LIKE         DEFINE    PR92N1        SX92N1
     C     *LIKE         DEFINE    PR1A1         SX1A1
     C     *LIKE         DEFINE    PR1A2         SX1A2
     C     *LIKE         DEFINE    PR1A3         SX1A3
     C     *LIKE         DEFINE    PR1A4         SX1A4
     C     *LIKE         DEFINE    PR1A5         SX1A5
     C*
     C     *LIKE         DEFINE    CACHK#        SVCHK#
     C     *LIKE         DEFINE    CADSEQ        SVDSEQ
     C     *LIKE         DEFINE    CAFDAT        SVDT
     C     *LIKE         DEFINE    CAEOB#        SVEOB#
     C     *LIKE         DEFINE    CAFEIN        SVFEIN
     C     *LIKE         DEFINE    CAMOD         SVMOD
     C     *LIKE         DEFINE    CAPRTY        SVPRTY
     C     *LIKE         DEFINE    CAUNTS        SVUNTS
     C*
     C     *LIKE         DEFINE    PMKEY         TABKEY
     C*
     C     *LIKE         DEFINE    CABRTC        XPBRTC
     C     *LIKE         DEFINE    CACPT4        XPCPT4
     C     *LIKE         DEFINE    CADIFF        XPDIFF
     C     *LIKE         DEFINE    CAEOMB        XPEOMB
     C     *LIKE         DEFINE    CAPMOD        XPPMOD
     C     *LIKE         DEFINE    CATOTA        XPTOTA
     C     *LIKE         DEFINE    CATOTC        XPTOTC
     C     *LIKE         DEFINE    CATOTD        XPTOTD
     C     *LIKE         DEFINE    CATOTP        XPTOTP
     C*
     C     *LIKE         DEFINE    CACPT4        XXCPT4
     C     *LIKE         DEFINE    CACPT4        XXCPCD
     C     *LIKE         DEFINE    CAFDAT        XXDATE
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CAFDAT        XXEFDT
     C     *LIKE         DEFINE    GZFRZP        XXFRZP
     C     *LIKE         DEFINE    PRMRA         XXMRA
     C     *LIKE         DEFINE    CAPART        XXPART
     C     *LIKE         DEFINE    CAPMOD        XXPMOD
     C     *LIKE         DEFINE    CAANMN        XXUNIT
     C     *LIKE         DEFINE    CAFDAT        XXYMD
     C     *LIKE         DEFINE    GZZIPA        XXZIPA
     C*
     C     *LIKE         DEFINE    CAFDAT        WKFDAT
     C*
     C                   Z-ADD     0             Q                 3 0
     C                   Z-ADD     0             S                 3 0
     C                   Z-ADD     0             T                 5 0
     C                   MOVE      *BLANKS       SVSPCD            3
     C                   MOVE      *BLANKS       XXEND             2
     C                   Z-ADD     0             XXOVRP            3 2
     C                   Z-ADD     0             XXPERC            5 2
     C                   MOVE      '0'           XX25EM            1
     C                   MOVE      *BLANKS       QQTYPE            1            ADD 06/23/93
     C*
     C* ===============================================================
     C*
     C*  Get the user parameter file
     C     UPKY          CHAIN     PUP01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99999'       QQS(1)
     C                   MOVE      '95'          QQC(1)
     C* Err Msg 95: "Invalid User Parameter, See Programmer"
     C                   GOTO      TERM
     C                   END
     C*
     C*  Get the Claim Master Record
     C     CMKY          CHAIN     PCM01                              60
     C*
     C*  If the get was unsuccessful, end the program
     C     *IN60         IFEQ      '1'
     C                   MOVE      '99998'       QQS(1)
     C                   MOVE      '96'          QQC(1)
     C* Err Msg 96: Invalid Case Number, See Programmer
     C                   GOTO      TERM
     C                   END
     C*
     C*** Do on each date brought in ********************************
     C*
     C*  Do adjudication for each possible date entered,if after 03/15/97
     C                   DO        50            S
     C                   MOVE      QQF(S)        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXYMD
     C*
     C     QQF(S)        IFNE      *ZERO
     C     XXYMD         ANDGE     990315
     C     CAKY1         CHAIN     PCA01                              54
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        SVFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         SVMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C                   MOVE      CAFEIN        SXFEIN                         SET PROVIDER WE
     C                   MOVE      CAMOD         SXMOD                          ARE PROCESSING
     C*
     C                   MOVE      QQSPCD        SVSPCD
     C*
     C*** DOWEQ on FDCAP file****************************************
     C*
     C     *IN54         DOWEQ     '0'                                          DO FOR EACH RCD
     C                   EXCEPT    ECA01
     C*
     C* If the Service Date is greater than the system date, give err message
     C     CAFDAT        IFGT      SYDATE
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '79'          QQC(Q)
     C*** Err Msg 79: "Service Date entered is greater than today's date
     C*                (system date)."
     C                   END                                                    END EQ *ZEROS
     C                   END                                                    END GT SYDATE
     C*
     C                   MOVE      '0'           XX25EM
     C*
     C*  Get the claim status record.  See if Override Flag is 'Y'.
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CACSCD        TABKEY
     C     TCSKY         CHAIN     PCS01                              60
     C*
     C     *IN60         IFEQ      '1'
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '97'          QQC(Q)
     C* Err Msg 97: "Invalid Claim Status Code, See Programmer"
     C                   END
     C*
     C*** NOT OVERRIDE ************************************************
     C*
     C* If override flag is not equal to 'Y', then process this record
     C*    Else do not adjudicate.
     C*
     C     CSOVRD        IFNE      'Y'
     C*
     C*== R&C =========================================================
     C* Check to see if this record is a Reasonable/Customary record
     C     CARCFL        IFEQ      'Y'                                           REAS/CUST
     C*
     C                   MOVEL     CACPT4        XXCPCD
     C                   MOVE      *BLANKS       XPEOMB
     C                   MOVE      *ZEROS        XPTOTA
     C                   MOVE      *ZEROS        XPTOTP
     C                   MOVE      *ZEROS        XPTOTD
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   MOVE      *BLANKS       XXFRZP
     C                   MOVE      *BLANKS       XXZIPA
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C* Now that the Provider is established, get its Geographic Zip Area
     C* 1st - establish the first 3-bytes of the zip code
     C                   MOVEL     PDZIP         XXFRZP
     C                   END                                                    END 0 *IN60
     C*
     C* 2nd - Get the corresponding Geographic Zip Area record:
     C     GZKY          SETGT     PGZ01
     C                   READP     PGZ01                                  60
     C     *IN60         IFEQ      '0'
     C     XXFRZP        ANDLE     GZTOZP
     C                   MOVE      GZZIPA        XXZIPA
     C                   END                                                    END 0 *IN60
     C*
     C     RCKY          SETGT     PRC01
     C     NXRC1         TAG
     C                   READP     PRC01                                  60
     C     *IN60         IFEQ      '0'
     C     RCCO#         ANDEQ     QQCO#
     C     RCZIPA        ANDEQ     XXZIPA
     C     RCCPT4        ANDEQ     XXCPCD
     C     RCSTAT        IFNE      'I'
     C                   Z-ADD     RCMRA         XCMRA
     C*
     C*** IF PROCEDURE IS "BY-REPORT" FLAG IT AS SUCH AND UPDATE RECORD.
     C     RCRPT         IFEQ      'Y'
     C*** IF MODIFIER EXISTS CHECK THE CODE THROUGH THE MODIFIER SEC.
     C     CAPMOD        IFNE      *BLANK
     C*
     C                   EXSR      PMOD
     C     CATOTC        COMP      XXMRA                              60
     C   60              Z-ADD     XXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     XXMRA         XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C*
     C                   ELSE                                                   ELSE CAPMOD NE *BLAN
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C                   END
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END                                                    END CAPMOD NE *BLANK
     C*
     C                   ELSE                                                   ELSE RCRPT EQ 'Y'
     C*
     C     CATOTC        COMP      RCMRA                              61
     C   61              MOVE      '207'         XPEOMB
     C* EOMB Code 207: "This charge has been paid per R&C fees"
     C   61              Z-ADD     RCMRA         XPTOTA
     C  N61              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   END                                                    END RCRPT EQ 'Y'
     C*
     C                   ELSE                                                    ELSE RCSTAT NE "I"
     C                   GOTO      NXRC1
     C                   END                                                     END RCSTAT NE "I"
     C                   END                                                     END 0 *IN60
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     *IN54         IFEQ      '0'
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD
     C                   END
     C*
     C                   END                                                     END CARCFL = Y
     C*
     C*== =============================================================
     C*
     C*   Check for level break (Provider)
     C     SXFEIN        IFNE      CAFEIN
     C     SXMOD         ORNE      CAMOD
     C                   EXSR      NEWPRO
     C                   END
     C*
     C*== Hospital ====================================================
     C*
     C*   Hospital Subroutine or Regular Providers.
     C     CAPRTY        IFEQ      '4'
     C                   GOTO      NXTRCD
     C*
     C*== Non-Hospital ================================================
     C*
     C                   ELSE                                                   Else eq '4'
     C*
     C*  Test for Fee Schedule Reimbursement = 'Y'
     C                   MOVEL     CACPT4        XXCPT4
     C                   MOVE      CAFDAT        XXEFDT
     C*
     C     PRKY          SETGT     PPR01                                         GET PROC CODE
     C     NXTPR2        TAG
     C                   READP     PPR01                                  60     RECORD
     C*
     C*  A hard error will cause the program to terminate ASAP.
     C     *IN60         IFEQ      '1'
     C     PRCPT4        ORNE      CACPT4
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '98'          QQC(Q)
     C* Err Msg 98: "Invalid Procedure Code, See Programmer"
     C                   GOTO      TERM
     C                   END
     C     PRSTAT        CABEQ     'I'           NXTPR2
     C                   Z-ADD     PRMRA         XCMRA
     C*
     C*== FS Reimbursement = "N" ======================================
     C*
     C*  If fee schedule reimbursement equals 'N', but the MRA is equal
     C*     to zero, take the information as is.
     C     PRFSR         IFEQ      'N'
     C*
     C*  Flag to display Fee Schedule non-reimburseable Error Message
     C                   MOVE      '1'           XX25EM
     C*
     C     PRMRA         IFEQ      *ZEROS
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   EXSR      UPDAT
     C*
     C                   ELSE
     C*
     C*  If fee schedule reimbursement equals 'N' But the MRA
     C*    is greater than zero, this CPT4 Code has been assigned a
     C*    specific MRA by the insurance company.
     C     CATOTC        COMP      PRMRA                              60
     C   60              MOVE      '202'         XPEOMB
     C* EOMB Code 202: "Charges exceed Reasonable & Customary"
     C   60              Z-ADD     PRMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END
     C                   END
     C*
     C*====================================================================
     C*== FS Reimbursement = "Y" ==========================================
     C*
     C*  If Fee Schedule Reimbursement equals 'Y', use MRA
     C*
     C*  Save the procedure code file values.
     C     PRFSR         IFEQ      'Y'
     C                   Z-ADD     PRMRA         SXMRA
     C                   Z-ADD     PRANAM        SXANAM
     C                   Z-ADD     PRPROF        SXPROF
     C                   MOVE      PRRPT         SXRPT
     C                   MOVE      PRSUPP        SXSUPP
     C                   MOVE      PRSURG        SXSURG
     C                   MOVE      PR92N1        SX92N1
     C                   MOVE      PR1A1         SX1A1
     C                   MOVE      PR1A2         SX1A2
     C                   MOVE      PR1A3         SX1A3
     C                   MOVE      PR1A4         SX1A4
     C                   MOVE      PR1A5         SX1A5
     C*
     C*--------------------------------------------------------------------
     C*-- By Report -------------------------------------------------------
     C     PRRPT         IFEQ      'Y'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CATOTC        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '91'          QQC(Q)
     C* Err Msg 91: "Carrier to set payment on individual case basis
     C*             based on documentation"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'Y'
     C*
     C*--------------------------------------------------------------------
     C*-- ANE/Amount Not Established --------------------------------------
     C     PRRPT         IFEQ      'A'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C*
     C     CATOTC        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   ELSE
     C*
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '24'          QQC(Q)                         ANE MESSAGE
     C* Err Msg 24: "The item is ANE - requires further inspection
     C*             by an adjuster"
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*--------------------------------------------------------------------
     C*-- Possible Bundled Codes ------------------------------------------
     C     PRRPT         IFEQ      'B'
     C*
     C*   If modifier exists check the code through the Modifier Subr
     C     CAPMOD        IFNE      *BLANK
     C                   EXSR      PMOD
     C                   END
     C*
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '205'         XPEOMB
     C* EOMB Code 205: "This procedure is included in the basic allowance
     C*                 of another procedure"
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C*
     C                   END
     C                   END                                                    END PRRPT EQ 'A'
     C*
     C*================================================================
     C*== Normal Processing of Procedure Code =========================
     C*
     C*-----------------------------------------------------------------
     C*-- Give Message for codes that end in '99' ---------------------
     C                   MOVE      CACPT4        XXEND
     C     XXEND         IFEQ      '99'
     C     CACHK#        IFEQ      *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    END CHK#/EOB# EQ 0
     C                   END                                                    END XXEND EQ '99'
     C*
     C*-----------------------------------------------------------------
     C*-- Execute all Subroutines here ---------------------------------
     C*
     C                   Z-ADD     1             T
     C     1             SUBST     CACPT4:T      DENTFL            1    65
     C     DENTFL        IFEQ      'D'
     C                   EXSR      DENT
     C*
     C                   ELSE
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      'AE'          QQC(Q)
     C* Err Msg AE: "This code should be submitted on a HCFA form."
     C                   END                                                    END EQ *ZEROS
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C*
     C                   EXSR      UPDAT
     C                   GOTO      NXTRCD                                       GET NEXT RECORD
     C                   END                                                    END DENTFL
     C*
     C                   END
     C*
     C*** ELSE IS OVERRIDE ********************************************
     C                   ELSE                                                   OVERRIDE TYPE
     C*
     C*  Test for a possible duplicate claim.
     C     CACPT4        IFNE      *BLANK
     C     CAEOB#        ANDEQ     *ZERO
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVEL     CAPMOD        XXPMOD
     C     CAKYP         CHAIN     PCA0P                              60
     C*
     C     DUPCL1        TAG
     C     *IN60         IFEQ      '0'
     C*
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C                   ELSE
     C*
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL1
     C                   END
     C                   END
     C                   END
     C*
     C     CAKY1A        CHAIN     PCA01                              60
     C     *IN60         IFEQ      '0'
     C*   If override type, we must still output the allowed/disallowed
     C     CAEOB#        IFEQ      *ZERO
     C     CATOTD        ANDEQ     0
     C     CAOTHR        ANDEQ     0
     C     CANDIS        ANDEQ     0
     C     CATOTA        ANDEQ     0
     C     CATOTP        ANDEQ     0
     C*
     C*   If there is an override percentage, calculate
     C     CSOVRP        IFGT      *ZERO
     C     CSOVRP        DIV       100           XXOVRP
     C     CATOTC        MULT      XXOVRP        CATOTA
     C                   END
     C*
     C     CATOTC        SUB       CATOTA        CATOTD
     C                   Z-ADD     CATOTA        CATOTP
     C                   END
     C                   Z-ADD     *ZEROS        CADIFF
     C                   Z-ADD     *ZEROS        CABRTC
     C*
     C                   Z-ADD     XCMRA         CAMRAA
     C                   UPDATE    PCA01
     C*
     C                   END
     C                   END                                                    END EQ OVRRD
     C*
     C*** Process next record *****************************************
     C     NXTRCD        TAG
     C*
     C*
     C     CAKY1         READE     PCA01                                  54      GET NEXT RCD
     C*                                                      TO PROCESS
     C*
     C     *IN54         IFEQ      '0'                                          SAVE FIELDS OF
     C                   Z-ADD     CADSEQ        SVDSEQ                         CURRENT REC.
     C                   MOVE      CAFEIN        SVFEIN
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAUNTS        SVUNTS
     C                   MOVEL     CACPT4        XPCPT4
     C                   MOVEL     CAPMOD        XPPMOD
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C                   END
     C*
     C                   END
     C*
     C                   END
     C                   END
     C*
     C*  End the program
     C     TERM          TAG
     C*
     C*  Release all the formats for a save return.
     C*
     C     DUMMY         CHAIN     PCA01                              54
     C  N54              EXCEPT    ECA01
     C*---------------------------------------------------------------------
     C*
     C                   Z-ADD     0             XCMRA             9 2
     C                   RETURN
     C                   MOVE      '1'           *INLR
     *********************************************************************
     *** D E N T A L                Codes: PREFIXED WITH A "D"           *
     *********************************************************************
     C     DENT          BEGSR
     C*
     C*====================================================================
     C*--- Valid suffixes: ------------------------------------------------
     C     CAPMOD        IFNE      *BLANK
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '10'          QQC(Q)
     C* Err Msg 10: "Invalid modifier used with this item.
     C*             Re-enter item w/valid modifier."
     C                   Z-ADD     CATOTC        XPTOTD
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      '201'         XPEOMB
     C* EOMB Code 201: "Service is not reimburseable under the Workers
     C*                 Compensation Program"
     C                   EXSR      UPDAT
     C                   GOTO      DENTE
     C                   END                                                    END *BLANKS
     C*
     C* ===============================================================
     C* Calculate Payment
     C     CATOTC        COMP      SXMRA                              60
     C   60              Z-ADD     SXMRA         XPTOTA
     C  N60              Z-ADD     CATOTC        XPTOTA
     C     CATOTC        SUB       XPTOTA        XPTOTD
     C                   Z-ADD     XPTOTA        XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     SXMRA         XPBRTC
     C*
     C* ===============================================================
     C* Update charge with payment
     C                   EXSR      UPDAT
     C* ===============================================================
     C*
     C     DENTE         ENDSR
     *********************************************************************
     *** NEW PROVIDER SUBROUTINE (LEVEL BREAK)               N E W P R O *
     *********************************************************************
     C     NEWPRO        BEGSR
     C*
     C                   Z-ADD     CADSEQ        HLDSEQ                         SAVE FIELD VALS
     C                   MOVE      CAFEIN        HLFEIN                         FOR RECORD WE
     C                   MOVE      CAMOD         HLMOD                          ARE PROCESSING
     C                   MOVE      CAPRTY        HLPRTY
     C                   Z-ADD     CAEOB#        HLEOB#
     C                   Z-ADD     CACHK#        HLCHK#
     C*
     C                   Z-ADD     HLDSEQ        CADSEQ                         SAVE FIELD VALS
     C                   MOVE      HLFEIN        CAFEIN                         FOR RECORD WE
     C                   MOVE      HLMOD         CAMOD                          ARE PROCESSING
     C                   MOVE      HLPRTY        CAPRTY
     C                   Z-ADD     HLEOB#        CAEOB#
     C                   Z-ADD     HLCHK#        CACHK#
     C*   Reget record.
     C     CAKYZ         CHAIN     PCA01                              54
     C*
     C  N54              EXCEPT    ECA01
     C*
     C                   Z-ADD     CADSEQ        SVDSEQ
     C                   MOVE      CAFEIN        SVFEIN                            CURR REC.
     C                   MOVE      CAMOD         SVMOD
     C                   MOVE      CAPRTY        SVPRTY
     C                   Z-ADD     CAEOB#        SVEOB#
     C                   Z-ADD     CACHK#        SVCHK#
     C*
     C*----------------------------------------------------------------
     C* Get and save the Provider's Specialty Code
     C     PDKY          CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C                   MOVE      QQSPCD        SVSPCD                         CHG 12/28/92
     C                   END
     C*
     C                   MOVE      CAFEIN        SXFEIN
     C                   MOVE      CAMOD         SXMOD
     C*
     C     NEWPRE        ENDSR
     *********************************************************************
     *** Procedure Code Modifier                                 P M O D *
     *********************************************************************
     C     PMOD          BEGSR
     C*
     C                   MOVE      *BLANK        TABKEY
     C                   MOVEL     CAPMOD        TABKEY
     C*
     C     PMKY          CHAIN     PPM01                              64
     C*
     C     PMPERC        IFNE      *ZERO
     C     PMPERC        DIV(H)    100           XXPERC            5 2
     C                   END
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 60
     C  N60              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '76'          QQC(Q)
     C*** Err Msg 76: "CPT modifier used with this charge.  Verify use
     C***              and payment."
     C                   END
     C*
     C     PMODE         ENDSR
     *********************************************************************
     *** Update                                                U P D A T *
     *********************************************************************
     C     UPDAT         BEGSR
     C*
     C* ===============================================================
     C* Establish default EOMB code, if necessary
     C     XPTOTD        IFGT      *ZERO
     C     XPTOTP        ANDGT     *ZERO
     C     XPEOMB        ANDEQ     *BLANK
     C                   MOVE      '202'         XPEOMB
     C*** EOMB 202: "Charges exceed Maximum Allowance found in reimburse manual"
     C                   END
     C*
     C* ===============================================================
     C** TEST FOR A POSSIBLE DUPLICATE CLAIM.
     C*
     C     CACPT4        IFNE      *BLANK
     C     CAEOB#        ANDEQ     *ZERO
     C                   MOVE      CACPT4        XXCPT4
     C                   MOVE      CAPMOD        XXPMOD
     C     CAKYP         CHAIN     PCA0P                              60
     C     DUPCL2        TAG
     C*
     C     *IN60         IFEQ      '0'
     C     CADSEQ        IFNE      PXDSEQ
     C     CADSEQ        OREQ      PXDSEQ
     C     CAEOB#        ANDNE     PXEOB#
     C*
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '03'          QQC(Q)
     C*** Err Msg 03: "Possible Duplicate Claim exists for this item"
     C                   END
     C                   ELSE
     C*
     C     CAKYP         READE     PCA0P                                  60
     C  N60              GOTO      DUPCL2
     C                   END
     C                   END
     C                   END
     C*
     C* ===============================================================
     C** Only process the record if not an override type
     C*
     C     CAKY1A        CHAIN     PCA01                              54
     C     CSOVRD        IFNE      'Y'
     C     CACHK#        ANDEQ     *ZERO
     C     CAEOB#        ANDEQ     *ZERO
     C                   Z-ADD     XPTOTA        CATOTA
     C                   Z-ADD     XPTOTD        CATOTD
     C                   Z-ADD     XPTOTP        CATOTP
     C                   Z-ADD     XPDIFF        CADIFF
     C                   Z-ADD     XPBRTC        CABRTC
     C                   MOVE      XPPMOD        CAPMOD
     C                   MOVE      XPEOMB        CAEOMB
     C*
     C                   MOVE      Q1USER        CAUSER
     C                   MOVE      Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   Z-ADD     DSTIME        CATIME
     C*
     C*
     C                   Z-ADD     XCMRA         CAMRAA
     C  N54              UPDATE    PCA01
     C* If Fee Schedule non-reimburseable ... display error message
     C* XX25EM Flag is set at beginning of program where PRFSR eq 'N'
     C     XX25EM        IFEQ      '1'
     C                   MOVE      CACPT4        XXCPT4
     C     CAKY6         CHAIN     PCA06                              60
     C     *IN60         IFEQ      '0'
     C     CAEOB#        IFEQ      *ZEROS
     C     CACHK#        ANDEQ     *ZEROS
     C                   Z-ADD     1             Q
     C     '00000'       LOOKUP    QQS(Q)                                 61
     C  N61              Z-ADD     30            Q
     C                   MOVEL     CADSEQ        QQS(Q)
     C                   MOVE      '19'          QQC(Q)
     C* Err Msg 19: *WARNING* Need Letter of Medical Necessity to pay this item
     C                   END                                                    ENDIF *IN61
     C                   MOVE      '0'           XX25EM
     C                   END                                                    ENDIF *IN60
     C                   END                                                    ENDEQ '1'
     C                   END
     C*
     C* ===============================================================
     C*
     C** Clear the work fields
     C                   Z-ADD     *ZERO         XPTOTA
     C                   Z-ADD     *ZERO         XPTOTD
     C                   Z-ADD     *ZERO         XPTOTP
     C                   Z-ADD     *ZEROS        XPDIFF
     C                   Z-ADD     *ZEROS        XPBRTC
     C                   MOVE      *BLANKS       XPPMOD
     C                   MOVE      *BLANK        XPEOMB
     C* ===============================================================
     C*
     C     UPDAE         ENDSR
     *********************************************************************
     OPCA01     E            ECA01
     *********************************************************************
