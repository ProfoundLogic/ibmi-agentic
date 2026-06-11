     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
      /TITLE U0060R02 - PRE-CERTIFICATION MAINTENANCE
      * ***************************************************************
      * DESCRIPTION :
      *    TO PRE-CERTIFY CHARGES, DATES, SERVICES AND LENGTH         *
      *    OF TIME BEFORE BILL IS TO BE ADJUDICATED.                  *
      *                                                               *
      *  CALLED FROM:                                                 *
      *    UTILIZATION/REVIEW MENU                                    *
      *    WORK WITH MEDICAL CASES - OPT 15                           *
      *                                                               *
      *  CREATED:  2/25/98   47    87MCB                              *
      * ********************************************************
      * REVISIONS:
      *
      *  06/09/20 C:212 Project : SY420
      *                 Mask SSN
      *                 Recompile Only
      *
      *  05/26/99 C:62  Project: MC145
      *                 F8=BILL HISTORY.
      *
      *  01/12/99 C:62  Project: 25MKK
      *                 MOVE WRITE TO FAEOP, FAE1P AND NAPBP TO THIS
      *                 PROGRAM.
      *
      *  09/26/98 C:53  Project: MC101 # 13633
      *                 REPLACE CALLS TO OBSOLETE PGM F0031R WITH      r
      *                 WINDOW SELECTION VIA F2014R
      *
      *  08/21/98 C:62 PROJECT: MC172
      *                - REMOVED REVIEW DATE.
      *
      *  08/06/98  C:94  PROJECT: MCMNT
      *                  FIXED POSITION OF FUNCTION KEYS IN DISPLAY
      *                  FILE.
      *
      * ********************************************************
      * INDICATORS:
      *
      * ********************************************************
     FU0060F02  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FUMPCP     UF   E             DISK
     F                                     RENAME(PPC01:DBRCDF)
     F                                     INFDS(DBFDS)
     FUMPCL     IF   E           K DISK
     FFMPDL     IF   E           K DISK
      *  GENERIC TABLE FILE
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT0P)
     F                                     INFDS(DBFDS3)
     F*** BEGIN ADD *** 01/12/99 **************************************
     F*** END   ADD *** 01/12/99 **************************************
     F*** BEGIN DEL *** 01/12/99 **************************************
     F**********FFMC1L   UF  E           K        DISK
     F**********FAEOP   O   E                    DISK
     F**********FAE1P   O   E                    DISK
     F**********NAPBP   O   E                    DISK
     F*** END   DEL *** 01/12/99 **************************************
     F*** BEG ADD *** 09/26/98 ****************************
     FFMCML1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     FFAPPP     IF   E             DISK
     F*** END ADD *** 09/26/98 ****************************
      *
      *
      ***PY SCOPYMT01I
      *
      *  UMPCP
      /COPY SCOPYMT01D
     D DBFDS           DS
     D  FLSTPC           *STATUS
     D  DBRRN                397    400B 0
      *
      *  FMCML1
     D DBFDS2          DS
     D**                                     *STATUS  FLSTCM
     D  DBRRN2               397    400B 0
      *
      *  FTGTP
     D DBFDS3          DS
     D  DBRRN3               397    400B 0
      *
     D**            'Work with Pre--      C         OPTNM
     D**            'Certifications'
     D OPTNM           C                   CONST('Pre-Certification -
     D                                     Information')
     D OPTNMR          C                   CONST('Re-Certification -
     D                                     Information')
     D COMAPI          C                   CONST('FA007R')
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
      * ===================================================================
      *

     C*** BEGIN DEL *** 01/12/99 **************************************
     C*
     C*   KEY LIST FOR THE COMPANY AUXILIARY FILE
     C*
     C********** C1KY      KLIST
     C**********           KFLD           XXCO#
     C*** END   DEL *** 01/12/99 **************************************
     C*
     C* Xref to WCMS Fund # to get WCMC Sub#/Fund
     C*
     C     FNKEY3        KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LIST FOR THE GENERIC TABLE FILE
     C*
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C*   KEY LIST FOR THE PRE-CERTIFICATION MASTER FILE
     C*
     C     PCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C*
     C     PCKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXCERT
     C                   KFLD                    XXEXT
     C*
     C*  PAYEE DEMOGRAPHIC RECORD
     C*
     C     PDKEY         KLIST
     C                   KFLD                    XXFEIN
     C                   KFLD                    XXMOD
      *
     C*** BEG ADD *** 09/26/98 ****************************
      *
      *  CLAIM MASTER FILE
      *
     C     CMKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXSUB#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
      *
     C*** END ADD *** 09/26/98 ****************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    QQCONT            1
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*** BEG ADD *** 09/26/98 ****************************
     C* PROVIDER SELECTION
     C*
     C     PROVSL        PLIST
     C                   PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZERO         ##RRN             9 0          FAPPP
     C                   PARM                    ##RRN2            9 0          FMCMP
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANK        ##RTCD            1
     C*
     C*** END ADD *** 09/26/98 ****************************
     C*
     C* INITIALIZE WORK FIELDS
     C*
     C     *LIKE         DEFINE    PCCO#         XXCO#                          3.0N
     C     *LIKE         DEFINE    PCSUB#        XXSUB#                         5.0N
     C     *LIKE         DEFINE    PCFND         XXFND                          3.0N
     C     *LIKE         DEFINE    PCFYR         XXFYR                          3.0N
     C     *LIKE         DEFINE    PCCASE        XXCASE                         7.0N
     C     *LIKE         DEFINE    PCCERT        XXCERT                         15 A
     C     *LIKE         DEFINE    PCEXT         XXEXT                          3.0N
     C     *LIKE         DEFINE    PCFEIN        XXFEIN                         9.0N
     C     *LIKE         DEFINE    PCMOD         XXMOD                          3.0N
     C     *LIKE         DEFINE    PCACDT        XXACDT
     C     *LIKE         DEFINE    PCACDT        XXDT
     C     *LIKE         DEFINE    PCACDT        XXEFDT
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         QQKEY
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        QQTBID
     C     *LIKE         DEFINE    GTTBID        @@TBID
     C     *LIKE         DEFINE    PCCAT         XXCAT                          1.0N
     C     *LIKE         DEFINE    PCRECD        XXTREC          + 2
     C     *LIKE         DEFINE    PCREQD        XXTREQ          + 2
     C     *LIKE         DEFINE    PCCERD        XXTCER          + 2
     C*** BEGIN DEL *** 01/12/99 **************************************
     C********** *LIKE     DEFN EOEOB#    XXEOB#           9.0N
     C*** END   DEL *** 01/12/99 **************************************
     C*
      /COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   DO
     C*
     C* VALIDATE REVIEW DATE
     C*
     C*** BEGIN DEL *** 08/21/98 **************************************
     C********** SCRVDT    IFNE *ZEROS
     C**********           Z-ADDSCRVDT    SRDTI
     C**********           CALL 'SRCVTDAT'DATCHK
     C**********           MOVELSRDTO     SCRVDT
     C**********
     C********** SRERR     IFEQ '1'
     C**********           MOVE *ON       *IN71
     C**********           MOVE *ON       *IN90
     C**********           LEAVE
     C**********           END
     C**********
     C**********           END                             END SCRVDT
     C*** END   DEL *** 08/21/98 **************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C**
     C*  VALIDATE ADMISSION STATUS
      *
     C     SCADST        IFNE      *BLANK
     C                   MOVEL(P)  'ADST'        XXTBID
     C                   MOVEL(P)  SCADST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9072
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
     C**
     C*  VALIDATE TYPE OF ADMISSION
      *
     C     SCTYAD        IFNE      *BLANK
     C                   MOVEL(P)  'TYAD'        XXTBID
     C                   MOVEL(P)  SCTYAD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *ON
     C     GTSTAT        ORNE      'A'
     C                   SETON                                        9073
     C                   LEAVE
     C                   ENDIF
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C**
     C*  VALIDATE P/A REFERRAL
      *
     C     SCPARF        IFNE      *BLANK
     C     SCPARF        ANDNE     'Y'
     C                   SETON                                        9074
     C                   LEAVE
     C                   ENDIF
     C**
     C*  VALIDATE RE-INJURY
      *
     C     SCRINJ        IFNE      *BLANK
     C     SCRINJ        ANDNE     'Y'
     C                   SETON                                        9075
     C                   LEAVE
     C                   ENDIF
     C**
     C*  VALIDATE HOME HEALTH
      *
     C     SCHMHL        IFNE      *BLANK
     C     SCHMHL        ANDNE     'Y'
     C                   SETON                                        9076
     C                   LEAVE
     C                   ENDIF
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 2ND OPINION INFORMATION
     C**
     C*  VALIDATE REQUIRED FIELD
      *
     C     SC2OPN        IFNE      *BLANK
     C     SC2OPN        ANDNE     'Y'
     C                   SETON                                        9077
     C                   LEAVE
     C                   ENDIF
      *
      * VALIDATE EFFECTIVE DATE
      *
     C     SC2DT         IFNE      *ZEROS
     C                   Z-ADD     SC2DT         SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SC2DT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN78
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END SC2DT
      *
     C** VALIDATE PROVIDER/MOD
     C*
     C     SC2PHY        IFNE      *ZERO
     C     SC2PHM        ORNE      *ZERO
     C                   Z-ADD     SC2PHY        XXFEIN
     C                   Z-ADD     SC2PHM        XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     PDSTAT        OREQ      'I'
     C                   MOVE      *ON           *IN79
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END                                                    END *IN60
     C                   END                                                    SC2PHY
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IME INFORMATION
     C**
     C*  VALIDATE REQUIRED FIELD
      *
     C     SCIME         IFNE      *BLANK
     C     SCIME         ANDNE     'Y'
     C                   SETON                                        9080
     C                   LEAVE
     C                   ENDIF
      *
      * VALIDATE EFFECTIVE DATE
      *
     C     SCIMDT        IFNE      *ZEROS
     C                   Z-ADD     SCIMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C                   MOVEL     SRDTO         SCIMDT
     C*
     C     SRERR         IFEQ      '1'
     C                   MOVE      *ON           *IN81
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C*
     C                   END                                                    END SCIMDT
     C*
     C** IF PROV/MOD ENTERED MUST BE VALID
     C*
     C     SCIPHY        IFNE      *ZERO
     C     SCIPHM        ORNE      *ZERO
     C                   MOVEL     SCIPHY        XXFEIN
     C                   Z-ADD     SCIPHM        XXMOD
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '1'
     C     PDSTAT        OREQ      'I'
     C                   MOVE      '1'           *IN82
     C                   MOVE      *ON           *IN90
     C                   LEAVE
     C                   END
     C                   END                                                    SCIPHY <> 0
      *
     C                   ENDDO
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C* ===============================================================
     C* INIT SUBROUTINE - INITALIZE ALL FIELDS ON ENTRY                 *
     C* ===============================================================
     C     INIT          BEGSR
     C*
     C                   Z-ADD     *ZERO         SCSUB#            5 0
     C                   Z-ADD     *ZERO         SCFND             3 0
     C                   Z-ADD     *ZERO         SCFYR             3 0
     C                   Z-ADD     *ZERO         SCYR              3 0
     C                   Z-ADD     *ZERO         SCYR2             2 0
     C                   Z-ADD     *ZERO         SCCASE            7 0
     C                   MOVE      *ZERO         SCSS#             9
     C                   Z-ADD     *ZERO         SCACDT            8 0
     C                   MOVE      *BLANK        SCCERT
     C                   Z-ADD     PCEXT         SCEXT
     C*** BEGIN DEL *** 08/21/98 **************************************
     C**********           MOVE *BLANKS   SCRVDT
     C*** END   DEL *** 08/21/98 **************************************
     C                   MOVE      *BLANKS       SCADST
     C                   MOVE      *BLANKS       SCASDS
     C                   MOVE      *BLANKS       SCTYAD
     C                   MOVE      *BLANKS       SCTADS
     C                   MOVE      *BLANKS       SCPARF
     C                   MOVE      *BLANKS       SCRINJ
     C                   MOVE      *BLANKS       SCHMHL
     C                   MOVE      *BLANKS       SC2OPN
     C                   Z-ADD     *ZEROS        SC2DT
     C                   MOVE      *BLANKS       SC2PHY
     C                   Z-ADD     *ZEROS        SC2PHM
     C                   MOVE      *BLANKS       SC2RNM
     C                   MOVE      *BLANKS       SC2RAC
     C                   MOVE      *BLANKS       SC2PH1
     C                   MOVE      *BLANKS       SC2PH2
     C                   MOVE      *BLANKS       SCIPHY
     C                   Z-ADD     *ZEROS        SCIPHM
     C                   MOVE      *BLANKS       SCIPNM
     C                   MOVE      *BLANKS       SCIPAC
     C                   MOVE      *BLANKS       SCIPH1
     C                   MOVE      *BLANKS       SCIPH2
     C                   ENDSR                                                  INIT
     C*** BEGIN ADD *** 05/26/99 **************************************
     C*===============================================================
     C* KGDO - CASE PROVIDER
     C*===============================================================
     C     KGDO          BEGSR
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C*
     C                   CALL      'F0277R'
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM      '1'           ##CALL            1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      DBRRN2        ##RRN2            9 0
     C*
     C                   ENDSR
     C*===============================================================
     C* KHDO - BILL HISTORY
     C*===============================================================
     C     KHDO          BEGSR
     C*
     C                   CALL      'F0350R'
     C                   PARM      '1'           ##CALL            1
     C                   PARM      PCSUB#        ##SUB#            5 0
     C                   PARM      PCFND         ##FND             3 0
     C                   PARM      PCFYR         ##FYR             3 0
     C                   PARM      PCCASE        ##CASE            7 0
     C                   PARM      *ZEROS        ##EOB#            9 0
     C                   PARM      PCCERT        ##CERT           15
     C*
     C                   ENDSR
     C*===============================================================
     C* KIDO - CLAIM INFORMATION
     C*===============================================================
     C     KIDO          BEGSR
     C*
     C                   CALL      'W0070R'                             99      CHG 05/10/99
     C                   PARM      '1'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'M'           ##MODE            1
     C                   PARM                    ##RRN3            9 0
     C                   PARM      *BLANKS       ##SORT            2
     C*
     C                   ENDSR
     C*===============================================================
     C* KKDO - CASE INFORMATION
     C*===============================================================
     C     KKDO          BEGSR
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C*
     C                   CALL      'F0260R'
     C                   PARM      '0'           ##KC
     C                   PARM      '0'           ##KL
     C                   PARM      'L'           ##MODE
     C                   PARM      DBRRN2        ##RRN
     C                   PARM      *BLANKS       ##SS#             9
     C                   PARM      *ZEROS        ##ACDT            8 0
     C*
     C                   ENDSR
     C*** END   ADD *** 05/26/99 **************************************
     C*===============================================================
     C* KJDO - SUBROUTINE FOR F10
     C*===============================================================
     C*
     C     KJDO          BEGSR
      *
     C                   SELECT
      *
      *  ADMISSION STATUS
      *
     C     HFLD          WHENEQ    'SCADST'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZERO         @@RRN3            9 0
     C                   PARM      'ADST'        @@TBID
      *
     C     @@RRN3        IFEQ      *ZERO
     C                   MOVE      *ON           *IN31                          POS CUSR
     C                   END
      *
     C     @@RRN3        IFNE      *ZERO
     C                   Z-ADD     @@RRN3        DBRRN3
     C     DBRRN3        CHAIN     PGT0P                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTKEY         SCADST
     C                   MOVEL(P)  GTDESC        SCASDS
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   END
     C                   END                                                    SCADST
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *
      *  TYPE OF ADMISSION
      *
     C     HFLD          WHENEQ    'SCTYAD'
     C                   CALL      'F2380R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM      '0'           @@KC              1
     C     *INKL         PARM      '0'           @@KL              1
     C                   PARM      *ZERO         @@RRN3            9 0
     C                   PARM      'TYAD'        @@TBID
      *
     C     @@RRN3        IFEQ      *ZERO
     C                   MOVE      *ON           *IN32                          POS CUSR
     C                   END
      *
     C     @@RRN3        IFNE      *ZERO
     C                   Z-ADD     @@RRN3        DBRRN3
     C     DBRRN3        CHAIN     PGT0P                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTKEY         SCTYAD
     C                   MOVEL(P)  GTDESC        SCTADS
     C                   MOVE      *ON           *IN33                          POS CUSR
     C                   END
     C                   END                                                    SCTYAD
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*** BEG ADD *** 09/26/98 **********************************
     C* 2ND OPINION PHYSICIAN
     C*
     C     HFLD          WHENEQ    'SC2PHY'
     C     HFLD          OREQ      'SC2PHM'
     C*
     C     CMKY1         CHAIN     PCM01                              99
     C                   Z-ADD     DBRRN2        ##RRN2
     C                   CALL      'F2014R'      PROVSL                 99
      *
     C     ##RRN         IFEQ      *ZERO
     C     ##FEIN        ANDEQ     *BLANKS
      *
     C                   MOVE      *BLANKS       SC2RNM
     C                   MOVE      '1'           *IN38                          PC
     C                   MOVE      *OFF          *IN52
     C                   ELSE
      *
     C                   MOVE      '1'           *IN39                          PC
      *
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PPP01                              60
      *
     C     PPFEIN        IFEQ      *ZERO
     C                   MOVEL(P)  *BLANKS       SC2PHY
     C                   MOVEL(P)  *BLANKS       SC2PHM
     C                   MOVEL(P)  PP40A1        SC2RNM
     C                   ELSE
     C                   MOVE      PPFEIN        SC2PHY
     C                   MOVE      PPMOD         SC2PHM
     C                   ENDIF                                                  PPFEIN
      *
     C                   ELSE
     C                   MOVE      ##FEIN        SC2PHY
     C                   MOVE      ##MOD         SC2PHM
     C                   ENDIF
      *
     C                   MOVE      SC2PHY        XXFEIN
     C                   MOVE      SC2PHM        XXMOD
      *
     C     XXFEIN        IFNE      *ZERO
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDNE     'I'
      *
     C                   Z-ADD     PDAREA        SC2RAC
     C                   MOVEL     PDPHN#        SC2PH1
     C                   MOVE      PDPHN#        SC2PH2
      *
     C                   MOVEL(P)  PDNAME        XXLAST
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SC2RNM
     C                   ELSE
     C                   MOVEL     PDNAME        SC2RNM
     C                   ENDIF                                                  END PDFSN
      *
     C                   ENDIF                                                  END *IN60
      *
     C                   ENDIF                                                  XXFEIN NE 0
      *
     C                   ENDIF                                                  ##RRN
      *
     C*
     C     HFLD          WHENEQ    'SCIPHY'
     C     HFLD          OREQ      'SCIPHM'
     C     CMKY1         CHAIN     PCM01                              99
     C                   Z-ADD     DBRRN2        ##RRN2
     C                   CALL      'F2014R'      PROVSL                 99
      *
     C     ##RRN         IFEQ      *ZERO
     C     ##FEIN        ANDEQ     *BLANKS
      *
     C                   MOVE      *BLANKS       SCIPNM
     C                   MOVE      '1'           *IN41                          PC
     C                   MOVE      *OFF          *IN53
     C                   ELSE
      *
     C                   MOVE      '1'           *IN41                          PC
      *
     C     ##RRN         IFNE      *ZERO
     C     ##RRN         CHAIN     PPP01                              60
      *
     C     PPFEIN        IFEQ      *ZERO
     C                   MOVEL(P)  *BLANKS       SCIPHY
     C                   MOVEL(P)  *BLANKS       SCIPHM
     C                   MOVEL(P)  PP40A1        SCIPNM
     C                   ELSE
     C                   MOVE      PPFEIN        SCIPHY
     C                   MOVE      PPMOD         SCIPHM
     C                   ENDIF                                                  PPFEIN
      *
     C                   ELSE
     C                   MOVE      ##FEIN        SCIPHY
     C                   MOVE      ##MOD         SCIPHM
     C                   ENDIF
      *
     C                   MOVE      SCIPHY        XXFEIN
     C                   MOVE      SCIPHM        XXMOD
      *
     C     XXFEIN        IFNE      *ZERO
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDNE     'I'
      *
     C                   Z-ADD     PDAREA        SCIPAC
     C                   MOVEL     PDPHN#        SCIPH1
     C                   MOVE      PDPHN#        SCIPH2
      *
     C                   MOVEL(P)  PDNAME        XXLAST
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCIPNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCIPNM
     C                   ENDIF                                                  END PDFSN
      *
     C                   ENDIF                                                  END *IN60
      *
     C                   ENDIF                                                  XXFEIN NE 0
      *
     C                   ENDIF                                                  ##RRN
      *
     C*** END ADD *** 09/26/98 **********************************
     C*** BEG DEL *** 09/26/98 **********************************
      **2ND*OPINION PHYSICIAN
      *********
     C*********  HFLD      WHEQ 'SC2PHY'
     C*********  HFLD      OREQ 'SC2PHM'
     C*********            MOVE SC2PHY    ZZFEIN  9
     C*********            MOVE SC2PHM    ZZMOD   3
     C*********            CALL 'F0031R'
     C*********            PARM '2'       Q3CALL  1
     C*********            PARM           ZZFEIN
     C*********            PARM           ZZMOD
     C*********            PARM *BLANK    QQRTC   1
     C*********            PARM *BLANK    QQTYPE  1
     C*********
     C*********  QQRTC     IFEQ 'A'
     C*********            MOVE *ON       *INKC
     C*********            END
     C*********
     C*********  QQRTC     IFEQ 'B'
     C*********            MOVE '1'       *IN38
     C*********            END                             ENDEQ 'B'
      *********
     C*********  QQRTC     IFEQ '*'
     C*********            MOVE '1'       *IN39
     C*********            MOVE ZZFEIN    SC2PHY
     C*********            MOVE ZZMOD     SC2PHM
     C*********            MOVE ZZFEIN    XXFEIN
     C*********            MOVE ZZMOD     XXMOD
      *********
     C*********  PDKEY     CHAINPPD01                60
     C*********  *IN60     IFEQ '0'
     C*********  PDSTAT    ANDNE'I'
     C*********            Z-ADDPDAREA    SC2RAC
     C*********            MOVELPDPHN#    SC2PH1
     C*********            MOVE PDPHN#    SC2PH2
      *********
     C*********  SC2RAC    IFEQ *ZEROS
     C*********  SC2PH1    ANDEQ*ZEROS
     C*********  SC2PH1    ANDEQ*ZEROS
     C*********            MOVE '0'       *IN52
     C*********            ELSE
     C*********            MOVE '1'       *IN52
     C*********            END
      *********
     C*********            MOVELPDNAME    XXLAST    P
     C*********  PDFSN     IFNE *BLANK
     C*********            MOVELPDFSN     XXFRST    P
     C*********            MOVE '1'       XXTERM  1
     C*********            CALL 'SRNAME'
     C*********            PARM           XXLAST
     C*********            PARM           XXFRST
     C*********            PARM           XXTERM
     C*********            MOVELXXLAST    SC2RNM
     C*********            ELSE
     C*********            MOVELPDNAME    SC2RNM
     C*********            END                             END CDFSN
     C*********            END                             END *IN60
     C*********            END                             ENDEQ '*'
     C*********
     C**-*-*-*- - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C***IME*PHYSICIAN
     C*********
     C*********  HFLD      WHEQ 'SCIPHY'
     C*********  HFLD      OREQ 'SCIPHM'
     C*********            MOVE SCIPHY    ZZFEIN  9
     C*********            MOVE SCIPHM    ZZMOD   3
     C*********            CALL 'F0031R'
     C*********            PARM '2'       Q3CALL
     C*********            PARM           ZZFEIN
     C*********            PARM           ZZMOD
     C*********            PARM *BLANK    QQRTC
     C*********            PARM *BLANK    QQTYPE
     C*********
     C*********  QQRTC     IFEQ 'A'
     C*********            MOVE *ON       *INKC
     C*********            END
     C*********
     C*********  QQRTC     IFEQ 'B'
     C*********            MOVE '1'       *IN41
     C*********            END                             ENDEQ 'B'
     C*********
     C*********  QQRTC     IFEQ '*'
     C*********            MOVE '1'       *IN30
     C*********            MOVE ZZFEIN    SCIPHY
     C*********            MOVE ZZMOD     SCIPHM
     C*********            MOVE ZZFEIN    XXFEIN
     C*********            MOVE ZZMOD     XXMOD
     C*********
     C*********  PDKEY     CHAINPPD01                60
     C*********  *IN60     IFEQ '0'
     C*********  PDSTAT    ANDNE'I'
     C*********            Z-ADDPDAREA    SCIPAC
     C*********            MOVELPDPHN#    SCIPH1
     C*********            MOVE PDPHN#    SCIPH2
     C*********
     C*********  SCIPAC    IFEQ *ZEROS
     C*********  SCIPH1    ANDEQ*ZEROS
     C*********  SCIPH1    ANDEQ*ZEROS
     C*********            MOVE '0'       *IN53
     C*********            ELSE
     C*********            MOVE '1'       *IN53
     C*********            END
     C*********
     C*********            MOVELPDNAME    XXLAST    P
     C*********  PDFSN     IFNE *BLANK
     C*********            MOVELPDFSN     XXFRST    P
     C*********            MOVE '1'       XXTERM
     C*********            CALL 'SRNAME'
     C*********            PARM           XXLAST
     C*********            PARM           XXFRST
     C*********            PARM           XXTERM
     C*********            MOVELXXLAST    SCIPNM
     C*********            ELSE
     C*********            MOVELPDNAME    SCIPNM
     C*********            END                             END PDFSN
     C*********
     C*********            END                             END *IN60
     C*********            END                             ENDEQ '*'
     C*** END DEL *** 09/26/98 **********************************
     C*
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CURSOR IS NOT ON A SEARCHABLE FEILD
      *
     C                   OTHER
     C                   MOVE      *ON           *IN20
     C                   ENDSL
     C*
     C                   ENDSR                                                  KJDO
     C*===============================================================
     C* KNDO - SUBROUTINE FOR F14 = DARTS
     C*===============================================================
     C*
     C     KNDO          BEGSR
      *
      * CALL API TO ACCESS DARTS
      *
     C                   MOVE      XXCO#         XACO#
     C                   MOVE      XXSUB#        XASUB#
     C                   MOVE      XXFND         XAFND
     C                   MOVE      XXFYR         XAFYR
     C                   MOVE      XXCASE        XACASE
      *
     C                   CALL      'FA703R'
     C                   PARM      *BLANK        @@RTCD            1
     C                   PARM                    @@WCIN
     C                   PARM                    @@WCDK
     C                   PARM                    XACO#             3
     C                   PARM                    XASUB#            5
     C                   PARM                    XAFND             3
     C                   PARM                    XAFYR             3
     C                   PARM                    XACASE            7
     C                   PARM      *BLANK        ##KEY            40
      *
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
      *
     C     *IN40         IFEQ      *ON
     C                   MOVEL     MODEA         SCMODE                         ADD MODE
     C                   END
      *
     C     PCEXT         IFGT      *ZERO
     C                   MOVEL(P)  OPTNMR        ##DATA                         RE-CERT
     C                   ELSE
     C                   MOVEL(P)  OPTNM         ##DATA                         PRE-CERT
     C                   END
      *
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA
     C                   PARM      40            ##LENG
     C                   MOVEL(P)  ##DATA        SCPRDS
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      *  REVIEW DATE
     C*** BEGIN DEL *** 08/21/98 **************************************
     C**********           Z-ADDPCRVDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTMDYY
     C**********           MOVELSRDTO     SCRVDT
     C*** END   DEL *** 08/21/98 **************************************
      *  ADMIT STATUS
     C                   MOVEL(P)  PCADST        SCADST
     C                   MOVEL(P)  'ADST'        XXTBID
     C                   MOVEL(P)  PCADST        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCASDS
     C                   ENDIF
      *
      *  TYPE OF ADMISSION
     C                   MOVEL(P)  PCTYAD        SCTYAD
     C                   MOVEL(P)  'TYAD'        XXTBID
     C                   MOVEL(P)  PCTYAD        XXKEY
     C     GTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C     GTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  GTDESC        SCTADS
     C                   ENDIF
      *
     C                   MOVEL(P)  PCPARF        SCPARF
     C                   MOVEL(P)  PCRINJ        SCRINJ
     C                   MOVEL(P)  PCHMHL        SCHMHL
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * 2ND OPINION INFORMATION
      *
     C                   MOVEL(P)  PC2OPN        SC2OPN
      *
     C                   Z-ADD     PC2DT         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SC2DT
      *
      *  PHYSICIAN
     C                   Z-ADD     PC2PHY        SC2PHY
     C                   Z-ADD     PC2PHY        XXFEIN
     C                   Z-ADD     PC2PHM        SC2PHM
     C                   Z-ADD     PC2PHM        XXMOD
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDNE     'I'
     C                   Z-ADD     PDAREA        SC2RAC
     C                   MOVEL     PDPHN#        SC2PH1
     C                   MOVE      PDPHN#        SC2PH2
      *
     C     SC2RAC        IFEQ      *ZEROS
     C     SC2PH1        ANDEQ     *ZEROS
     C     SC2PH2        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN52
     C                   ELSE
     C                   MOVE      '1'           *IN52
     C                   END
      *
     C                   MOVEL(P)  PDNAME        XXLAST           56
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST           15
     C                   MOVE      '1'           XXTERM            1
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SC2RNM
     C                   ELSE
     C                   MOVEL     PDNAME        SC2RNM
     C                   END                                                    END CDFSN
      *
     C                   END                                                    END *IN60
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * IME INFORMATION
      *
     C                   MOVEL(P)  PCIME         SCIME
      *
     C                   Z-ADD     PCIMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCIMDT
      *
      *  TREATING PHYSICIAN
     C                   Z-ADD     PCIPHY        SCIPHY
     C                   Z-ADD     PCIPHM        SCIPHM
     C                   Z-ADD     PCIPHY        XXFEIN
     C                   Z-ADD     PCIPHM        XXMOD
      *
     C     PDKEY         CHAIN     PPD01                              60
     C     *IN60         IFEQ      '0'
     C     PDSTAT        ANDNE     'I'
     C                   Z-ADD     PDAREA        SCIPAC
     C                   MOVEL     PDPHN#        SCIPH1
     C                   MOVE      PDPHN#        SCIPH2
      *
     C     SCIPAC        IFEQ      *ZEROS
     C     SCIPH1        ANDEQ     *ZEROS
     C     SCIPH2        ANDEQ     *ZEROS
     C                   MOVE      '0'           *IN53
     C                   ELSE
     C                   MOVE      '1'           *IN53
     C                   END
      *
     C                   MOVEL(P)  PDNAME        XXLAST
      *
     C     PDFSN         IFNE      *BLANK
     C                   MOVEL(P)  PDFSN         XXFRST
     C                   MOVE      '1'           XXTERM
     C                   CALL      'SRNAME'
     C                   PARM                    XXLAST
     C                   PARM                    XXFRST
     C                   PARM                    XXTERM
     C                   MOVEL     XXLAST        SCIPNM
     C                   ELSE
     C                   MOVEL     PDNAME        SCIPNM
     C                   END                                                    END CDFSN
      *
     C                   END                                                    END *IN60
      *
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*  SCREEN 1
     C*
     C*** BEGIN DEL *** 08/21/98 **************************************
     C**********           Z-ADDSCRVDT    SRDTI
     C**********           CALL 'SRCVTDAT'DTCYMD
     C**********           MOVELSRDTO     PCRVDT
     C*** END   DEL *** 08/21/98 **************************************
     C*
     C                   MOVEL(P)  SCADST        PCADST
     C                   MOVEL     SCTYAD        PCTYAD
     C                   MOVEL     SCPARF        PCPARF
     C                   MOVEL     SCRINJ        PCRINJ
     C                   MOVEL     SCHMHL        PCHMHL
     C*
     C                   MOVEL     SC2OPN        PC2OPN
     C*
     C                   Z-ADD     SC2DT         SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PC2DT
     C*
     C                   Z-ADD     SC2PHY        PC2PHY
     C                   Z-ADD     SC2PHM        PC2PHM
     C*
     C                   MOVEL     SCIME         PCIME
     C*
     C                   Z-ADD     SCIMDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         PCIMDT
     C*
     C                   Z-ADD     SCIPHY        PCIPHY
     C                   Z-ADD     SCIPHM        PCIPHM
     C*
     C                   MOVEL     Q1USER        PCUSER
     C                   MOVEL     Q1PGM         PCPGM
     C                   Z-ADD     XXDT          PCDT
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        PCTIME
     C*
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other function keys
     C*================================================================
     C     OTHFKS        BEGSR
      *
      * RESET ERROR INDICATORS
     C                   MOVEA     '00000000'    *IN(30)                        *IN30 - 37
     C                   MOVEA     '00'          *IN(38)                        *IN38 - 39
     C                   MOVEA     '00'          *IN(41)                        *IN41 - 42
      *
     C*** BEGIN ADD *** 05/26/99 **************************************
     C     *INKG         CASEQ     *ON           KGDO                     90     F7 = CASE PROV
     C     *INKH         CASEQ     *ON           KHDO                     90     F8 = BILL HIST
     C     *INKI         CASEQ     *ON           KIDO                     90     F9 = CLAIM INFO
     C     *INKK         CASEQ     *ON           KKDO                     90     F6 = CASE INFO
     C*** END   ADD *** 05/26/99 **************************************
     C     *INKJ         CASEQ     *ON           KJDO                     90     F10 = SEARCH
     C     *INKN         CASEQ     *ON           KNDO                     90     F14 = DARTS
     C                   ENDCS
      *
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
      *
      *  CLEAR THE ENTIRE IBM ARRAY (*IN)
      *
     C                   CLEAR                   *IN
     C*
     C                   RETURN
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     1             #OFSCR            1 0          TWO SCREENS
     C                   MOVE      *ON           DSPTYP
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    @@WCIF            1            WC INST
     C                   PARM                    @@MCIF            1            MC INST
     C                   PARM                    @@WCIN            1            WC INTERFACE
     C                   PARM                    @@AR              1            MC AR INST
     C                   PARM                    @@NTWK            1            NETWORK INST
     C                   PARM                    @@UR              1            PRECERT INST
     C                   PARM                    @@MED             1            MEDMGMT INST
     C                   PARM                    @@EDI             1            EDI INST
     C                   PARM                    @@WCDK            1            WC DARTS KEY
     C*** BEGIN ADD *** 05/26/99 **************************************
     C                   PARM                    @@RHAB            1            REHAB INST
     C                   PARM                    @@MSS             1            MS SUPPORT
     C                   PARM                    @@FSCH            1            FEE SCHEDULE
     C                   PARM                    @@PRVB            1            PROVIDER BILLS
     C*** END   ADD *** 05/26/99 **************************************
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI                          CURRENT DATE
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDT
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'A'
     C                   MOVE      *ON           *IN40
     C     @@MODE        WHENEQ    'D'
     C                   MOVE      *ON           *IN09
     C     @@MODE        WHENEQ    '2'
     C                   MOVEL(P)  MODED         SCMODE                         SELECT
     C                   MOVE      *ON           *IN05
     C                   ENDSL
     C*
     C* INITIALIZE ALL SCREEN FIELDS
     C*
     C                   EXSR      INIT
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * GET INFO FOR HEADING
      *   SUB,GROUP,YEAR,CASE
      *
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   MOVE      PCCO#         XXCO#
     C                   MOVE      PCSUB#        XXSUB#
     C                   MOVE      PCFND         XXFND
     C                   MOVE      PCFYR         XXFYR
     C                   MOVE      PCCASE        XXCASE
     C                   MOVEL     PCCERT        XXCERT
     C                   MOVE      PCEXT         XXEXT
     C                   MOVEL     PCCERT        SCCERT
     C                   MOVE      PCEXT         SCEXT
     C                   END                                                    END *IN99
      *
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CALL API TO CREATE A TWO LINE HEADING
      *
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      XXSUB#        QQSUB#
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      XXCASE        QQCASE
      *
     C                   CALL      'FA702R'
     C                   PARM      *BLANK        QQRTCD            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQSUB#            5
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C     SCL1HD        PARM      *BLANK        QQL1             78
     C     SCL2HD        PARM      *BLANK        QQL2             78
     C*** BEGIN ADD *** 05/26/99 **************************************
     C*
     C* IS PROVIDER BILLS INSTALLED
     C*
     C     @@PRVB        IFEQ      'Y'
     C*
     C                   SETON                                        16
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       16
     C*
     C                   ENDIF
     C*
     C*** CALL WCMS CLIAMS MASTER MAINTENANCE
     C*
B01  C     @@WCIN        IFEQ      'Y'
     C     @@WCIF        ANDEQ     'Y'                                          WC INTERFACE
     C*
     C* IF GROUP IS LINKED WITH WCMS THEN OBTAIN WCMS GROUP NUMBER.
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      QQCO#         #1CO#
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM      QQSUB#        #1SUB#            5
     C                   PARM      QQFND         #1FND
     C*
B02  C     ##RTCD        IFEQ      *BLANKS
     C*
     C                   CALL      'WA004R'                             99      CHG 05/10/99
     C                   PARM                    #1CO#             3
     C                   PARM      #1WCF#        #1FND             3
     C                   PARM      QQFYR         #1FYR             3
     C                   PARM      QQCASE        #1CASE            7
     C                   PARM      *ZEROS        ##RRN             9 0
     C*
B03  C     ##RRN         IFNE      *ZEROS
     C*
     C                   SETON                                        18
     C                   Z-ADD     ##RRN         ##RRN3
     C*
S03  C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
E03  C                   ENDIF                                                  ##RRN NE 0
     C*
S02  C                   ELSE                                                   ##RTCD EQ ' '
     C*
     C                   SETOFF                                       18
     C*
E02  C                   ENDIF                                                  ##RTCD EQ ' '
     C*
S01  C                   ELSE
     C*
     C                   SETOFF                                       18
     C*
E01  C                   ENDIF                                                  ##WCIN EQ 'Y'
     C*** END   ADD *** 05/26/99 **************************************
      *
      *  TOTAL REC DAYS
     C                   Z-ADD     *ZEROS        SCTREC
     C     PCKY1         SETGT     PPC01                                        W/ EXT
     C     *IN60         DOUEQ     *ON
     C     PCKY          READPE    PPC01                                  60
     C     *IN60         IFEQ      *OFF
     C                   ADD       PCRECD        SCTREC
     C                   END
     C                   ENDDO
      *
     C                   ENDSR                                                  PGMSTR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             60
     C*
     C     *IN60         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   UPDATE    DBRCDF
     C                   END                                                    END 0 *IN60
     C*
     C                   MOVE      *ON           OUTFLG
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   EXSR      UPDATR
     C*
     C*** BEGIN DEL *** 01/12/99 **************************************
     C*
     C* GET THE NEXT AVAILABLE EOB# FROM CO AUXILLARY FILE
     C*
     C********** C1KY      CHAINPC101                60
     C********** *IN60     IFEQ '0'
     C********** C1EOB#    IFEQ *HIVAL
     C**********           Z-ADD1         C1EOB#
     C**********           ELSE
     C**********           ADD  1         C1EOB#
     C**********           END
     C**********           UPDATPC101
     C**********           ELSE
     C**********           Z-ADD*HIVAL    C1EOB#
     C**********           END
     C*
     C**********           Z-ADDC1EOB#    EOCHK#
     C**********           Z-ADDC1EOB#    EOEOB#
     C*
     C* MOVE FIELDS INTO PARM FIELDS FOR F15 TIME/MATERIALS CALL
     C*
     C**********           Z-ADDC1EOB#    XXEOB#
     C**                   MOVE C1EOB#    Q2EOB#
     C*
     C**********           Z-ADDXXCO#     EOCO#
     C**********           Z-ADDXXSUB#    EOSUB#
     C**********           Z-ADDXXFND     EOFND
     C**********           Z-ADDXXFYR     EOFYR
     C**********           Z-ADDXXCASE    EOCASE
     C*
     C**MOVE FIELDS INTO PARM FIELDS FOR F15 TIME/MATERIALS CALL
     C**
     C**                   MOVE XXCO#     Q2CO#
     C**                   MOVE XXSUB#    Q2SUB#
     C**                   MOVE XXFND     Q2FND
     C**                   MOVE XXYR      Q2FYR
     C**                   MOVE XXCASE    Q2CASE
     C**********           Z-ADDC1EOB#    XXEOB#
     C**                   MOVE C1EOB#    Q2EOB#
     C*
     C**********           Z-ADDXXDT      EOTRDT
     C**********           MOVE PCFEIN    EOFEIN
     C**********           Z-ADDPCMOD     EOMOD
     C**********           Z-ADD*ZEROS    EOAMT
     C**********           Z-ADD*ZEROS    EOFRDT
     C**********           Z-ADD*ZEROS    EOTODT
     C**********           MOVE *BLANKS   EOTRCD
     C**********           MOVE *BLANKS   EOPNAM
     C**********           MOVE *BLANKS   EOADR1
     C**********           MOVE *BLANKS   EOADR2
     C**********           MOVE *BLANKS   EOCTY
     C**********           MOVE *BLANKS   EOST
     C**********           Z-ADD*ZEROS    EOZIP
     C**********           MOVE *BLANKS   EOEOB1
     C**********           MOVE *BLANKS   EOEOB2
     C**********           MOVE *BLANKS   EOEOB3
     C**********           MOVE 'U'       EOSYS
     C**********           MOVE 'X'       EOFLG1
     C**********           MOVE 'R'       EOFLG2
     C**********           MOVE *BLANKS   EOFLG3
     C**********           MOVE 'Y'       EOFLG4
     C**********           MOVE *BLANKS   EOFLG5
     C**********           MOVE *BLANKS   EOFLG6
     C**********           MOVE *BLANKS   EOFLG7
     C**********           MOVE *BLANKS   EOFLG8
     C**********           MOVE *BLANKS   EOFLG9
     C**********           Z-ADD*ZEROS    EOAMTC
     C********** @@NTWK    IFEQ 'Y'
     C**********           MOVE *BLANKS   EOSTAT
     C**********           ELSE
     C**********           MOVE 'P'       EOSTAT
     C**********           END
     C**********           MOVELQ1USER    EOUSER
     C**********           MOVELQ1PGM     EOPGM
     C**********           Z-ADDXXDT      EODT
     C**********           Z-ADDDSTIME    EOTIME
     C**********           MOVELQ1USER    EOIUSR
     C**********           Z-ADDXXDT      EOIDT
     C**********           Z-ADD*ZEROS    EOWCCK
     C**********           MOVE *BLANKS   EODIA1
     C**********           MOVE *BLANKS   EODIA2
     C**********           MOVE *BLANKS   EODIA3
     C**********           MOVE *BLANKS   EODIA4
     C**********           MOVE *BLANKS   EOEMPL
     C**********           Z-ADD*ZEROS    EODFC
     C**********           MOVE *BLANKS   EOFLIC
     C**********           MOVE 'N'       EOTAPE
     C**********           Z-ADD1         EO#BLL
     C**********           MOVE *BLANKS   EOCLSF
     C**********           MOVE *BLANKS   EOAST
     C**********           MOVE *BLANKS   EOAUDT
     C**********           MOVE *BLANKS   EOBTFL
     C**********           MOVE 'H'       EOHRFL
     C**********           MOVE *BLANKS   EOBLFL
     C**********           MOVE *BLANKS   EONLFL
     C**********           MOVE *BLANKS   EODIFL
     C**********           Z-ADD*ZEROS    EOINV#
     C**********           MOVE *BLANKS   EORMDS
     C**********           MOVE *BLANKS   EORMK1
     C**********           MOVE *BLANKS   EORMK2
     C**********           MOVE *BLANKS   EORMK3
     C**********           MOVE *BLANKS   EOCKCN
     C**********           Z-ADD*ZEROS    EOOTHR
     C**********           Z-ADD*ZEROS    EONDIS
     C**********           MOVE *BLANKS   EOSB01
     C**********           MOVE *BLANKS   EOSB02
     C**********           MOVE *BLANKS   EOSB03
     C**********           MOVE *BLANKS   EOSB04
     C**********           MOVE *BLANKS   EOSB05
     C**********           MOVE *BLANKS   EOSB06
     C**********           MOVE *BLANKS   EOSB07
     C**********           MOVE *BLANKS   EOSB08
     C**********           MOVE *BLANKS   EOSB09
     C**********           MOVE *BLANKS   EOSB10
     C**********           Z-ADD*ZEROS    EOBAMT
     C**********           MOVE *BLANKS   EOZIPA
     C**********           MOVELXXCERT    EOCERT
     C**********           Z-ADDXXEXT     EOEXT
     C**********           WRITEPEO01
      *
     C*  WRITE THE EOMB AUXILARY FILE
     C*
     C**********           Z-ADDXXCO#     E1CO#
     C**********           Z-ADDXXSUB#    E1SUB#
     C**********           Z-ADDXXFND     E1FND
     C**********           Z-ADDXXFYR     E1FYR
     C**********           Z-ADDXXCASE    E1CASE
     C**********           Z-ADDC1EOB#    E1EOB#
     C**********           MOVE *BLANKS   E1ACCT
     C**********           Z-ADD*ZEROS    E1ENDT
     C**********           Z-ADD*ZEROS    E1BLDT
     C**********           Z-ADD*ZEROS    E1ORDT
     C**********           Z-ADDXXDT      E1CRDT
     C**********           Z-ADD*ZEROS    E1TRPH
     C**********           Z-ADD*ZEROS    E1TRPM
     C**********           MOVE *BLANKS   E1FLG1
     C**********           MOVE *BLANKS   E1FLG2
     C**********           MOVE *BLANKS   E1FLG3
     C**********           MOVE *BLANKS   E1FLG4
     C**********           MOVE *BLANKS   E1FLG5
     C**********           Z-ADD*ZEROS    E1DT1
     C**********           Z-ADD*ZEROS    E1DT2
     C**********           Z-ADD*ZEROS    E1DT3
     C**********           Z-ADD*ZEROS    E1DT4
     C**********           Z-ADD*ZEROS    E1DT5
     C**********           Z-ADD*ZEROS    E1AMT1
     C**********           Z-ADD*ZEROS    E1AMT2
     C**********           Z-ADD*ZEROS    E1AMT3
     C**********           Z-ADD*ZEROS    E1AMT4
     C**********           Z-ADD*ZEROS    E1AMT5
     C**********           MOVE *BLANKS   E110A1
     C**********           MOVE *BLANKS   E110A2
     C**********           MOVE *BLANKS   E110A3
     C**********           MOVE *BLANKS   E120A1
     C**********           MOVE *BLANKS   E120A2
     C**********           MOVE *BLANKS   E140A1
     C**********           MOVELQ1USER    E1USER
     C**********           MOVELQ1PGM     E1PGM
     C**********           Z-ADDXXDT      E1DT
     C**********           Z-ADDDSTIME    E1TIME
     C**********           MOVELQ1USER    E1IUSR
     C**********           Z-ADDXXDT      E1IDT
     C**********           WRITEPE101
     C*
     C* MOVE FIELDS FOR WRITE TO NAPBP FILE
     C*
     C**********           Z-ADDC1EOB#    PBEOB#
     C**********           Z-ADDXXCO#     PBCO#
     C**********           Z-ADDXXSUB#    PBSUB#
     C**********           Z-ADDXXFND     PBFND
     C**********           Z-ADDXXFYR     PBFYR
     C**********           Z-ADDXXCASE    PBCASE
     C**********           MOVELPCSS#     PBSS#
     C**********           Z-ADDPCACDT    PBACDT
     C**********           MOVELXXCERT    PBCERT
     C**********           Z-ADDXXEXT     PBEXT
     C**********           Z-ADDXXDT      PBTRDT
     C**********           MOVE PCFEIN    PBFEIN
     C**********           Z-ADDPCMOD     PBMOD
     C**********           Z-ADD*ZEROS    PBBAMT
     C**********           Z-ADD*ZEROS    PBCAMT
     C**********           Z-ADD*ZEROS    PBINV#
     C**********           MOVE 'H'       PBHRFL
     C**********           MOVE *BLANKS   PBBLFL
     C**********           MOVE *BLANKS   PBFLG1
     C**********           MOVE *BLANKS   PBFLG2
     C**********           MOVE *BLANKS   PBFLG3
     C**********           MOVE *BLANKS   PBFLG4
     C**********           MOVE *BLANKS   PBFLG5
     C**********           MOVE 'A'       PBSTAT
     C**********           MOVELQ1USER    PBUSER
     C**********           MOVELQ1PGM     PBPGM
     C**********           MOVELQ1USER    PBIUSR
     C**********           Z-ADDXXDT      PBDT
     C**********           Z-ADDDSTIME    PBTIME
     C**********           Z-ADDXXDT      PBIDT
     C**********           WRITEPPB01
     C*** END   DEL *** 01/12/99 **************************************
     C*
     C                   ENDSR
