     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F0362RNY - NY: HOSPITAL (UB) HEADER SUMMARY
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**                                                             **
     H**   PROGRAMMER: C:79                                          **
     H**   CREATED:   09/07/04 PROJECT: NY009 FROM F0352R            **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*
     H*  08/11/21 C:295 Project: NY056
     H*                 Add Hospital Name to screen
     H*
     H*  06/10/19 C:180 Support: 80081
     H*                 Remove Visibility property for all fields except
     H*                 buttons - dspf changes only.
     H*
     H*  05/20/19 C:180 Project: MC427
     H*                 Modernize header screen.
     H*                 Change source for loading type of service desc.
     H*
     H*  04/04/17 C:212 Project: MC409
     H*                 Add new field for Vendor Bill in DSPF and processing
     H*
     H*  07/13/01 C:79 PROJECT: 114MCG
     H*                1.ADD MEDICARE NUMBER TO THE SCREEN
     H*
     H*  06/08/01 C:62 PROJECT:FSMNT
     H*                COMPILE ONLY
     H*
     H*  11/08/00 C:62 PROJECT:FS146
     H*                CHANGE 'BLTY' TO 'ZZBT' AND 'FLBT'.
     H*
     H*****************************************************************
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      16      PreCerts installed                             **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FAEOP  (DBRCDF): EOMB Transaction File                 **
     H**      FMCML1 (PCM01) : Claim file (CO#/SUB#/FND/FYR/CASE)    **
     H**      FMPDL  (PPD01) : WCMC Provider Master (FEIN/MOD)       **
     H**      FMP1L  (PP101) : Provider Auxiliary File (FEIN/MOD)    **
     H**      FAEUL  (PEU01) : EOMB Hosp/UB Header File              **
     H**                       (CO#/SUB#/FND/FYR/CASE/EOB#)          **
     H**      FTABL          : WCMC tables (CO#/KEY)                 **
     H**             (PCC01) : County code table file                **
     H**             (PSP01) : Speciality code table file            **
     H**      STABL          : System tables (CO#/KEY)               **
     H**             (PST01) : System state table                    **
     H**      FTGTL  (PGT01) : WCMC Generic tables (CO#/TBID/TKEY)   **
     H**                     : BLTY - Type of bill                   **
     H**                     : SVTY - Type of service                **
     H**                     : SVTZ - Type of service - status       **
     H**      UMPCL6 (PPC01) : Pre-Certification master              **
     H**                       (CO/SUB/FND/FYR/CASE/CERT/EXT EXT=0)  **
     H*****************************************************************
     H*
     FF0362FNY  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     FFAEOP     UF   E             DISK
     F                                     RENAME(PEO01:DBRCDF)
     FFAEUL     IF   E           K DISK
     FFMCML1    IF   E           K DISK
     FFMP1L     IF   E           K DISK
     FFMPDL     IF   E           K DISK
     FFTABL     IF   E           K DISK
     FSTABL     IF   E           K DISK
     FFTGTL     IF   E           K DISK
     FUMPCL6    IF   E           K DISK
     F                                     INFDS(DBFDS)
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*
     D                 DS
     D  XXCNTY                 1      6
     D  P1ST                   1      2
     D  SCCNTY                 3      6
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Hospital (UB) Header-
     D                                      Summary')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEOP
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#            5 0
     C                   PARM                    ##FYR             3 0
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     CMKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C*
     C     EUKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKTKEY
     C*** BEGIN ADD *** 11/08/00 **************************************
     C*
     C     GTKY2         KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTBID
     C*** END   ADD *** 11/08/00 **************************************
     C*
     C     P1KY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     PCKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKCERT
     C                   KFLD                    KKEXT
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C     STKY          KLIST
     C                   KFLD                    KKKEY
     C*
     C     TBKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKKEY
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    SPKEY         KKKEY
     C     *LIKE         DEFINE    EOCO#         KKCO#
     C     *LIKE         DEFINE    EOSUB#        KKSUB#
     C     *LIKE         DEFINE    EOFND         KKFND
     C     *LIKE         DEFINE    EOFYR         KKFYR
     C     *LIKE         DEFINE    EOCASE        KKCASE
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    EOCERT        KKCERT
     C     *LIKE         DEFINE    EOEXT         KKEXT
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKTKEY
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DLTPRE - Pre-processing before delete occurs
     C*================================================================
     C     DLTPRE        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* DSPLYR - Processing after displaying information
     C*================================================================
     C     DSPLYR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* KIDO - F9=Pre-Cert Info.
     C*================================================================
     C     KIDO          BEGSR
     C*
     C                   CALL      'U0060R01'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      'D'           ##MODE            1
     C                   PARM      DBRRN         ##RRN             9 0
     C                   PARM      *ZEROS        #1CO#             3
     C                   PARM      *ZEROS        #1SUB#            5
     C                   PARM      *ZEROS        #1FND             3
     C                   PARM      *ZEROS        #1FYR             3
     C                   PARM      *ZEROS        #1CASE            7
     C                   PARM      *BLANKS       #1CERT           15
     C                   PARM      *BLANKS       #1EXT             3
     C                   PARM      *BLANKS       #1OCST            1
     C                   PARM      *BLANKS       #1CONT            1
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C*          Get FAEOP
     C*          ------------------------------------------------------
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   MOVEL     EOAST         SCAST
     C                   MOVEL(P)  EOAST         KKKEY
     C     STKY          CHAIN     PST01                              99
     C  N99              MOVEL(P)  STNAME        SCASNM
     C   99              MOVEL(P)  *BLANKS       SCASNM
     C                   MOVEL(P)  EOCERT        SCCERT
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C                   MOVE(P)   EOZIPA        SCZIPA
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C*
     C***** Beg Add *** 04/04/17 **************************************
     C                   EVAL      SCSINV = EOSINV
     C***** End Add *** 04/04/17 **************************************
     C*
     C*          Get FMPDP
     C*          ------------------------------------------------------
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  PDSPCD        SCSPCD
     C                   MOVEL(P)  PDZIP         SCZIP
     C                   Z-ADD     EOCO#         KKCO#
     C                   MOVEL(P)  SCSPCD        KKKEY
     C     TBKY          CHAIN     PSP01                              99
     C  N99              MOVEL(P)  SPDESC        SCSCDS
     C   99              MOVEL(P)  *BLANKS       SCSCDS
     C                   ELSE                                                   *IN99 = Off
     C                   MOVEL(P)  *BLANKS       SCSPCD
     C                   MOVE(P)   *ZEROS        SCZIP
     C                   MOVEL(P)  *BLANKS       SCSCDS
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*          Get FMP1P
     C*          ------------------------------------------------------
     C     P1KY          CHAIN     PP101                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  P1ZIP         SCZIP
     C     4             SUBST(P)  P1CNTY:6      SCCNTY
     C     5             SUBST(P)  P1CNTY:1      SCSMSA
     C                   MOVEL(P)  XXCNTY        KKKEY
     C     TBKY          CHAIN     PCC01                              99
     C  N99              MOVEL(P)  CCDESC        SCCDSC
     C   99              MOVEL(P)  *BLANKS       SCCDSC
     C     P1ST          CAT(P)    'SM':0        KKTBID
     C                   MOVEL(P)  SCSMSA        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCSDSC
     C   99              MOVEL(P)  *BLANKS       SCSDSC
     C                   ELSE                                                   *IN99 = Off
     C                   MOVEL(P)  *BLANKS       SCCNTY
     C                   MOVEL(P)  *BLANKS       SCSMSA
     C                   MOVEL(P)  *BLANKS       SCCDSC
     C                   MOVEL(P)  *BLANKS       SCSDSC
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*
     C*          Get FAEUP
     C*          ------------------------------------------------------
     C     EUKY          CHAIN     PEU01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  EUBLTY        SCBLTY
     C*** BEGIN CHG *** 11/08/00 **************************************
     C**********           MOVEL'BLTY'    KKTBID    P
     C                   MOVEL(P)  BTTBID        KKTBID
     C*** END   CHG *** 11/08/00 **************************************
     C                   MOVEL(P)  SCBLTY        KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCBLTD
     C   99              MOVEL(P)  *BLANKS       SCBLTD
     C*
     C                   Z-ADD     EUSCPF        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPF
     C                   Z-ADD     EUSCPT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCSCPT
     C*
     C                   MOVEL(P)  EUICN         SCICN
      *** Begin Change *** 05/20/19 ***********************************
     C***                MOVEL(P)  'SVTY'        KKTBID
     C                   MOVEL(P)  'NYT1'        KKTBID
      *** End   Change *** 05/20/19 ***********************************
     C                   MOVEL(P)  SCICN         KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCICND
     C   99              MOVEL(P)  *BLANKS       SCICND
     C                   MOVEL(P)  EUDCN         SCDCN
     C                   MOVEL(P)  'SVTZ'        KKTBID
     C                   MOVEL(P)  SCDCN         KKTKEY
     C     GTKY          CHAIN     PGT01                              99
     C  N99              MOVEL(P)  GTDESC        SCDCND
     C   99              MOVEL(P)  *BLANKS       SCDCND
     C                   ENDIF                                                  *IN99 = Off
     C*
     C*** BEG ADD *** 07/13/01 ****************************************
     C****** Beg Add ********** 08/11/21 **********************************
     C*****              MOVEL(P)  EUMCAR        SCMED#
      /Free
         scmed# = %Trim(EUmcar) + '-' + %Trim(EUhnam);
      /End-Free
     C****** End Add ********** 08/11/21 **********************************
     C*** END ADD *** 07/13/01 ****************************************
     C*
     C*          Get FMCMP
     C*          ------------------------------------------------------
     C     CMKY          CHAIN     PCM01                              99
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C*          Get UMPCP
     C*          ------------------------------------------------------
     C     *IN16         IFEQ      *ON
     C                   MOVEL(P)  EOCERT        KKCERT
     C                   MOVEL(P)  EOEXT         KKEXT
     C     PCKY          CHAIN     PPC01                              99
     C   99              SETOFF                                       16
     C                   ENDIF                                                  *IN16 = On
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKI         CASEQ     *ON           KIDO                           F10=search
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C**  GET SYSTEM INFO
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
     C*
     C     ##UR          IFEQ      'Y'
     C                   SETON                                        16
     C                   ELSE                                                   ##UR = 'Y'
     C                   SETOFF                                       16
     C                   ENDIF                                                  ##UR = 'Y'
     C*** BEGIN ADD *** 11/08/00 **************************************
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C     EOAST         CAT(P)    'BT':0        BTTBID            4
     C                   MOVEL(P)  BTTBID        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'ZZBT'        KKTBID
     C     GTKY2         CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   MOVEL(P)  'BLTY'        BTTBID
     C                   ELSE
     C                   MOVEL(P)  'ZZBT'        BTTBID
     C                   ENDIF
     C                   ENDIF
     C*** END   ADD *** 11/08/00 **************************************
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C*
     C                   ENDSR
