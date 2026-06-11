     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*
     H*****************************************************************
     H*  F8079R01 - Ancillary Bill Information                        *
     H*                                                               *
     H*  Called from: F8079R - Work with Ancillary Bills with Errors  *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/05/2011 by c:198 for Project 45MHZ             *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8079F01  CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F*
     F*  Physical file definition (for maintaining record)
     FFAC1P     UF A E             DISK
     F                                     RENAME(PC101:DBRCDF)
     FFMC1L     UF   E           K DISK
     F                                     RENAME(PC101:PM101)
     FFMCML1    IF   E           K DISK
     FFMPDL     IF   E           K DISK

      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      ***** End Add *** 06/09/20 *******************************************

      /COPY SCOPYMT01D
     D*
     D CLM             S              1    DIM(20)
     D DUE             S              1    DIM(11)
     D*
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  CLAIM#                 1     20
     D  XXFND                  1      3
     D  XXFYR                  4      6
     D  XXCASE                 7     13
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Ancillary Bill -
     D                                     Transfers')
     D OPTNMT          C                   CONST('Ancillary Bill -
     D                                     Transfer')
     D OPTNMD          C                   CONST('Ancillary Bill -
     D                                     Deletion')
     D COMAPI          C                   CONST('FA007R')
     D ZEROS           C                   CONST('000000000')

     I*
     IPM101
     I              C1CO#                       M1CO#
     I              C1EOB#                      M1EOB#
     I*
     I***PY SCOPYMT01I

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RTCD            1
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C                   PARM                    ##SUB#
     C                   PARM                    ##FYR
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTYYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      '*YYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     M1KY          KLIST
     C                   KFLD                    ##CO#
     C*
     C     CMKY1         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C                   KFLD                    ##FYR
     C                   KFLD                    ##CASE
     C*
     C     CMKY2         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    SCFND
     C                   KFLD                    SCFYR
     C                   KFLD                    SCCASE
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CMSS#         KKSS#
     C     *LIKE         DEFINE    CMACDT        KKACDT
     C     *LIKE         DEFINE    CMSS#         ##SS#
     C     *LIKE         DEFINE    CMCO#         ##CO#
     C     *LIKE         DEFINE    CMSUB#        ##SUB#
     C     *LIKE         DEFINE    CMFND         ##FND
     C     *LIKE         DEFINE    CMFYR         ##FYR
     C     *LIKE         DEFINE    CMCASE        ##CASE
     C     *LIKE         DEFINE    CMSUB#        KKSUB#
     C     *LIKE         DEFINE    CMFND         KKFND
     C     *LIKE         DEFINE    CMFYR         KKFYR
     C     *LIKE         DEFINE    CMCASE        KKCASE
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    PDMOD         HDMOD
     C     *LIKE         DEFINE    CMDT          XXFRDT
     C     *LIKE         DEFINE    CMDT          XXTODT
     C*
     C/COPY SCOPYMT01C
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
     C*================================================================
     C* ERRCK - Error checking for why this bill wasn't processed.
     C*================================================================
     C     ERRCK         BEGSR
     C*
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C*
     C                   MOVEL     C1LCLM        CLAIM#
     C                   MOVE      XXFND         SCFND
     C                   MOVE      XXFYR         SCFYR
     C                   MOVE      XXCASE        SCCASE
     C     C1PRFL        IFEQ      'P'
     C     C1CASE        ANDNE     *ZEROS
     C                   MOVE      C1FND         SCFND
     C                   MOVE      C1FYR         SCFYR
     C                   MOVE      C1CASE        SCCASE
     C                   ENDIF
     C*
     C* Check WCMC Claim Master to see if SS#/ACDT exists.
     C*
     C     CMKY2         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        21
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
     C                   Z-ADD     *ZEROS        KKMOD
     C                   Z-ADD     *ZEROS        KKFEIN
     C     C1PRFL        IFEQ      'E'
     C     C1PFEI        ANDNE     *ZEROS
     C                   MOVE      C1PFEI        KKFEIN
     C                   MOVE      C1MOD         KKMOD
     C                   ENDIF
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PDSTAT        ANDEQ     'I'
     C                   SETON                                        22
     C                   MOVEL     'Y'           XXCHG
     C                   ENDIF
     C*
     C* Payment error
     C*
     C                   SETOFF                                       26
     C     C1PRFL        IFEQ      'P'
     C                   SETON                                        26
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C*
     C* Check WCMC Claim Master to see if the claim number exists
     C*
     C     *IN90         IFEQ      *OFF
     C*
     C                   Z-ADD     ##SUB#        KKSUB#
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCCASE        KKCASE
     C     CMKY2         CHAIN     PCM01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF
     C                   ENDIF
     C*
     C* FEIN/MOD does not exist.
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     SCFEIN        KKFEIN
     C                   Z-ADD     SCMOD         KKMOD
     C*
     C     PDKY          CHAIN     PPD01                              99
     C     *IN99         IFEQ      *ON
     C     *IN99         OREQ      *OFF
     C     PDSTAT        ANDEQ     'I'
     C                   SETON                                        7290
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ELSE
     C*
     C* DELETION REASON CAN'T BE BLANKS
     C*
     C     SCDLRE        IFEQ      *BLANKS
     C     *IN90         ANDEQ     *OFF
     C                   SETON                                        7390
     C                   ENDIF
     C                   ENDIF                                                  *IN16 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=Search
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C     HFLD          WHENEQ    'SCFND'
     C     HFLD          OREQ      'SCFYR'
     C     HFLD          OREQ      'SCCASE'
     C*
     C                   CALL      'F0269R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      *ZEROS        ##SUB#
     C                   PARM      *ZEROS        ##FND
     C                   PARM      *ZEROS        ##FYR
     C                   PARM      *ZEROS        ##CASE
     C                   PARM      *BLANKS       ##SS#
     C                   PARM      *ZEROS        ##ACDT            8 0
     C                   PARM      ' '           ##RTCD            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FYR         OREQ      *ZEROS
     C     ##CASE        OREQ      *ZEROS
     C                   SETON                                        3190
     C                   ELSE
     C     CMKY1         CHAIN     PCM01                              99
     C*
     C                   MOVEL(P)  CMLSN         ##LAST
     C                   MOVEL(P)  CMFSN         ##FRST
     C                   Z-ADD     CMFND         SCFND
     C                   Z-ADD     CMFYR         SCFYR
     C                   Z-ADD     CMCASE        SCCASE
     C                   MOVE      '1'           ##TERM
     C*
     C                   CALL      'SRNAME'
     C                   PARM                    ##LAST           56
     C                   PARM                    ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  ##SS#         SCSSN
      /Free
       scSSN = MaskData('SSN':'##SS#':##SS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     ##ACDT        SCDOI
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C* PROVIDER SEARCH
     C*
     C     HFLD          WHENEQ    'SCFEIN'
     C     HFLD          OREQ      'SCMOD'
     C*
     C                   CALL      'F0032R'
     C                   PARM      '2'           ##CALL
     C                   PARM      *BLANKS       ##FEIN            9
     C                   PARM      *BLANKS       ##MOD             3
     C                   PARM      *BLANKS       ##RTCD
     C                   PARM      *BLANKS       ##TYPE            1
     C*
     C     ##RTCD        IFEQ      'A'
     C     ##RTCD        OREQ      'B'
     C     ##FEIN        OREQ      *BLANKS
     C                   SETON                                        9032
     C                   ELSE
     C                   MOVE      ##FEIN        SCFEIN
     C                   MOVE      ##MOD         SCMOD
     C*
     C                   CALL      'FA725R'
     C                   PARM      SCFEIN        #1FEIN            9 0
     C                   PARM      SCMOD         #1MOD             3 0
     C                   PARM      *BLANKS       #1NAME           40
     C                   PARM      *BLANKS       #1ERR             1
     C     #1ERR         IFNE      '0'
     C                   MOVEL(P)  #1NAME        SCPNAM
     C                   ENDIF
     C                   ENDIF                                                  ##RTCD = A
     C*
     C                   ENDSL
     C*
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN16         IFEQ      *OFF
     C                   EXSR      ERRCK
     C                   ENDIF
     C*
     C                   MOVEL(P)  C1AUTH        SCBLID
     C*
     C                   CALL      'SRNAME'
     C                   PARM      CMLSN         ##LAST           56
     C                   PARM      CMFSN         ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
     C*
     C     C1PRFL        IFEQ      'P'
     C     C1CASE        ANDNE     *ZEROS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOI
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL     CMSS#         SCSSN
      /Free
       scSSN = MaskData('SSN':'CMSS#':CMSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C*
     C                   ELSE
     C*
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  C1PSS#        SCSSN
      /Free
       scSSN = MaskData('SSN':'C1PSS#':C1PSS#);
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   MOVEL     C1ACDT        XXDATE            8 0
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCDOI
     C*
     C                   ENDIF                                                  C1PRFL = 'P'
     C*
     C     C1PRFL        IFEQ      'P'
     C     *IN22         OREQ      '0'
     C*
     C                   Z-ADD     KKFEIN        SCFEIN
     C                   Z-ADD     KKMOD         SCMOD
     C     PDDBA1        IFNE      'Y'
     C     PDFSN         IFEQ      *BLANKS
     C                   MOVE      PDNAME        SCPNAM
     C                   ELSE
     C     PDFSN         CAT(P)    PDNAME:1      SCPNAM
     C                   ENDIF
     C                   MOVE      PDADR1        SCPAD1
     C                   ELSE
     C                   MOVE      PDADR1        SCPNAM
     C                   MOVE      PDADR2        SCPAD1
     C                   ENDIF
     C                   MOVE      PDCTY         SCPCTY
     C                   MOVE      PDST          SCPST
     C                   MOVE      PDZIP         SCPZIP
     C*
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       SCDLRE
     C*
     C     C1PRFL        IFEQ      'P'
     C*
     C*  PAID
     C*
     C     C1TDUE        IFNE      *BLANKS
     C*
     C                   EXSR      PRTDUE
     C*
     C     C1REV         IFEQ      'Y'
     C                   Z-SUB     XXTDUE        XXAMT1            9 2
     C                   ELSE
     C                   Z-ADD     XXTDUE        XXAMT1
     C                   ENDIF
     C                   MOVE      XXAMT1        SCPAID
     C*
     C                   ENDIF
     C*
     C                   MOVEL     C1RXFD        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCFRDT
     C*
     C                   MOVEL     C1RXFD        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCTODT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C     *IN16         IFEQ      *ON
     C*
     C                   MOVEL(P)  Q1USER        C1DLUS
     C                   MOVEL(P)  Q1PGM         C1DLPG
     C*
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSTIME        C1DLTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         C1DLDT
     C                   MOVEL     SRDTO         C1PRDT
     C*
     C                   MOVEL(P)  SCDLRE        C1DLRE
     C                   MOVEL(P)  'D'           C1PRFL
     C*
     C                   Z-ADD     *ZEROS        C1CO#
     C                   Z-ADD     *ZEROS        C1SUB#
     C                   Z-ADD     *ZEROS        C1FND
     C                   Z-ADD     *ZEROS        C1FYR
     C                   Z-ADD     *ZEROS        C1CASE
     C                   Z-ADD     *ZEROS        C1EOB#
     C                   MOVEL     *BLANKS       C1PSS#
     C                   Z-ADD     *ZEROS        C1PFEI
     C                   Z-ADD     *ZEROS        C1MOD
     C                   Z-ADD     *ZEROS        C1ACDT
     C*
     C                   ELSE
     C*
     C* Now that the the errors have been corrected let's check for
     C* duplicate payments
     C*
     C                   SETOFF                                       91
     C     *IN90         IFEQ      *OFF
     C     C1PRFL        ANDEQ     'E'
     C*
     C*  PAID
     C*
     C     C1TDUE        IFNE      *BLANKS
     C*
     C                   EXSR      PRTDUE
     C*
     C     C1REV         IFEQ      'Y'
     C                   Z-SUB     XXTDUE        XXAMT1            9 2
     C                   ELSE
     C                   Z-ADD     XXTDUE        XXAMT1
     C                   ENDIF
     C*
     C                   ENDIF                                                  C1TDUE <> ' '
     C*
     C     SCPAID        IFNE      *ZEROS
     C*
     C                   MOVEL     C1RXFD        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXFRDT
     C*
     C                   MOVEL     C1RXFD        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         XXTODT
     C*
     C                   MOVE      CMCO#         #1CO#
     C                   MOVE      CMSUB#        #1SUB#
     C                   MOVE      CMFND         #1FND
     C*
     C                   CALL      'FA700R'
     C                   PARM      *BLANKS       ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM      *BLANKS       #1WCF#            3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C*
     C     ##RTCD        IFEQ      *BLANKS
     C                   MOVE      #1WCF#        $#FND
     C                   ELSE
     C                   Z-ADD     *ZEROS        $#FND
     C                   ENDIF
     C*
     C* Check for duplicate request/payment
     C*
     C                   CALL      'WA052R'
     C                   PARM      CMCO#         $#CO#             3 0          Company #
     C                   PARM                    $#FND             3 0          Fund
     C                   PARM      CMFYR         $#FYR             3 0          Fund Year
     C                   PARM      CMCASE        $#CASE            7 0          Case
     C                   PARM      XXFRDT        $#FROM            7 0          From Date
     C                   PARM      XXTODT        $#TO              7 0          To Date
     C                   PARM      SCPAID        $#AMT             9 2          Amount
     C                   PARM      'ME'          $#RECD            2            Reserve Code
     C                   PARM      PDFEIN        $#FEIN            9 0          FEIN #
     C                   PARM      'M'           $#RETP            1            Reserve Type
     C                   PARM      *BLANKS       $#RTCD            2
     C*
     C     $#RTCD        IFNE      *BLANKS
     C                   EXFMT     WDW01
     C     *INKL         IFEQ      *ON
     C                   SETON                                        91
     C                   SETOFF                                       KL
     C                   MOVEL     'P'           C1PRFL
     C                   MOVEL     'P'           @@RTCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     CMCO#         C1CO#
     C                   Z-ADD     CMSUB#        C1SUB#
     C                   Z-ADD     CMFND         C1FND
     C                   Z-ADD     CMFYR         C1FYR
     C                   Z-ADD     CMCASE        C1CASE
     C                   MOVEL     CMSS#         C1PSS#
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTYYMD
     C                   MOVEL     SRDTO         C1ACDT
     C                   Z-ADD     PDFEIN        C1PFEI
     C                   Z-ADD     PDMOD         C1MOD
     C*
     C     *IN91         IFEQ      *OFF
     C     C1EOB#        IFEQ      *ZEROS
     C     C1PRFL        ANDEQ     'E'
     C     C1EOB#        OREQ      *ZEROS
     C     C1PRFL        ANDEQ     'P'
     C     M1KY          CHAIN     PM101                              99
     C     *IN99         IFEQ      *OFF
     C                   ADD       1             M1EOB#
     C                   UPDATE    PM101
     C                   ENDIF
     C                   Z-ADD     M1EOB#        C1EOB#
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     *BLANKS       C1DLUS
     C                   MOVEL     *BLANKS       C1DLPG
     C                   Z-ADD     *ZEROS        C1DLTM
     C                   Z-ADD     *ZEROS        C1DLDT
     C                   MOVEL     *BLANKS       C1DLRE
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C                   SETOFF                                       3132
     C*
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   SETON                                        LR
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   SETOFF                                       0916
     C                   SETOFF                                       212223
     C                   SETOFF                                       2425
     C*
     C     @@MODE        COMP      'D'                                    09
     C     @@CALL        COMP      'D'                                    16
     C*
     C                   Z-ADD     1             #OFSCR            1 0
     C                   MOVEL     'N'           XXCHG             1
     C*
     C     *IN16         IFEQ      *OFF
     C                   MOVEL(P)  OPTNMT        ##DATA
     C                   ELSE
     C                   MOVEL(P)  OPTNMD        ##DATA
     C                   ENDIF
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDX
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C*
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   UPDATE    DBRCDF
     C*
     C     *IN16         IFEQ      *OFF
     C     *IN91         ANDEQ     *OFF
     C                   CALL      'F8077R'
     C                   PARM      *BLANKS       ##DCID           12            Document ID
     C                   PARM      *ZEROS        ##RRN             9 0          FAC9P RRN
     C                   PARM      @@RRN         ##RRN2            9 0          FAC1P RRN
     C                   PARM      XXCHG         ##PRVC            1            PROV CHANGE
     C                   ENDIF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   WRITE     DBRCDF
     C                   FEOD      FAC1P
     C*
     C                   ENDSR
     C*-------------------------------------------------------------------
     C     PRTDUE        BEGSR
     C*** Identifying significant digits and decimal positions - Beg
     C*** C1TDUE = 'BBBBB1434BB'
     C                   Z-ADD     0             X                 2 0
     C                   Z-ADD     0             M                 2 0
     C                   Z-ADD     0             N                 2 0
     C                   Z-ADD     0             P                 2 0
     C                   Z-ADD     0             Y                 2 0
     C                   Z-ADD     0             Z                 2 0
     C                   Z-ADD     0             XXTDUE            9 2
     C                   MOVEL     ' '           WHO
     C                   MOVEL     ' '           XXDUE
     C     ' '           CHECK     C1TDUE        M                 2 0
     C     ' '           CHECKR    C1TDUE        N                 2 0
     C*** M = 6 AND N = 9
     C     N             SUB       M             P                 2 0
     C                   ADD       1             P
     C*** P = 4
     C     P             SUBST     C1TDUE:M      XXDUE            11
     C*** XXFEEC = '900BBBBBBBBB'
     C                   Z-ADD     11            Y                 2 0
     C     P             DOWNE     0
     C*** P = 4 initially  and Y = 11
     C     1             SUBST     XXDUE:P       WHO               1
     C*** XXFEEC Pth character is moved to the Yth character of FEE
     C                   MOVE      WHO           DUE(Y)
     C                   SUB       1             Y
     C                   SUB       1             P
     C                   ENDDO
     C*
     C* Now P = 0 and Y = 9
     C     Y             DOWNE     0
     C                   MOVEL     '0'           DUE(Y)
     C                   SUB       1             Y
     C                   ENDDO
     C                   MOVEA     DUE           X1TDUE           11
     C                   MOVE      X1TDUE        XXTDUE
     C*** Identifying significant digits and decimal positions - End
     C                   ENDSR
