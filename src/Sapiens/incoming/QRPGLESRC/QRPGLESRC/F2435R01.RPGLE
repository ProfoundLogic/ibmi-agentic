     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE F2435R01 - BILL TYPE CODE INFORMATION
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program updates or displays the Bill Status Code   **
     H**     table.                                                  **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     F2435R - Work with Bill Type Codes                      **
     H**   CALLS TO:                                                 **
     H**     None                                                    **
     H**                                                             **
     H**   PROGRAMMER: C:85                                          **
     H**   CREATED:   04/03/98      PROJECT: MC152                   **
     H*****************************************************************
     H*
     H*****************************************************************
     H** REVISIONS:
     H*****************************************************************
     H*
     H*  04/19/06 C:163 PROJECT 114WMF
     H*                 Back to Base - EOMB Processing (114MEO)
     H*
     H*  06/24/99 C:62 PROJECT: MC202
     H*                ADDING PEND LETTER FLAG.
     H*
     H*  10/07/98 C:94 PROJECT: 87MFX
     H*                CHANGE F12=PREVIOUS TO F12=CANCEL
     H*
     H*  09/10/98 C:56 PROJECT: MC152
     H*                Change title to 'Bill type codes'
     H*  08/06/98  C:94  PROJECT: MCMNT
     H*                  ON AN ADD DEFAULT STATUS TO 'A' IF THERE IS
     H*                  NO STATUS IN FILE.
     H*
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      60      General usage                                  **
     H**      87      Error: MCR0355 - Code can not be blanks        **
     H**      88      Error: MCR0900 - Code already exists           **
     H**      89      Error: MCR0355 - Description can not be blanks **
     H*****************************************************************
     H*
     H*****************************************************************
     H** FILE USAGE:                                                 **
     H**   FILES:                                                    **
     H**      FTBSP  (PFRCDF) : Bill Status Code Table File          **
     H**      FTBSL  (      ) : Logical Over Table Files (CO#/TYPE/  **
     H**                        CODE)                                **
     H**             (DBRCDF) : Bill Status Code Table File          **
     H**      FTBSL1 (      ) : Logical Over Table Files (CO#/DESC)  **
     H**             (DBRCDF2): Bill Status Code Table File          **
     H*****************************************************************
     H*
     FF2435F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFTBSP     UF A E             DISK
     F                                     RENAME(PBS01:DBRCDF)
     FFTBSL     IF   E           K DISK
     F*  Physical file definition (for retrieving type description)
     FFTGTP     IF   E             DISK
     F*
     F*  Logical file definition (for retrieving type description)
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*
     F*  Logical file definition (for retrieving DARTS remark cd.desc.)
     FSTCTL1    IF   E           K DISK
     F*                                                                *********
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D*** BEG CHG *** 09/10/98 ****************************************
     D**********    'Bill Status Codes'   C         OPTNM
     D OPTNM           C                   CONST('Bill Status Type Cod-
     D                                     es')
     D*** END CHG *** 09/10/98 ****************************************
     D COMAPI          C                   CONST('FA007R')

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C*
     C     BSKY          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKTYPE
     C                   KFLD                    KKCODE
     C*
     C     KEY3          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKDESC
      *
     C     GTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
      *
     C*
     C*----------------------------------------------------------------
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*----------------------------------------------------------------
     C     *LIKE         DEFINE    BSCO#         KKCO#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    BSDESC        KKDESC
      *
     C     *LIKE         DEFINE    GTCO#         XXCO#
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXKEY
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C*----------------------------------------------------------------
     C*
     C/COPY SCOPYMT01C
     C*----------------------------------------------------------------
     C*================================================================
     C*  CHGSTS Status - User added subroutine
     C*================================================================
     C*
     C     CHGSTS        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             60
     C     *IN60         IFEQ      *OFF
     C     BSSTAT        IFEQ      'I'
     C                   MOVE      'A'           BSSTAT
     C                   ELSE                                                   STAT = 'I'
     C                   MOVE      'I'           BSSTAT
     C                   ENDIF                                                  STAT = 'I'
     C*
     C                   MOVEL(P)  Q1USER        BSUSER
     C                   MOVEL(P)  Q1PGM         BSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSDT
     C*
     C                   Z-ADD     DSTIME        BSTIME
     C*
     C                   UPDATE    DBRCDF
     C*
     C                   ENDIF                                                  *IN60 = *Off
     C*
     C                   ENDSR                                                  CHGSTS
     C*
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
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C                   MOVEA     '000000'      *IN(31)
     C*
     C* SCTYPE must not be blank
     C*
     C  N90
     CAN 40SCTYPE        IFEQ      *BLANK
     C*
     C                   SETON                                        7190
     C*
     C                   ENDIF                                                  SCTYPE=BLANK
     C*
     C* Chain to check SCTYPE
     C*
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     ##CO#         XXCO#
     C                   MOVEL(P)  'BLPR'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C*
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN72
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  GTDESC        SCTDES
     C*** END   ADD *** 06/24/99 **************************************
     C                   ENDIF                                                  *IN60=ON
     C                   ENDIF                                                  *IN90=OFF
     C*** BEGIN DEL *** 06/24/99 **************************************
     C*
     C* If SCTYPE is equal to 'P' then execute screen 2
     C*
     C**N90***** SCTYPE    IFEQ 'P'
     C**********           Z-ADD2         #OFSCR
     C**********           ELSE
     C**********           Z-ADD1         #OFSCR
     C**********           ENDIF                           SCTYPE='P'
     C*** END   DEL *** 06/24/99 **************************************
     C*
     C* SCCODE must not be blank
     C*
     C  N90SCCODE        IFEQ      *BLANK
     C                   SETON                                        7390
     C                   ENDIF                                                  SCCODE=BLANK
     C*
     C* DON'T ALLOW DUPLICATES
     C*
     C     *IN90         IFEQ      *OFF
     C     *IN40         ANDEQ     *ON
     C*
     C                   MOVE      ##CO#         KKCO#
     C                   MOVEL(P)  SCTYPE        KKTYPE
     C                   MOVEL(P)  SCCODE        KKCODE
     C*
     C     BSKY          CHAIN     PBS01                              99
     C*
     C  N99              SETON                                        7490
     C*
     C                   ENDIF
     C*
     C* SCDESC must not be blank
     C*
     C*** BEGIN DEL *** 06/24/99 **************************************
     C********** *IN40     IFEQ *ON
     C*** END   DEL *** 06/24/99 **************************************
     C  N90SCDESC        IFEQ      *BLANKS
     C                   SETON                                        7590
     C                   ENDIF                                                  SCDESC=BLANK
     C*** BEGIN DEL *** 06/24/99 **************************************
     C**********           ENDIF                           *IN40=ON
     C*** END   DEL *** 06/24/99 **************************************
     C*** BEGIN ADD *** 06/24/99 **************************************
     C*
     C* VALID VALUES ARE 'Y' OR 'N' IF THE TYPE IS 'P'.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDEQ     'P'
     C     SCLTR         ANDNE     'Y'
     C     SCLTR         ANDNE     'N'
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF
     C*
     C* VALID VALUES ARE 'N' OR ' ' IF THE TYPE IS NOT 'P'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     'P'
     C     SCLTR         ANDNE     'N'
     C     SCLTR         ANDNE     ' '
     C*
     C                   SETON                                        7790
     C*
     C                   ENDIF
     C*
     C* MAKE SURE THAT PEND LETTER IS NOT 'Y' IF TYPE IS NOT 'P'.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     'P'
     C     SCLTR         ANDEQ     'Y'
     C*
     C                   SETON                                        7890
     C*
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*** BEGIN ADD *** 04/19/06 **************************************
     C*
     C* VALID VALUES ARE 'Y' OR 'N' IF THE TYPE IS 'P'.
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDEQ     'P'
     C     SCLTTY        ANDNE     'T'
     C     SCLTTY        ANDNE     ' '
     C                   SETON                                        7990
     C                   ENDIF
     C*
     C* VALUE MUST BE BLANK IF TYPE IS NOT 'P'
     C*
     C     *IN90         IFEQ      *OFF
     C     SCTYPE        ANDNE     'P'
     C     SCLTTY        ANDNE     *BLANKS
     C     *IN90         OREQ      *OFF
     C     SCLTR         ANDNE     'Y'
     C     SCLTTY        ANDNE     *BLANKS
     C                   SETON                                        8090
     C                   ENDIF
     C*** END   ADD *** 04/19/06 **************************************
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C                   MOVEA     '000000'      *IN(31)
     C*
     C* SCLTR must be 'Y' or 'N'
     C*
     C  N90SCLTR         IFNE      'Y'
     C     SCLTR         ANDNE     'N'
     C                   SETON                                        7190
     C                   ENDIF                                                  SCLTR<>'Y'or'N'
     C*
     C* SCRMCD must not be blank
     C*
     C  N90SCRMCD        IFEQ      *BLANK
     C                   SETON                                        7290
     C                   ENDIF                                                  SCRMCD=BLANK
     C*
     C* Chain to check SCRMCD
     C*
     C  N90SCRMCD        CHAIN     PCT01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN90
     C                   MOVE      *ON           *IN73
     C                   ENDIF                                                  *IN60=ON
     C*
     C* SCDIDT must be greater than 0
     C*
     C  N90SCDIDT        IFLE      *ZERO
     C                   SETON                                        7490
     C                   ENDIF                                                  SCDIDT<=0
     C*
     C* SCDIFL must not be blank
     C*
     C  N90SCLTR         IFEQ      'Y'
     C     SCDIFL        ORNE      *BLANKS
     C*
     C                   CALL      'F2435C'
     C                   PARM      *BLANKS       ##CALL            1
     C                   PARM      SCDIFL        ##FILE           10
     C*
     C     ##CALL        IFEQ      'F'
     C*
     C                   SETON                                        7590
     C*
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCDIFL=BLANK
     C*
     C* SCDIFD must not be blank
     C*
     C  N90SCLTR         IFEQ      'Y'
     C     SCDIFD        ANDEQ     *BLANKS
     C*
     C                   SETON                                        7690
     C*
     C                   ENDIF                                                  SCDIFD=BLANK
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for screen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH
     C*================================================================
     C     KJDO          BEGSR
     C*
     C                   SELECT
     C*
     C* SEARCH TYPE CODES
     C*
     C     HFLD          WHENEQ    'SCTYPE'
     C                   MOVEL     'BLPR'        ##TBID
     C*
     C                   CALL      'F2380R'      ACDPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        31
     C*
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCTYPE
     C                   MOVEL(P)  GTDESC        SCTDES
     C                   SETON                                        32
     C*
     C                   ENDIF
     C*
     C     HFLD          WHENEQ    'SCRMCD'
     C*
     C                   CALL      'SC305R'
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##DESC           40
     C                   PARM      *BLANKS       ##RTCD            1
     C*
     C     *INKL         IFEQ      *ON
     C     ##RMCD        OREQ      *BLANKS
     C*
     C                   SETON                                        31
     C*
     C                   ELSE
     C*
     C                   MOVEL     ##RMCD        SCRMCD
     C                   MOVEL     ##DESC        SCRDES
     C                   SETON                                        32
     C*
     C                   ENDIF
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
     C*----------------------------------------------------------------
     C*          Move data base fields to screen
     C*
     C*** BEG ADD *** 08/06/98 ************************************
     C     *IN40         IFEQ      *ON
     C     BSSTAT        ANDEQ     *BLANK
     C                   MOVEL(P)  'A'           SCSTAT
     C                   ELSE
     C                   MOVEL(P)  BSSTAT        SCSTAT
     C                   ENDIF
     C*** END ADD *** 08/06/98 ************************************
     C*** BEGIN ADD *** 06/24/99 **************************************
     C     *IN40         IFEQ      *OFF
     C*** END   ADD *** 06/24/99 **************************************
     C                   MOVEL(P)  BSTYPE        SCTYPE
     C                   MOVEL(P)  BSCODE        SCCODE
     C                   MOVEL(P)  BSDESC        SCDESC
     C*******              MOVELBSSTAT    SCSTAT    P      DEL 08/06/98
     C*
     C                   MOVEL(P)  BSLTR         SCLTR
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  BSLTTY        SCLTTY
     C*** END   ADD *** 04/19/06 **************************************
     C*** BEGIN DEL *** 06/24/99 **************************************
     C**********           MOVELBSRMCD    SCRMCD    P
     C**********           MOVELBSDIDT    SCDIDT    P
     C**********           MOVELBSDIFL    SCDIFL    P
     C**********           MOVELBSDIFD    SCDIFD    P
     C*** END   DEL *** 06/24/99 **************************************
     C*
     C* Move Type Description to screen
     C*
     C                   Z-ADD     ##CO#         XXCO#
     C                   MOVEL(P)  'BLPR'        XXTBID
     C                   MOVEL(P)  SCTYPE        XXKEY
     C*
     C     GTKY          CHAIN     PGT02                              60
     C*** BEGIN CHG *** 06/24/99 **************************************
     C********** *IN60     IFEQ *OFF
     C**********           MOVELGTDESC    SCTDES    P
     C**********           ENDIF                           *IN60=OFF
     C  N99              MOVEL(P)  GTDESC        SCTDES
     C   99              MOVEL(P)  *BLANKS       SCTDES
     C*** END   CHG *** 06/24/99 **************************************
     C*** BEGIN DEL *** 06/24/99 **************************************
     C*
     C* Move Remark Code Description to screen
     C*
     C********** SCRMCD    CHAINPCT01                60
     C********** *IN60     IFEQ *OFF
     C**********           MOVELCTDESC    SCRDES    P
     C**********           ENDIF                           *IN60=OFF
     C*** END   DEL *** 06/24/99 **************************************
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   ELSE
     C*
     C                   MOVEL(P)  *BLANKS       SCTYPE
     C                   MOVEL(P)  *BLANKS       SCCODE
     C                   MOVEL(P)  *BLANKS       SCDESC
     C                   MOVEL(P)  *BLANKS       SCLTR
     C                   MOVEL(P)  *BLANKS       SCTDES
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  *BLANKS       SCLTTY
     C*** END   ADD *** 04/19/06 **************************************
     C*
     C                   ENDIF
     C*** END   ADD *** 06/24/99 **************************************
     C*
     C*----------------------------------------------------------------
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*----------------------------------------------------------------
     C*          Move screen fields to data base
     C*
     C                   MOVE      ##CO#         BSCO#
     C                   MOVEL(P)  SCTYPE        BSTYPE
     C                   MOVEL(P)  SCCODE        BSCODE
     C                   MOVEL(P)  SCDESC        BSDESC
     C*** BEGIN ADD *** 06/24/99 **************************************
     C                   MOVEL(P)  SCLTR         BSLTR
     C*** END   ADD *** 06/24/99 **************************************
     C*** BEGIN ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  SCLTTY        BSLTTY
     C*** END   ADD *** 04/19/06 **************************************
     C                   MOVEL(P)  SCSTAT        BSSTAT
     C*
     C*** BEGIN DEL *** 06/24/99 **************************************
     C**********           MOVELSCLTR     BSLTR     P
     C**********           MOVELSCRMCD    BSRMCD    P
     C**********           MOVELSCDIDT    BSDIDT    P
     C**********           MOVELSCDIFL    BSDIFL    P
     C**********           MOVELSCDIFD    BSDIFD    P
     C*** END   DEL *** 06/24/99 **************************************
     C*
     C*----------------------------------------------------------------
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
     C*----------------------------------------------------------------
     C*
     C* Initialize number of screens to 1
     C*
     C                   Z-ADD     1             #OFSCR            9 0
     C*
     C*          If add: set on indicator 40
     C*
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'D'                                    09
     C*
     C                   ENDSR
     C*================================================================
     C* PREOTH - Pre-processing for other modes
     C*================================================================
     C     PREOTH        BEGSR
     C*
     C                   SELECT
     C     @@MODE        WHENEQ    'S'
     C*
     C                   EXSR      CHGSTS
     C                   RETURN
     C                   ENDSL
     C*
     C                   ENDSR
     C*================================================================
     C* UPDATR - Update data base record and after processing
     C*================================================================
     C     UPDATR        BEGSR
     C     @@RRN         CHAIN     DBRCDF                             99
B01  C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL(P)  Q1USER        BSUSER
     C                   MOVEL(P)  Q1PGM         BSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSDT
     C*
     C                   Z-ADD     DSTIME        BSTIME
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C                   ENDSR
     C*
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL(P)  Q1USER        BSUSER
     C                   MOVEL(P)  Q1PGM         BSPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSDT
     C*
     C                   Z-ADD     DSTIME        BSTIME
     C*
     C*** BEG ADD *** 08/06/98 *************************************
     C                   MOVEL(P)  Q1USER        BSIUSR
     C                   MOVEL(P)  Q1PGM         BSIPGM
     C                   TIME                    DSDTTM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         BSIDT
     C*
     C                   Z-ADD     DSTIME        BSITIM
     C*
     C*** END ADD *** 08/06/98 *************************************
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FTBSP
     C*
     C                   ENDSR
