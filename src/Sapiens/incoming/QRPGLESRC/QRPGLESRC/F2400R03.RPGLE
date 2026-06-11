     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H* PROGRAM: F2400R03                                             *
     H* DESCRIPTION: WORK WITH STATE FS RULES                         *
     H*****************************************************************
     H* CREATED: 10/17/02 C:79 PROJ:FS239                             *
     H*****************************************************************
     H* INDICATORS:
     H*****************************************************************
     H*                                                               *
     H* PROGRAMMER NOTES:                                             *
     H* 1.  When adding fields add to the I spec for PFR02 so         *
     H*     when copying rules all the field values will get          *
     H*     copied                                                    *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/20/18  C:079   Support : 99 71719                         *
     H*                    Cosmetic screen changes                    *
     H*
     H* 12/08/16 C:79 Project: SY387                                  *
     H*               Add effective and expiration dates to screen
     H*
     H* 10/15/07 C:47 Project: SEQ60443                               *
     H*               Add @@ST to call of F2330R.
     H*
     H* 07/03/05 C:79 Project: TN002                                  *
     H*               Add generic table search on PT per visit        *
     H*
     H* 10/31/03 C:79 Project: FS270                                  *
     H*               Add status                                      *
     H*
     H* 09/25/03 C:79 Project: TX020                                  *
     H*               Add field to pay amb miles rural,urban,etc      *
     H*
     H* 04/07/03 C:79 Project: PA020                                  *
     H*               Add field to pay P/T per visit rates            *
     H*****************************************************************
     H*
     FF2400F03  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFDFRP     UF A E             DISK
     F                                     RENAME(PFR01:DBRCDF)
     FFTABL     IF   E           K DISK    USROPN
     FFTMIL     IF   E           K DISK
     FSTSTL1    IF   E           K DISK
     FFTGTL     IF   E           K DISK
     F*** BEG ADD *** 09/25/03 ****************************************
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
     F*** END ADD *** 09/25/03 ****************************************
     D/COPY SCOPYMT01D
     D*
     D QCA             S             56    DIM(1) CTDATA PERRCD(1)              CMD EXEC/QCMDEXC
     D*
     D***PY SCOPYMT01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('State FS Rules Maint-
     D                                     enance')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*
     D*** DEFAULT DATE AND TIME
     D*
     D                 DS
     D  DFDTTM                 1     12  0
     D  DFTIME                 1      6  0
     D  DFDATE                 7     12  0
     D*
     D*    USING DATA STRUCTURE FOR DATABASE OVERIDE
     D*
     D                 DS
     D  @@CMD                  1     56
     D  XXST                  38     39

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
     C                   PARM                    @@CO#             3 0
     C                   PARM                    @@RRN3            9 0
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
     C*
     C*  EOMB SEARCH
     C     EBKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQKEY
     C*
     C*  MESSAGE DESCRIPTION
     C*
     C     MGKY          KLIST
     C                   KFLD                    XXMSG
     C*
     C*  MESSAGE DESCRIPTION
     C*
     C     MIKY          KLIST
     C                   KFLD                    KKMSG#
     C*
     C*  STATE DESCRIPTION
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST
     C*
     C     GTKY          KLIST
     C                   KFLD                    QQCO#
     C                   KFLD                    QQID
     C                   KFLD                    QQKEY
     C*
     C*================================================================
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*
     C     *LIKE         DEFINE    FRBTOS        XXTOS
     C     *LIKE         DEFINE    FRST          XXST1                                         2
     C     *LIKE         DEFINE    FRMSG         XXMSG                                         2
     C     *LIKE         DEFINE    MIDESC        XXDESC                                        2
     C     *LIKE         DEFINE    MIMSG#        KKMSG#                                        2
     C     *LIKE         DEFINE    STKEY         KKST                                          2
     C     *LIKE         DEFINE    FRRULE        KKRULE                                        2
     C     *LIKE         DEFINE    FRST          K1ST                                          2
     C     *LIKE         DEFINE    GTTBID        QQID                                          2
     C*================================================================
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
     C* ERRCK1 - Error checking for srceen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*** BEG ADD *** 04/07/03 ****************************************
     C*
     C*   VALIDATE PAY PHYSICAL THERAPY PER VISIT FIELD
     C*
     C  N90SCPPTV        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPPTV        QQKEY
     C                   MOVEL(P)  'TYPT'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        9077
     C*
     C                   ENDIF
     C*
     C*** END ADD *** 04/07/03 ****************************************
     C*
     C*****************************************************************
     C*
     C*   VALIDATE MULTIPLE REDUCTIONS FLAG
     C*
     C  N90SCMR          IFNE      *BLANK
     C     SCMR          ANDNE     'Y'
     C*
     C                   SETON                                        9071
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C*   VALIDATE MILEAGE RURAL,URBAN, ETC
     C*
     C  N90SCPARE        IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCPARE        QQKEY
     C                   MOVEL(P)  'ZRAR'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C   99              SETON                                        9078
     C*
     C                   ENDIF
     C*** END ADD *** 09/25/03 ****************************************
     C*****************************************************************
     C*
     C*   VALIDATE 2ND PASS FLAG
     C*
     C  N90SCPAS2        IFNE      *BLANK
     C     SCPAS2        ANDNE     'Y'
     C*
     C                   SETON                                        9072
     C*
     C                   ENDIF
     C*****************************************************************
     C*
     C*   VERIFY EOB CODE IS VALID
     C*
     C  N90SCEOB         IFNE      *BLANK
     C*
     C                   MOVEL(P)  SCEOB         QQKEY
     C     EBKY          CHAIN     PEB01                              60
     C   60              SETON                                        9073
     C     EBSTAT        IFEQ      'I'
     C                   SETON                                        9073
     C                   ENDIF
     C                   ENDIF
     C*****************************************************************
     C*   VERIFY ERROR MESSAGE IS A VALID VALUE
     C*
     C  N90SCMSG         IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG         XXMSG
     C     MGKY          CHAIN     PMI01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9074
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCMSG         XXST1
     C*
     C     XXST1         IFNE      SCST
     C                   SETON                                        9074
     C                   ENDIF                                                  XXST1 NE SCST
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF                                                  SCMSG NE ' '
     C*
     C  N90SCMSG2        IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG2        XXMSG
     C     MGKY          CHAIN     PMI01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9075
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCMSG2        XXST1
     C*
     C     XXST1         IFNE      SCST
     C                   SETON                                        9075
     C                   ENDIF                                                  XXST1 NE SCST
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF                                                  SCMSG2 NE' '
     C*
     C  N90SCMSG3        IFNE      *BLANKS
     C                   MOVEL(P)  SCMSG3        XXMSG
     C     MGKY          CHAIN     PMI01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   SETON                                        9076
     C*
     C                   ELSE
     C*
     C                   MOVEL     SCMSG3        XXST1
     C*
     C     XXST1         IFNE      SCST
     C                   SETON                                        9076
     C                   ENDIF                                                  XXST1 NE SCST
     C*
     C                   ENDIF                                                  *IN60 = *ON
     C*
     C                   ENDIF                                                  SCMSG3 NE' '
     C*
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
     C                   ENDSR                                                  END SCRND
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C     *IN40         IFEQ      *ON
     C*
     C                   MOVE      @@ST          SCST
     C*
     C*    CLEARING PROCESSING INFORMATION
     C*
     C                   MOVE      *BLANK        SCEOB
     C                   MOVE      *BLANK        SCMSG
     C                   MOVE      *BLANK        SCMSG2
     C                   MOVE      *BLANK        SCMSG3
     C                   MOVE      *BLANKS       SCEOBD
     C                   MOVE      *BLANKS       SCMSGD
     C                   MOVE      *BLANKS       SCMSD2
     C                   MOVE      *BLANKS       SCMSD3
     C                   MOVE      *BLANKS       SCMR
     C                   MOVE      *BLANKS       SCPAS2
     C                   MOVE      *BLANKS       SCPCDS
     C*** BEG ADD *** 04/09/03 ****************************************
     C                   MOVE      *BLANKS       SCPPTV
     C*** END ADD *** 04/09/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      *BLANKS       SCPARE
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 10/31/03 ****************************************
     C                   MOVE      'A'           SCSTAT
     C*** END ADD *** 10/31/03 ****************************************
     C*** BEG ADD *** 12/08/16 ****************************************
     C                   Eval      scEfDt = *zeros
     C                   Eval      scExDt = *zeros
     C*** END ADD *** 12/08/16 ****************************************
     C*
     C                   ELSE
     C*
     C                   MOVE      FRST          SCST
     C                   Z-ADD     FRRULE        SCRULE
     C                   MOVE      FRPTY         SCPTY
     C                   MOVE      FRPCAT        SCPCAT
     C                   MOVE      FRDESC        SCDESC
     C                   MOVE      FREOB         SCEOB
     C                   MOVE      FRMSG         SCMSG
     C                   MOVE      FRMSG2        SCMSG2
     C                   MOVE      FRMSG3        SCMSG3
     C                   MOVE      FRMR          SCMR
     C                   MOVE      FRPAS2        SCPAS2
     C*** BEG ADD *** 04/09/03 ****************************************
     C                   MOVE      FRPPTV        SCPPTV
     C*** END ADD *** 04/09/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      FRPARE        SCPARE
     C*** END ADD *** 09/25/03 ****************************************
     C*** BEG ADD *** 10/31/03 ****************************************
     C                   MOVE      FRSTAT        SCSTAT
     C*** END ADD *** 10/31/03 ****************************************
     C*** BEG ADD *** 12/08/16 ****************************************
     C                   Z-ADD     FREFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEFDT
     C                   Z-ADD     FREXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCEXDT
     C*** END ADD *** 12/08/16 ****************************************
     C*
     C* GET EOB CODE DESCRIPTION
     C*
     C*
     C     SCEOB         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCEOB         QQKEY
     C*
     C     EBKY          CHAIN     PEB01                              60
     C                   MOVEL     EBDESC        SCEOBD
     C*
     C                   ELSE
     C                   MOVE      *BLANKS       SCEOBD
     C*
     C                   ENDIF                                                  SCEOB NE BLANKS
     C*
     C* GET MESSAGE DESCRIPTION
     C*
     C     SCMSG         IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCMSG         KKMSG#
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        SCMSGD
     C                   ELSE
     C                   MOVE      *BLANKS       SCMSD2
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCMSG NE BLANKS
     C*
     C     SCMSG2        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCMSG2        KKMSG#
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        SCMSD2
     C                   ELSE
     C                   MOVE      *BLANKS       SCMSD2
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCMSG2 NE BLANK
     C*
     C     SCMSG3        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  SCMSG3        KKMSG#
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        SCMSD3
     C                   ELSE
     C                   MOVE      *BLANKS       SCMSD3
     C                   ENDIF
     C*
     C                   ENDIF                                                  SCMSG3 NE BLANK
     C*
     C                   ENDIF                                                  *IN40 = *ON
     C*
     C                   TIME                    DFDTTM
     C*
     C* GET THE PROCEDURE CATEGORY DESCRIPTION
     C*
     C                   MOVEL(P)  SCPCAT        QQKEY
     C                   MOVEL(P)  'PCAT'        QQID
     C*
     C     GTKY          CHAIN     PGT01                              99
     C*
     C  N99              MOVEL(P)  GTDESC        SCPCDS
     C   99              MOVE      *BLANKS       SCPCDS
     C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   MOVEL     SCEOB         FREOB
     C                   MOVEL     SCMSG         FRMSG
     C                   MOVEL     SCMSG2        FRMSG2
     C                   MOVEL     SCMSG3        FRMSG3
     C                   MOVE      SCMR          FRMR
     C                   MOVE      SCPAS2        FRPAS2
     C*** BEG ADD *** 04/09/03 ****************************************
     C                   MOVE      SCPPTV        FRPPTV
     C*** END ADD *** 04/09/03 ****************************************
     C*** BEG ADD *** 09/25/03 ****************************************
     C                   MOVE      SCPARE        FRPARE
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C*******************
     C* F10=SEARCH      *
     C*******************
     C*
     C     *INKJ         IFEQ      '1'
     C*
     C* SET OFF POSITIONING INDICATORS
     C*
     C                   SETOFF                                       202122
     C                   SETOFF                                       232526
     C*** BEG ADD *** 07/03/05 ****************************************
     C*
     C***********************************
     C*  PT PER VISIT SEARCH
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPPTV'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'TYPT'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPPTV
     C                   SETON                                        9025
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9026
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF
     C*** BEG ADD *** 09/25/03 ****************************************
     C*
     C***********************************
     C*  REGION SEARCH RURAL,URBAN,ETC  *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCPARE'
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      0             ##RRN             9 0
     C                   PARM      'ZRAR'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C*
     C     ##RRN         CHAIN     PGT02                              99
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL     GTKEY         SCPARE
     C                   SETON                                        9024
     C*
     C                   ENDIF                                                  *IN99 = *OFF
     C*
     C                   ELSE
     C                   SETON                                        9025
     C*
     C                   ENDIF                                                  ##RRN NE *ZEROS
     C*
     C                   ENDIF
     C*** END ADD *** 09/25/03 ****************************************
     C*
     C***********************************
     C*  EOB CODE SEARCH                *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCEOB'
     C*
     C                   CALL      'F2330R'
     C                   PARM      '2'           QQCALL            1
     C     *INKC         PARM      '0'           QQKC              1
     C     *INKL         PARM      '0'           QQKL              1
     C*** BEG CHG *** 10/15/07 ****************************************
     C***                  PARM           QQST    2
     C                   PARM      @@ST          QQST              2
     C*** END CHG *** 10/15/07 ****************************************
     C                   PARM      *BLANKS       QQKEY            10
     C*
     C     QQKEY         IFEQ      *BLANKS
     C*
     C                   SETON                                        20
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  QQKEY         SCEOB
     C*
     C     EBKY          CHAIN     PEB01                              60
     C                   MOVEL     EBDESC        SCEOBD
     C*
     C                   SETON                                        21
     C*
     C                   ENDIF                                                  QQKEY IFEQ BLAN
     C*
     C                   ENDIF                                                  HFLD IFEQ SCEOB
     C***********************************
     C*  ERROR MESSAGES                 *
     C***********************************
     C*
     C     HFLD          IFEQ      'SCMSG'
     C     HFLD          OREQ      'SCMSG2'
     C     HFLD          OREQ      'SCMSG3'
     C*
     C                   MOVE      *BLANK        QQKEY
     C                   CALL      'F2385R'
     C                   PARM      '2'           QQCALL
     C                   PARM      @@RRN3        QQRRN             9 0
     C                   PARM                    QQTYPE            1
     C                   PARM      SCST          QQST              2
     C                   PARM      *BLANKS       QQMSG#            7
     C*
     C* GET MESSAGE DESCRIPTION
     C*
     C     QQMSG#        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  QQMSG#        KKMSG#
     C*
     C     MIKY          CHAIN     PMI01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  MIDESC        XXDESC
     C                   ELSE
     C                   MOVE      *BLANKS       XXDESC
     C                   ENDIF
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCMSG'
     C                   MOVEL(P)  QQMSG#        SCMSG
     C                   MOVEL(P)  XXDESC        SCMSGD
     C                   SETON                                        22
     C*
     C     HFLD          WHENEQ    'SCMSG2'
     C                   MOVEL(P)  QQMSG#        SCMSG2
     C                   MOVEL(P)  XXDESC        SCMSD2
     C                   SETON                                        23
     C*
     C     HFLD          WHENEQ    'SCMSG3'
     C                   MOVEL(P)  QQMSG#        SCMSG3
     C                   MOVEL(P)  XXDESC        SCMSD3
     C                   SETON                                        23
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCMSG'
     C                   SETON                                        21
     C*
     C     HFLD          WHENEQ    'SCMSG2'
     C                   SETON                                        22
     C*
     C     HFLD          WHENEQ    'SCMSG3'
     C                   SETON                                        23
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  QQMSG# NE BLANK
     C*
     C                   ENDIF                                                  HFLD = SCMSG
     C*
     C                   SETON                                        90
     C*
     C                   ENDIF                                                  *INKJ = *ON
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C                   CLOSE     FTABL
     C                   MOVE      *BLANKS       OPNFLG
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C     @@MODE        COMP      '1'                                    09
     C     @@MODE        COMP      '2'                                    09
     C     @@MODE        COMP      '3'                                    09
     C     @@MODE        COMP      'D'                                    09
     C     @@MODE        COMP      'A'                                    40
     C     @@MODE        COMP      'P'                                    41
     C*
     C                   MOVEL     @@CO#         QQCO#             3 0
     C*
     C*   CRTOVERIDES PER STATE
     C*
     C* OVERRIDE DATABASE FILE FTABL
     C*
     C                   MOVEL     QCA(1)        @@CMD
     C                   MOVEL     @@ST          XXST
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    @@CMD
     C                   PARM                    LENGTH           15 5
     C*
     C     OPNFLG        IFEQ      *BLANKS
     C                   OPEN      FTABL
     C                   MOVE      'Y'           OPNFLG            1
     C                   ENDIF
     C*
     C* GET DESCRIPTION FOR STATE
     C*
     C                   MOVEL     XXST          KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STNAME        SCSTDS
     C                   ENDIF
     C*
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C*
     C                   ENDSR
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C*
B01  C     *IN99         IFEQ      *OFF
     C*
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  Q1USER        FRUSER
     C                   MOVEL(P)  Q1PGM         FRPGM
     C                   MOVEL     DFTIME        FRTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE            7 0
     C                   Z-ADD     XXDATE        FRDT
     C*
     C                   UPDATE    DBRCDF
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   MOVEL(P)  Q1USER        FRUSER
     C                   MOVEL(P)  Q1PGM         FRPGM
     C                   MOVEL     DFTIME        FRTIME
     C                   Z-ADD     DFDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C                   Z-ADD     XXDATE        FRDT
     C                   MOVEL(P)  Q1USER        FRIUSR
     C                   MOVEL(P)  Q1PGM         FRIPGM
     C                   MOVEL     DFTIME        FRITIM
     C                   Z-ADD     XXDATE        FRIDT
     C*
     C                   WRITE     DBRCDF
     C                   FEOD      FDFRP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
**  QCMDEXC TABLE
OVRDBF FILE(FTABL) TOFILE(*LIBL/FTABL  )
