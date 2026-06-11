     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8913R1 - WORK FILE BUILD - AGENT PRODUCERS - WHAPP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - AGENT PRODUCERS FILE
     H*                                                               *
     H* CREATED: 05/16/00  C: 58                                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 12/07/12 C:284 SUPPORT 81115
     H*                Inactive producer's should not be written in   *
     H*                WHAPP file.
     H*
     H* 03/25/04 C: 132 CI010                                         *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.          *
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/25/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 08/26/02 C:132 PROJECT WC924                                  *
     H*                Check to see if Producer assigned to any Group *
     H*                with a valid Include Group Flag WMFNP/FNCICF   *
     H*                                                               *
     H* 05/30/02 C:132 PROJECT WC924                                  *
     H*                Add Agent Email Address & Agent Internet Addr  *
     H*                                                               *
     H* 08/07/01 C:132 PROJECT WC924                                  *
     H*    Changed to Load *BLANKS in APUSR, APPWD Fields in Output
     H*    File WHAPP.                                                *
     H*                                                               *
     H* 08/01/01 C:132 PROJECT WC924                                  *
     H*    Changed to Use input DATE PARM as the Last Extract Date
     H*    Will only include records from WMAPP that have a Maint
     H*    Date (APDT) Greater than the Last Extract Date             *
     H*                                                               *
     H* 07/25/01 C:003 PROJECT WC924                                  *
     H*    CHANGED TO NOT INCLUDE OPEN TO WMAAP, WRITE A RECORD FOR
     H*    WMAPP RECORD NOT EVERY WMAAP RECORD
     H*                                                               *
     H* 07/24/01 C:132 PROJECT WC924                                  *
     H*                Chg 'AgentNo' to be 'Agent FEIN' + 'Agent Mod' *
     H*                                                               *
     H* 03/21/01 C:61  PROJECT WC924                                  *
     H*                Chg 'User ID' to be 'Producer ID' + 'Loc Id'   *
     H*                                                               *
     H* 02/06/01 C:61  PROJECT WC924                                  *
     H*              - Remove FEIN.                                   *
     H*              - Load Agent Number w/ GRP/AGT                   *
     H*                                                               *
     H* 09/05/00  C:58 PROJECT: WC924 Add prodcr loc ID 3S 0 (Mod)
     H* 08/22/00  C:58 PROJECT: WC924 Add user ID           12A
     H*                               Password              10A
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     F*
     F* Agent Producer
     FWMAPL     IF   E           K DISK
     F                                     RENAME(PAP01:PAP02)
      ***** BEG ADD **** 08/26/02 ****************************************
     F*
     F* Agent Assignment by Group file
     FWMAAL     IF   E           K DISK
     F*
     F* Group Master File
     FWMFNL     IF   E           K DISK
      ***** END ADD **** 08/26/02 ****************************************
     F*
     F* Output - Agent Producer file
     FWHAPP     O    E             DISK
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(50)                              HEX CNVT ARRY
      ***** END ADD ***** 05/30/02 ***************************************
     D*
     D*    8 and 10 digit dates, numeric and character
     D                 DS
     D  DN                     1      8
     D                                     DIM(8)                               8 digit num
     D  D8X                    1      8
     D  D8N                    1      8  0
     D                 DS
     D  DX                     1     10
     D                                     DIM(10)                              10 dig edited
     D  D10X                   1     10
     D  D10N                   1     10  0
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** END ADD ***** 02/27/04 ************************************

     I*
     IPAP02
     I              APCO#                       A1CO#
     I              APFEIN                      A1FEIN
     I              APMOD                       A1MOD
     I              APPROD                      A1PROD
     I              APLSN                       A1LSN
     I              APFSN                       A1FSN
     I              APDOB                       A1DOB
     I              APSS#                       A1SS#
      ***** BEG ADD ***** 05/30/02 ***************************************
     I              APEMAL                      A1EMAL
     I*
      ***** BEG ADD ***** 12/07/12 ***************************************
     I              APSTAT                      A1STAT
      ***** END ADD ***** 12/07/12 ***************************************

     C*
      *  Agent Assignment
      *
     C     AAKY          KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1FEIN
     C                   KFLD                    A1MOD
     C*
     C************************************************************
     C*  Begin
     C************************************************************
     C*
     C*  READ Producer file
     C*
     C                   READ      PAP02                                  60
      *
     C     *IN60         DOWEQ     '0'
      ***** BEG ADD ******* 12/07/12 **********************************
     C     A1STAT        ANDEQ     'A'
      ***** END ADD ******* 12/07/12 **********************************
      *
      *    Check to see if WMAPP Maint Date (APDT) is Greater
      *    than the Last Extract Date (XXDATE), if so Write Record
     C     APDT          IFGT      XXDATE
     C*
     C*   Write output record
     C                   EXSR      SROUT
     C*
     C                   ENDIF
     C*
     C                   READ      PAP02                                  60
     C                   ENDDO
     C*
     C*
     C                   MOVE      '1'           *INLR
     C*
     C*
     C*
     C*================================================================
     C* SROUT - Write record to output file WHAPP
     C*================================================================
     C     SROUT         BEGSR
     C*
     C                   CLEAR                   PPA01
     C*
      ***** BEG ADD **** 08/26/02 ****************************************
      *
      *  Read the Agent Assignment file for Group Number, then chain to
      *  Group Master (WMFNL) to see if the WMFNL/WMCICF flag is set to
      *  include the group in the EXTRACT files.
      *
      *  KEY LIST FOR WMFNL
     C     FNKY          KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    AAFND
      *
     C                   MOVE      ' '           GRPFLG            1
     C     AAKY          SETLL     PAA01
     C     AAKY          READE     PAA01                                  61
     C     *IN61         DOWEQ     *OFF
     C     FNKY          CHAIN     PFN01                              62
     C     *IN62         IFEQ      *OFF
     C     FNCICF        ANDEQ     'Y'
     C                   MOVE      'Y'           GRPFLG
     C                   ENDIF
     C     AAKY          READE     PAA01                                  61
     C                   ENDDO
      *
      * If the group flag is 'Y', Inlude the agent, else bypass
      *
     C     GRPFLG        IFNE      'Y'
     C                   GOTO      OUTEND
     C                   ENDIF
      *
      ***** END ADD **** 08/26/02 ****************************************
     C*
     C*     Producer ID fields
     C                   MOVE      A1MOD         APMOD
     C                   MOVEL     A1PROD        APPROD
     C*
     C*     Producer name
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1LSN         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1LSN
      *
     C     A1FSN         IFNE      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1FSN         HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1FSN
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL     A1LSN         APLSN
     C                   MOVEL     A1FSN         APFSN
     C*
     C*     Date of birth
     C                   Z-ADD     A1DOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
     C                   MOVE      D10X          APDOB
     C                   ENDIF
     C*
     C*     Producer SS#
     C                   MOVEL     A1SS#         APSS#
     C*
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1EMAL        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1EMAL
      ***** END ADD ***** 02/27/04 ************************************
     C*
      ***** BEG ADD ***** 05/30/02 ***************************************
     C*     Producer Email
     C                   MOVEL(P)  A1EMAL        APEMAL
      ***** END ADD ***** 05/30/02 ***************************************
     C*
     C*     Address
      *
      ***** BEG ADD ***** 02/27/04 ***************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     APMAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            APMAD1
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     APMCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            APMCTY
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  APMAD1        APMAD
     C                   MOVEL(P)  APMCTY        APCTY
     C                   MOVEL(P)  APMST         APST
     C*
     C                   MOVEL     APMZIP        XXZIP5            5
     C                   MOVE      APMZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         APZIP
     C                   CAT(P)    XXZIP4:0      APZIP
     C*
     C                   MOVEL(P)  APPROD        APUSR
     C                   CAT(P)    APMOD:0       APUSR
      ***** BEG CHG ***** 08/07/01 ************************************
      * Change to Load Blanks into APUSR & APPWD
     C**********           MOVEL'PASSWORD'APPWD
     C                   MOVE      *BLANKS       APUSR
     C                   MOVE      *BLANKS       APPWD
      ***** END CHG ***** 08/07/01 ************************************
     C*
     C*      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C*************        Z-ADDDSDATE    SRDTI
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDY2
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   ELSE
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
      ***** END ADD ***** 03/25/04 ************************************
     C                   ENDIF
     C                   MOVE      D10X          APAODT
      *
      *  Agent Number
      *
     C                   MOVEL(P)  A1FEIN        APAGT#                         ADD 7/25/01
     C                   MOVEL(P)  A1MOD         XXMOD             3            ADD 7/25/01
     C                   CAT(P)    XXMOD:0       APAGT#
     C*
     C*  Write to output file
     C                   WRITE     PPA01
      *
      ***** BEG ADD **** 08/26/02 ****************************************
     C     OUTEND        TAG
      ***** END ADD **** 08/26/02 ****************************************
      *
     C                   ENDSR                                                  SROUT
     C*
     C*================================================================
     C*      EDDAT      EDIT DATE  8 digit to MM/DD/YYYY
     C*================================================================
     C     EDDAT         BEGSR
     C*
     C                   Z-ADD     1             W                 2 0
     C                   DO        10            X                 2 0
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C                   MOVEL     '/'           DX(X)
     C                   ELSE
     C                   MOVEL     DN(W)         DX(X)
     C                   ADD       1             W
     C                   ENDIF                                                  .X=3/6
     C*
     C                   ENDDO                                                  .DO 10
     C*
     C                   ENDSR
     C*
     C*================================================================
     C*      *INZSR  Initialization
     C*================================================================
     C     *INZSR        BEGSR
     C*
     C*  PLIST definitions
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@DATE            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
     C                   PARM                    @@STYR            3 0
      ***** END ADD ***** 03/25/04 ************************************
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
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDYY'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*
     C************************************************************
     C*
     C*   *LIKE defines
     C*
     C     *LIKE         DEFINE    APDT          XXDATE
     C     *LIKE         DEFINE    APDT          XXRNDT
     C     *LIKE         DEFINE    APDT          XXDOB
     C*
     C*
     C*  INITIALIZE
     C*
     C                   TIME                    DSDTTM
     C*
     C                   MOVEL     @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     @@DATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ELSE
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXDATE
     C*
     C                   ENDIF                                                  SRERR
     C*
     C                   ENDSR
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      *================================================================
      * SRHEX - Check Alpha input field for Invald Hex characters
      *         The Alpha field is loaded into the arrary w/MOVEA
      *         then Loop thru array & replace w/blank if out of
      *         is not a valid HEX Character. After exiting subr
      *         the array is moved back to the alpha field w/MOVEA
      *================================================================
     C     SRHEX         BEGSR
      *
     C                   DO        50            K                 3 0
      *
     C     HX(K)         IFLT      X'40'
     C     HX(K)         OREQ      X'41'
     C     HX(K)         ORGT      X'F9'
     C                   MOVE      X'40'         HX(K)
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ENDSR                                                  SROUT
      *================================================================
      ***** END ADD ***** 02/27/04 ************************************
