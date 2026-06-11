     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8912R - WORK FILE BUILD - AGENT FILE - WHAGP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE -  AGENT FILE
     H*                                                               *
     H* CREATED: 05/16/00  C: 58                                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/20/07 C:132 CI071 - 60081                                  *
     H*              - Re-Compile for AS OF DATE to show up in WHAGP. *
     H*                No Changes in Code for this fix.               *
     H*                                                               *
     H* 07/07/06 C:132 135WEW                                         *
     H*              - Remove Agency Short name added to WHAGP/AGSHNM *
     H*                which has now been removed from file.          *
     H*                                                               *
     H* 05/05/06 C:132 135WEW                                         *
     H*              - Add Agency Short name to WHAGP/AGSHNM, which   *
     H*                is a new field.                                *
     H*              - NOTE: New field is added before end of file,   *
     H*                      will need additional JAVA changes.       *
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/25/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 10/09/03 C:132 PROJECT:  CI010                                *
     H*                - USE PARM DATE FOR ASOF DATE                  *
     H*                                                               *
     H* 08/26/02 C:132 PROJECT WC924                                  *
     H*                Check to see if Agent is assigned to any Group *
     H*                with a valid Include Group Flag WMFNP/FNCICF   *
     H*                                                               *
     H* 05/30/02 C:132 PROJECT WC924                                  *
     H*                Add Agent Email Address & Agent Internet Addr  *
     H*                to WHAGP file & load email from WMAGP/AGEMAL   *
     H*                                                               *
     H* 08/07/01 C:132 PROJECT WC924
     H*              - Chg to output *BLANKS int AGCLT#
     H*                                                               *
     H* 07/24/01 C:132 PROJECT WC924
     H*              - Chg to use AAFEIN + AAMOD for AGCLT# and
     H*                AGAGT# in Data Structure & output in WHAGP     *
     H*                                                               *
     H* 05/09/01 C:132 PROJECT WC924
     H*              - Chg to Always write a Record in WHAGP file
     H*                                                               *
     H* 04/04/01 C:58  PROJECT WC924
     H*              - Use 'A' + Grp + Agt in CLT#
     H*              - Get mod from AAMOD if needed
     H*
     H* 03/21/01 C:61  PROJECT WC924                                  *
     H*              - Correct Grp/Agent#.                            *
     H*              - Correct Phone and FAX #                        *
     H*                                                               *
     H* 10/24/00  C:03 PROJECT: WC924 CORRECT LOAD OF PHN#'S AND FEIN
     H* 09/05/00  C:58 PROJECT: WC924 Add loc ID 3S 0 (Mod)
     H* 05/15/00  C:58 PROJECT: WC924 Add client ID 12A
     H*
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     F*
     F* Agent Master
     FWMAGL     IF   E           K DISK
     F                                     RENAME(PAG01:PAG02)
      ***** BEG ADD **** 08/26/02 ****************************************
     F*
     F* Agent Assignment by Group file
     FWMAAL     IF   E           K DISK
     F*
     F* Group Master File
     FWMFNL     IF   E           K DISK
      ***** END ADD **** 08/26/02 ****************************************
     F*
     F* Output - Policy agent file layout
     FWHAGP     O    E             DISK

     D*
     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(50)                              HEX CNVT ARRY
      ***** END ADD ***** 05/05/06 ***************************************
     D*
     D                 DS
     D* AGFEIN + AGMOD into CLTID for AGCLT# and AGAGT#
     D  XXCLT#                 1     12
     D  A1FEIN                 1      9  0
     D  A1MOD                 10     12  0
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
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      ***** END ADD ***** 02/27/04 ************************************

     I*
     IPAG02
      ***** BEG ADD **** 08/26/02 ****************************************
     I              AGCO#                       A1CO#
      ***** END ADD **** 08/26/02 ****************************************
     I              AGNAME                      A1NAME
     I              AGFEIN                      A1FEIN
     I              AGFAX#                      A1FAX#
     I              AGMOD                       A1MOD
      ***** BEG ADD **** 05/30/02 ****************************************
     I              AGEMAL                      A1EMAL
     I              AGIADR                      A1IADR
      ***** END ADD **** 05/30/02 ****************************************
     I*
      ***** BEG ADD ***** 05/05/06 ***************************************
     I              AGSHNM                      A1SHNM
     I*

     C*
     C************************************************************
     C*  Begin
     C************************************************************
     C*
     C*  READ AGENT
     C*
     C                   READ      PAG02                                  60
     C     *IN60         DOWEQ     '0'
     C                   EXSR      SROUT
     C*
     C                   READ      PAG02                                  60
     C                   ENDDO
     C*
     C                   MOVE      '1'           *INLR
      ***** BEG ADD ******* 03/21/01 **********************************
      *
      *
      *
      *================================================================
      * SRPHN - Format phone number for output
      *================================================================
     C     SRPHN         BEGSR
      *
     C     XXAREA        IFNE      '000'
      *
     C                   MOVEL(P)  '('           XXPHON           20
     C                   CAT(P)    XXAREA:0      XXPHON
     C                   CAT(P)    ')':0         XXPHON
     C                   CAT(P)    XXPH1:1       XXPHON
     C                   CAT(P)    '-':0         XXPHON
     C                   CAT(P)    XXPH2:0       XXPHON
      *
     C                   ELSE
      *
     C                   MOVEL(P)  *BLANKS       XXPHON
     C                   ENDIF
      *
     C                   ENDSR
     C*
     C*================================================================
     C* SROUT - Write record to output file WHAGP
     C*================================================================
     C     SROUT         BEGSR
     C*
     C                   CLEAR                   PAG01
     C*
      ***** BEG ADD **** 08/26/02 ****************************************
      *
      *  Read the Agent Assignment file for Group Number, then chain to
      *  Group Master (WMFNL) to see if the WMFNL/WMCICF flag is set to
      *  include the group in the EXTRACT files.
      *
      *  KEY LIST FOR WMAAL
     C     AAKY          KLIST
     C                   KFLD                    A1CO#
     C                   KFLD                    A1FEIN
     C                   KFLD                    A1MOD
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
     C                   MOVE      A1MOD         WKAGT#           12 0
     C                   MOVEL     A1FEIN        WKAGT#
     C*
     C* Use XXCLT# from Data Structure of AAFEIN + AAMOD
      ***** BEG CHG ******* 08/07/01 **********************************
     C***********          MOVELXXCLT#    AGCLT#    P
     C                   MOVE      *BLANKS       AGCLT#
      ***** END CHG ******* 08/07/01 **********************************
     C                   MOVEL(P)  XXCLT#        AGAGT#
     C*
     C     *IN60         IFEQ      *OFF
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1NAME        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1NAME
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     AGMAD1        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            AGMAD1
      *
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     AGMCTY        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            AGMCTY
      *
     C     A1EMAL        IFNE      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1EMAL        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1EMAL
     C                   ENDIF
      *
     C     A1IADR        IFNE      *BLANKS
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     A1IADR        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            A1IADR
     C                   ENDIF
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   MOVEL(P)  A1NAME        AGNAME
     C                   MOVEL(P)  A1FEIN        AGFEIN
     C                   MOVEL(P)  AGMAD1        AGAD1
     C                   MOVEL(P)  AGMCTY        AGCTY
     C                   MOVEL(P)  AGMST         AGST
     C                   MOVEL(P)  A1FAX#        AGFAX#
     C                   MOVE(P)   AGFAXA        AGFAX#
      *
      ***** BEG ADD **** 05/30/02 ****************************************
      * AGIADR is available, so use it ?
     C                   MOVEL(P)  A1IADR        AGIADR
     C                   MOVEL(P)  A1EMAL        AGEMAL
      ***** END ADD **** 05/30/02 ****************************************
      *
      ***** BEG ADD ***** 05/05/06 ***************************************
      ***** BEG DEL ***** 07/07/06 ***************************************
      ********* If Client 135, load agent short name in AGNAME, and always
      *********  load agent short name in new field, AGSHNM.
     C*********            MOVELA1SHNM    AGSHNM    P
      ***** END DEL ***** 07/07/06 ***************************************
      *  If Client 135, load agent short name in AGNAME
      *
     C     CLTID         IFEQ      '135'
     C     A1SHNM        ANDNE     *BLANKS
     C                   MOVEL(P)  A1SHNM        AGNAME
     C                   ENDIF
      ***** END ADD ***** 05/05/06 ***************************************
      *
     C                   MOVE      AGARE1        XXAREA            3
     C                   MOVEL     AGPHN1        XXPH1             3
     C                   MOVE      AGPHN1        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        AGPHN#
      *
     C                   MOVE      AGFAXA        XXAREA            3
     C                   MOVEL     A1FAX#        XXPH1             3
     C                   MOVE      A1FAX#        XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        AGFAX#
     C*
     C                   MOVEL     AGMZIP        XXZIP5            5
     C                   MOVE      AGMZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         AGZIP
     C                   CAT(P)    XXZIP4:0      AGZIP
     C*
     C*  If didn't have a Agent Master Record, write with Unknown Name
     C                   ELSE
     C                   MOVEL(P)  'UNKNOWN'     AGNAME
     C*
     C                   ENDIF
     C*
     C*      ID
     C                   MOVE      'A'           AGA               1            Add-Define It
     C                   MOVEL     WKAGT#        DSFEIN            9            Add-Define It
     C                   MOVE      A1MOD         AGMOD
     C     AGMOD         IFEQ      '000'                                             +
     C                   MOVE      A1MOD         AGMOD                               +
     C                   ENDIF                                                       +
     C*
     C*      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
      ***** BEG CHG **** 10/09/03 ********************************
      * Change the Parm Date into the AS OF Date for output record
     C                   MOVEL     @@DATE        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
     C*************        Z-ADDDSDATE    SRDTI
      ***** END CHG **** 10/09/03 ********************************
     C                   CALL      'SRCVTDAT'    DTMDYY
     C     SRERR         IFEQ      '0'
     C                   MOVEL     SRDTO         D8N
     C                   EXSR      EDDAT
     C                   ENDIF
     C                   MOVE      D10X          AGAODT
     C*
     C*
     C*  Write to output file
     C*
     C                   WRITE     PAG01
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
     C************************************************************
     C*
     C*  PLIST definitions
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@DATE            8
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
     C                   PARM      '*MDY'        SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     *LIKE         DEFINE    AGDT          XXDATE
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
      ***** BEG ADD ***** 05/05/06 ***************************************
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ***** 05/05/06 ***************************************
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
