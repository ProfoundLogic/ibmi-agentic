     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8916R - WORK FILE BUILD - CLAIM TRANSFERS - WHCTP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - CLAIM TRANSFER FILE
     H*                                                               *
     H* CREATED: 03/24/01  C: 58                                      *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHCTP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.          *
     H*                                                               *
     H* 08/03/01 C:132 PROJECT WC924                                  *
     H*    Changed to Use input DATE PARM as the Last Extract Date
     H*    Pull records that are Greater than Last Extract Date       *
     H*                                                               *
     H*****************************************************************
     F*   Group Master
     FWMFNL     IF   E           K DISK
     F*
     F*   Claim Transfer
     FWACTL6    IF   E           K DISK
     F                                     RENAME(PCT01:PCT02)
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     F*  EXCLUDED EMPLOYERS FILE
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
     F**** End Add ******  07/22/05  **************************************
     F*
     F* Output - Claim Transfer file
     FWHCTP     O    E             DISK
     F*

     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
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
     D*    Policy number assembly
     D                 DS
     D  XXPOL#                 1     18
     D  C1FND                  1      3  0
     D  CTEMP#                 4     12  0
     D  CTFYR                 13     15  0
     D*
     D                 DS
     D  X2POL#                 1     18
     D  CXFND                  1      3  0
     D  CTXEMP                 4     12  0
     D  CTXFYR                13     15  0
     D*
     D*
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D*----------------------------------------------------------------
     D*        TEXT
     D*----------------------------------------------------------------
     D*
     D*    Type codes.
     D MOVTYP          C                   CONST('MOVECLAIM')
     D MERTYP          C                   CONST('MERGECLAIM')
     D DELTYP          C                   CONST('DELCLAIM')
     D*
     D*    Deletion indicator.
     D DELCOD          C                   CONST('*** DELETION ***')

     I*
     I*
     IPCT02
     I              CTCO#                       C1CO#
     I              CTFND                       C1FND
     I              CTCASE                      C1CASE
     I              CTDT                        C1DT
     I              CTTRDT                      C1TRDT

     C*
     C************************************************************
     C*  Begin
     C************************************************************
     C*  READ Group master for this company
     C     FNKEY         SETLL     PFN01
     C     FNKEY         READE     PFN01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*  Do not process Test Groups.
     C     FNTEST        IFNE      'Y'
     C*
     C*  READ Transfer file from first of month.
     C     CTKEYF        SETLL     PCT02
     C     CTKEY         READE     PCT02                                  61
     C*
     C*   Read all for group, through date.
     C     *IN61         DOWEQ     '0'
     C********   CTTRDT    ANDLEXXTODT
     C     C1TRDT        IFGT      XXTODT                                       Past Lst Extrct
     C                   Z-ADD     C1FND         CXFND
     C*
     C*   Write output record
     C                   EXSR      SROUT
     C*
     C                   ENDIF                                                  End GT XXTODT
     C*
     C*   Read next transfer record.
     C     CTKEY         READE     PCT02                                  61
     C                   ENDDO                                                   . 61
     C                   ENDIF                                                   . TEST<>Y
     C*
     C*   Read next group for company.
     C     FNKEY         READE     PFN01                                  60
     C                   ENDDO                                                   . 60
     C*
     C                   MOVE      '1'           *INLR
     C*================================================================
     C* SROUT - Write record to output file WHCTP
     C*================================================================
     C     SROUT         BEGSR
     C                   CLEAR                   PCT01
     C*
     C**** Begin Add ****  07/22/05  **************************************
     C*   Get From and To policy numbers
     C     CLTID         IFEQ      '135'
     C                   MOVEL     XXPOL#        WKPOL#
     C                   MOVEL     X2POL#        WKPOL2
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        WKPOL#                           New (TO)
     C                   ENDIF                                                  e 62 OFF...&
     C     RPKEY2        CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        WKPOL2                           Old (FROM)
     C                   ENDIF                                                  e 62 OFF...&
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
     C*
     C*   Deletion.
     C     CTREQ         IFEQ      XXDEL
     C                   MOVEL     DELTYP        CTTYPE
     C**** Begin Chg ****  07/22/05  **************************************
     C*******************  MOVELXXPOL#    CTOPOL
     C                   MOVEL     WKPOL#        CTOPOL
     C**** End Chg ******  07/22/05  **************************************
     C                   MOVEL     C1CASE        CTOCAS
     C                   ELSE
     C*
     C     C1CASE        IFNE      CTXCAS
     C     CTEMP#        ORNE      CTXEMP
     C     CTFYR         ORNE      CTXFYR
     C*
     C**** Begin Add ****  07/22/05  **************************************
     C*   Get From and To policy numbers
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        CTNPOL                           New (TO)
     C                   ENDIF                                                  e 62 OFF...&
     C     RPKEY2        CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        CTOPOL                           Old (FROM)
     C                   ENDIF                                                  e 62 OFF...&
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
     C*
     C*   Process by record ID.
     C                   SELECT
     C*
     C**** Begin CHG ****  07/22/05  **************************************
     C*   Merge / transfer.
     C     C1CASE        WHENNE    CTXCAS
     C                   MOVEL     MERTYP        CTTYPE
     C******************** MOVELXXPOL#    CTNPOL
     C                   MOVEL     WKPOL#        CTNPOL
     C                   MOVEL     C1CASE        CTNCAS
     C******************** MOVELX2POL#    CTOPOL
     C                   MOVEL     WKPOL2        CTOPOL
     C                   MOVEL     CTXCAS        CTOCAS
     C*
     C*   Move.
     C                   OTHER
     C                   MOVEL     MOVTYP        CTTYPE
     C******************** MOVELXXPOL#    CTNPOL
     C                   MOVEL     WKPOL#        CTNPOL
     C                   MOVEL     C1CASE        CTOCAS
     C******************** MOVELX2POL#    CTOPOL
     C                   MOVEL     WKPOL2        CTOPOL
     C                   MOVEL     CTXCAS        CTNCAS
     C*
     C******************** MOVELXXPOL#    CTNPOL
     C                   MOVEL     WKPOL#        CTNPOL
     C                   MOVEL     C1CASE        CTNCAS
     C******************** MOVELX2POL#    CTOPOL
     C                   MOVEL     WKPOL2        CTOPOL
     C                   MOVEL     CTXCAS        CTOCAS
     C                   ENDSL
     C                   ENDIF                                                  DLT <> " "
     C                   ENDIF                                                  CAS <> XCAS
     C**** END CHG ******  07/22/05  **************************************
     C*
     C     CTTYPE        IFNE      *BLANK
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
     C                   MOVE      D10X          CTDT
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *ON
      ****** END ADD ***** 08/11/04 ************************************
     C*
     C*     Write to output file
     C                   WRITE     PCT01
      *
      ****** BEG ADD ***** 08/11/04 ************************************
     C                   ENDIF                                                  END *IN65 *ON
      ****** END ADD ***** 08/11/04 ************************************
      *
     C                   ENDIF
     C*
     C*
     C                   ENDSR                                                  SROUT
     C*
     C*
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
     C                   PARM                    @@CO#             3
     C                   PARM                    @@DATE            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
     C                   PARM                    @@STYR            3 0
      ***** END ADD ***** 03/25/04 ************************************
     C*
     C*  KLIST definitions
     C*
     C     FNKEY         KLIST
     C                   KFLD                    KLCO#
     C*
     C     CTKEYF        KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
     C                   KFLD                    KLFRDT
     C*
     C     CTKEY         KLIST
     C                   KFLD                    FNCO#
     C                   KFLD                    FNFND
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    C1CO#
     C                   KFLD                    C1FND
     C                   KFLD                    CTEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C**** Begin Add ****  07/22/05  **************************************
     C* WMRPP Keys to get carrier record/policy numbers WMRPP
     C*   TO Key
     C     RPKEY         KLIST
     C                   KFLD                    C1CO#
     C                   KFLD                    C1FND
     C                   KFLD                    CTEMP#
     C                   KFLD                    CTDIV
     C                   KFLD                    CTFYR
     C*   FROM Key
     C     RPKEY2        KLIST
     C                   KFLD                    C1CO#
     C                   KFLD                    C1FND
     C                   KFLD                    CTXEMP
     C                   KFLD                    CTXDIV
     C                   KFLD                    CTXFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
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
     C     *LIKE         DEFINE    C1DT          XXDATE
     C     *LIKE         DEFINE    C1DT          XXTODT
     C     *LIKE         DEFINE    C1CO#         KLCO#
     C     *LIKE         DEFINE    C1DT          KLFRDT
     C*
     C**** Beg Add ******  07/22/05  **************************************
     C     *LIKE         DEFINE    CTNPOL        WKPOL#
     C     *LIKE         DEFINE    CTNPOL        WKPOL2
     C**** End Add ******  07/22/05  **************************************
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
     C* Set Starting Date for CTKEYF Equal to Last Extract Date
     C                   MOVE      @@CO#         KLCO#
     C                   Z-ADD     XXDATE        KLFRDT
     C**********           MOVE '01'      KLFRDT
     C*
     C                   Z-ADD     XXDATE        XXTODT
     C*
     C                   MOVEL     *BLANKS       XXDEL            30
     C                   MOVEL     DELCOD        XXDEL
     C*
     C                   ENDSR
     C*
