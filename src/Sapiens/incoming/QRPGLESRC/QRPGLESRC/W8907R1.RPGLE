     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8907R1 - WORK FILE BUILD - WHDPP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - DIVISION DEPARTMENTS FILE - WHDPP          *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 09/09/05 C: 132   CI099 - 48925                               *
     H*             Change to create correct Policy number field      *
     H*                                                               *
     H* 08/12/05 C: 132 CI093 - 47984                                 *
     H*             Retrieve the Client ID and if is KRMS (093) then  *
     H*             bypass the check on WDEHP and always write record *
     H*                                                               *
     H*  07/22/05       C: 58      Project: 135WEH
     H*        For extracted policy#, use WMRPP/carrier number:
     H*          for: client 135;
     H*                                                               *
     H* 11/15/04 C: 132 CI021                                         *
     H*             Add check on WDEHP file when a policy start date  *
     H*             in WDELL is in the future. If no WDEHP record     *
     H*             for future start dates, do not write the record.  *
     H*                                                               *
     H* 08/11/04 C: 132 91DLK                                        *
     H*             Add Exclude Employers file WHXPP, Check file when *
     H*             Write WHDPP record, if exists in WHXPP file then  *
     H*             do not write the record.                          *
     H*                                                               *
     H* 03/25/04 C: 132 CI010   and CI015                             *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 03/10/04 C: 58  CI015
     H*             Add start fund year PARM; extract only therefrom.
     H*                                                               *
     H* 02/27/04 C:132 SUPPORT CI099 40909     (ADDED 03/12/04)       *
     H*                Check for certain hex characters in output     *
     H*                alpha fields and replaced if exists. Fields    *
     H*                from tables and static files are not checked   *
     H*                                                               *
     H* 04/24/02  C:132 WCI091 SEQ: 32217                             *
     H*               - Change: Chg Extact date to be > Maint date    *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*                                                               *
     H* 07/31/00  C:132 PROJECT WC924
     H*                 Add Parm for Last Extract Date
     H*                                                               *
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: DPAODT - Data AS OF date.
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
      *
     F* Department Code
     FWMDPL     IF   E           K DISK
      *
     F* Employer Detail
     FWDELL     IF   E           K DISK
     F*
      ****** BEG ADD ***** 08/11/04 ************************************
     FWHXPL     IF   E           K DISK
      ****** END ADD ***** 08/11/04 ************************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * Employer Detail History
     FWDEHL1    IF   E           K DISK
      ****** END ADD ***** 11/15/04 ************************************
      *
     F* Output - Division Locations
     FWHDPP     O    E             DISK
     F                                     RENAME(PDP01:PDP02)
     F*
     F* Group Master
     FWMFNL     IF   E           K DISK
     F*
     F**** Begin Add ****  07/22/05  **************************************
     F*   Employer Carrier File
     FWMRPL     IF   E           K DISK
     F**** End Add ******  07/22/05  **************************************

     D*
     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
      *
      ***** BEG ADD ***** 02/27/04 ************************************
     D HX              S              1    DIM(50)                              HEX CNVT ARRY
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
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
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
      ***** END ADD ***** 02/27/04 ************************************
      *

     IPDP01
     I              DPCO#                       D1CO#
     I              DPFND                       D1FND
     I              DPEMP#                      D1EMP#
     I              DPDIV                       D1DIV
     I              DPDEPT                      D1DEPT
     I              DPDESC                      D1DESC
     I              DPDT                        D1DT
     I*

     C*
      *===========================================================
      *  PLIST Definitions
      *===========================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@EXDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
      ***** END ADD ***** 03/25/04 ************************************
     C***** BEG ADD ******* 03/10/04 **********************************
     C                   PARM                    @@STYR            3 0
     C***** END ADD ******* 03/10/04 **********************************
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*MDY'        SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *===========================================================
      *  KLIST
      *===========================================================
      *
      *  Department
     C     D1KY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C*
     C*  Fund master
     C     FNKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *
      *  Employer Excluded file
      *
     C     XPKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
      ****** END ADD ***** 08/11/04 ************************************
     C*
      ****** BEG ADD ***** 11/15/04 ************************************
      *
     C     EHKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C**** Begin Add ****  07/22/05  **************************************
     C     RPKEY         KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELFND
     C                   KFLD                    ELEMP#
     C                   KFLD                    ELDIV
     C                   KFLD                    ELFYR
     C*
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
     C**** End Add ******  07/22/05  **************************************
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL     @@JBDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
     C*
     C     SRERR         IFEQ      *OFF
     C*
     C                   MOVEL     @@JBDT        SRDTI
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
     C     *LIKE         DEFINE    ELPFDT        XXDATE
     C*
      ****** END ADD ***** 11/15/04 ************************************
     C*
     C     *LIKE         DEFINE    ELCO#         SVCO#
     C     *LIKE         DEFINE    ELFND         SVFND
     C     *LIKE         DEFINE    ELPFDT        XXEXDT
      *
      *  INITIALIZE Extract Date for Comparison
      *
     C                   TIME                    DSDTTM
      *
     C                   MOVEL     @@EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DATCHK
      *
     C     SRERR         IFEQ      *OFF
     C                   MOVEL     @@EXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
     C                   ELSE
      *
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         XXEXDT
      *
     C                   ENDIF                                                  SRERR
      *
      ************************************************************
      *  Begin
      ************************************************************
      *
      *  READ Employer policy year file
      *
     C                   READ      PEL01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C*   Get group master.
     C     ELCO#         IFNE      SVCO#
     C     ELFND         ORNE      SVFND
     C*
     C     FNKY          CHAIN     PFN01                              63
     C     *IN63         IFEQ      *ON
     C                   MOVEL     'Y'           FNTEST
     C                   ENDIF                                                  .63 = ON
     C                   Z-ADD     ELCO#         SVCO#
     C                   Z-ADD     ELFND         SVFND
     C                   ENDIF                                                  .CO/FND <>
     C*
     C     FNTEST        IFEQ      'Y'
     C     FNCICF        ORNE      'Y'
     C*
     C                   GOTO      NEXT
     C                   ELSE
      *
      ****** BEG ADD ***** 08/11/04 ************************************
      *  If GET hit on excluded Employer, then bypass the write of record
     C     XPKY          CHAIN     PXP01                              65
     C     *IN65         IFEQ      *OFF
     C                   GOTO      NEXT
     C                   ENDIF
      ****** END ADD ***** 08/11/04 ************************************
      *
     C***** BEG ADD ******* 03/10/04 **********************************
     C     ELFYR         IFLT      @@STYR
     C                   GOTO      NEXT
     C                   ENDIF                                                   FYR>=@@STYR
     C***** END ADD ******* 03/10/04 **********************************
      *
      ****** BEG ADD ***** 11/15/04 ************************************
      * If Policy start date > run date, check Detail History to see if
      * has been billed (WDEHP), bypass if no recordin WDEHP.
     C     ELPFDT        IFGT      XXDATE
     C     EHKY          CHAIN     PEH01                              60
     C     *IN60         IFEQ      *ON
      ****** BEG ADD ***** 08/12/05 ************************************
     C     CLTID         ANDNE     '093'
      ****** END ADD ***** 08/12/05 ************************************
     C                   GOTO      NEXT
     C                   ENDIF                                                  *IN60 = *ON
     C                   ENDIF                                                   ELPFDT<>XXDATE
      ****** END ADD ***** 11/15/04 ************************************
      *
      *  Department file
      *
     C     D1KY          CHAIN     PDP01                              60
     C     *IN60         DOWEQ     '0'
      *
     C     D1CO#         IFEQ      *ZERO
     C                   GOTO      NEXT
     C                   ENDIF                                                  D1CO#
      *
      *   Check Last Extract Date against WMDPL Maintenance Date
      *
      **** BEG CHG ***** 04/24/02 **********************************************
     C******     XXEXDT    IFGT D1DT
     C     XXEXDT        IFGE      D1DT
      **** END CHG ***** 04/24/02 **********************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C     D1KY          READE     PDP01                                  60
     C                   ENDDO
      *
     C                   ENDIF                                                  ELCO#
      *
     C                   READ      PEL01                                  60
     C                   ENDDO
      *
     C                   MOVE      '1'           *INLR
      *
      *================================================================
      * SROUT - Write record to output file WHDPP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PDP02
      *
      *  Policy Number
     C**** Begin Add ****  07/22/05  **************************************
     C     CLTID         IFEQ      '135'
     C     RPKEY         CHAIN     PRP01                              62
     C     *IN62         IFEQ      *OFF
     C     RPCAP#        ANDNE     *BLANK
     C                   MOVEL(P)  RPCAP#        DPPONR
     C                   ELSE
     C**** End Add ******  07/22/05  **************************************
      *
     C                   MOVEL(P)  D1FND         DPPONR
     C                   MOVEL(P)  D1EMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       DPPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       DPPONR
     C**** Begin Add ****  07/22/05  **************************************
     C                   ENDIF                                                  e 62 OFF...&
      ***** BEG ADD ***** 09/09/05 ***************************************
     C                   ELSE
     C                   MOVEL(P)  D1FND         DPPONR
     C                   MOVEL(P)  D1EMP#        XXTMP             9
     C                   CAT(P)    XXTMP:0       DPPONR
     C                   MOVEL(P)  ELFYR         XXTMP             9
     C                   CAT(P)    XXTMP:0       DPPONR
      ***** END ADD ***** 09/09/05 ***************************************
     C                   ENDIF                                                  e CLTID=135
     C**** End Add ******  07/22/05  **************************************
      *
      ***** BEG ADD ***** 02/27/04 ************************************
      * Clear array then Check for Invalid Hex characters
     C                   MOVEA     *BLANKS       HX
     C                   MOVEA     D1DESC        HX
     C                   EXSR      SRHEX
     C                   MOVEA     HX            D1DESC
      ***** END ADD ***** 02/27/04 ************************************
      *
     C                   Z-ADD     D1DIV         DPDIV
     C                   MOVEL(P)  D1DEPT        DPDEPT
     C                   MOVEL(P)  D1DESC        DPDEPD
     C                   Z-ADD     D1CO#         DPCO#
      *
      *      As of date
     C                   MOVEL     *BLANKS       D10X
      *
      ****** BEG DEL ***** 11/15/04 ************************************
     C*************        TIME           DSDTTM
      ****** END DEL ***** 11/15/04 ************************************
      *
      ***** BEG CHG ***** 03/25/04 ************************************
     C*************        Z-ADDDSDATE    SRDTI
     C                   MOVEL     @@JBDT        ZZRNDT            8 0
     C                   Z-ADD     ZZRNDT        SRDTI
      ***** END CHG ***** 03/25/04 ************************************
     C                   CALL      'SRCVTDAT'    DTMDYY
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
     C                   MOVE      D10X          DPAODT
      *
      *  Write to output file
      *
     C                   WRITE     PDP02
     C                   ENDSR                                                  SROUT
      *
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
