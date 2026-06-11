     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W8910R1 - WORK FILE BUILD - WHFNP
     H*****************************************************************
     H*                                                               *
     H* DESCRIPTION:                                                  *
     H*                                                               *
     H*   DATA WAREHOUSE - OFFICE FILE - WHFNP                        *
     H*                                                               *
     H* CREATED: 04/04/00  C:61                                       *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 04/30/10  C:189 PROJECT: 139DMP                               *
     H*                - Add NCCI  Carrier ID for CICENTER            *
     H*                                                               *
     H* 07/20/07 C: 132   135DJB                                      *
     H*           - Add WMREP,WMEXP to code for group region#, desc.  *
     H*           - Add Region# & Description fields to WHPRP before  *
     H*             the "As Of Date" field at end of file.            *
     H*                                                               *
     H* 01/23/06 C:132 PROJECT:  135DWEI                              *
     H*                If WMFMP/FNPEOF is not blank for Group, then   *
     H*                use Group Short Name instead of Group Name.    *
     H*                Added Call to get CltID, but did not use for   *
     H*                this project.                                  *
     H*                                                               *
     H* 02/10/05 C:132 PROJECT:  126DDH                               *
     H*                Add Group Number to the end of the Group Name  *
     H*                and Group Short Name                           *
     H*                                                               *
     H* 03/25/04 C: 132 CI010                                         *
     H*           - Add start Date PARM for Job Date, so each file    *
     H*             has same ASOF DATE, for next incremetal.  CI010   *
     H*                                                               *
     H* 05/30/02  C:132 PROJECT WC924                                 *
     H*               - Change: Concat F1CO# * F1FND to FNFND as      *
     H*                 Carier ID in output WHFND/FNFND & pad w/zeros *
     H*                                                               *
     H* 04/24/02  C:132 WCI091 SEQ: 32217                             *
     H*               - Change: Chg Extact date to be > Maint date    *
     H*                                                               *
     H* 10/02/01  C:132 PROJECT WC924                                 *
     H*               - Change: Add a check new FNCICF flag in WMFNP  *
     H*                 for a 'Y' to include in Extract = Else Bypass *
     H*
     H* 08/07/01  C:132 PROJECT WC924
     H*                 ADD Phone & FAX Numbers to Output File
     H*
     H* 07/31/01  C:123 PROJECT WC924
     H*                 Add Last Extract Date Input Parm
     H*
     H* 05/04/01  C:03 PROJECT WC924
     H*                CHANGE ALIGNMENT OF FNFND FIELD
     H*
     H* 06/10/00  C:58 PROJECT WC993
     H*                 Select only non-test groups.
     H*
     H* 05/15/00  C:58 PROJECT WC924
     H*                 ADD field: FNAODT - Data AS OF date.
     H*                                                               *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     F*
     F* Group
     FWMFNL     IF   E           K DISK
     F*
     F* Output - Carrier
     FWHFNP     O    E             DISK
     F                                     RENAME(PFN01:PFN02)
     F*
      ***** BEG ADD ***** 07/20/07 ***********************************
     FWMREL     IF   E           K DISK
     F*
     FWMEXL     IF   E           K DISK
      ***** END ADD ***** 07/20/07 ***********************************
     F*** BEGIN ADD *** 04/30/10 **************************************
     FWDF2L     IF   E           K DISK
     F*** END   ADD *** 04/30/10 **************************************

     D*
     D*----------------------------------------------------------------
     D*      ARRAYS
     D*----------------------------------------------------------------
     D*
     D*     o Date field arrays
      ***** END ADD ***** 01/23/06 ***********************************
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
      **** BEGIN ADD *** 05/30/02 ************************************
     D                 DS
     D  ##CRID                 1      6
     D  F1CO#                  1      3  0
     D  F1FND                  4      6  0
      **** END ADD ***** 05/30/02 ************************************
     D*
     D* System Date and Time
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     I*
     I*
     IPFN01
     I              FNFND                       F1FND
     I              FNNAME                      F1NAME
     I              FNSHNM                      F1SHNM
     I              FNMAD1                      F1MAD1
     I              FNMAD2                      F1MAD2
     I              FNMCTY                      F1MCTY
     I              FNMST                       F1MST
     I              FNMZIP                      F1MZIP
     I              FNPAD1                      F1PAD1
     I              FNPAD2                      F1PAD2
     I              FNPCTY                      F1PCTY
     I              FNPST                       F1PST
     I              FNPZIP                      F1PZIP
     I              FNCO#                       F1CO#
     I              FNDT                        F1DT
     I              FNPH1A                      F1PH1A
     I              FNPH1                       F1PH1
     I              FNAREA                      F1AREA
     I              FNPH2                       F1PH2
     I              FNFAXA                      F1FAXA
     I              FNFAX                       F1FAX
      ***** BEG ADD ***** 01/23/06 ***********************************
     I              FNPEOF                      F1PEOF
      *
      ************************************************************
      *  KLIST
      ************************************************************
      *
      ***** BEG ADD ***** 07/20/07 ***********************************

     C* Group Master File
     C     FNKY          KLIST
     C                   KFLD                    F1CO#
     C                   KFLD                    F1FND
     C* Admin Darts Control File
     C     EXKY          KLIST
     C                   KFLD                    F1CO#
     C                   KFLD                    F1FND
     C* Region Master File
     C     REKY          KLIST
     C                   KFLD                    EXCO#
     C                   KFLD                    EXFND
     C                   KFLD                    EXDRGN
      ***** END ADD ***** 07/20/07 ***********************************
     C*
      *===========================================================
      *  PLIST Definitions
      *===========================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @@EXDT            8
      ***** BEG ADD ***** 03/25/04 ************************************
     C                   PARM                    @@JBDT            8
     C                   PARM                    @@STYR            3 0
      ***** END ADD ***** 03/25/04 ************************************
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
     C*
     C     *LIKE         DEFINE    F1DT          XXEXDT
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
      *===========================================================
      *  Begin
      *===========================================================
      *
      ***** BEG ADD ***** 01/23/06 ***********************************
     C* Retrieve Client ID number.
     C                   CALL      'RTVCLTID'
     C                   PARM                    CLTID             3
      ***** END ADD ***** 01/23/06 ***********************************
      *
      *  READ Group file
      *
     C                   READ      PFN01                                  60
     C     *IN60         DOWEQ     '0'
      *
     C     F1CO#         IFEQ      *ZERO
     C     FNTEST        OREQ      'Y'
     C     FNCICF        ORNE      'Y'
     C                   GOTO      NEXT
     C                   ENDIF                                                  ALCO# EQ ZERO
      *
      *   Check Last Extract Date against WMOFL Maintenance Date
      *
      **** BEG CHG ***** 04/24/02 **********************************************
     C*******    XXEXDT    IFGT F1DT
     C     XXEXDT        IFGE      F1DT
      **** END CHG ***** 04/24/02 **********************************************
     C                   GOTO      NEXT
     C                   ENDIF
      *
      *   Write output record
      *
     C                   EXSR      SROUT
      *
     C     NEXT          TAG
     C                   READ      PFN01                                  60
     C                   ENDDO
     C                   MOVE      '1'           *INLR
      *
      *================================================================
      * SROUT - Write record to output file WHFNP
      *================================================================
     C     SROUT         BEGSR
      *
     C                   CLEAR                   PFN02
      *
      **** BEGIN CHG *** 05/30/02 ************************************
     C********             MOVELF1FND     FNFND     P
     C                   MOVEL(P)  ##CRID        FNFND
      **** END CHG ***** 05/30/02 ************************************
     C                   MOVEL(P)  F1NAME        FNNAME
      *
      ***** BEG ADD ***** 01/23/06 ***********************************
      * If PEO Group, then use Short Group Name instead of Group Name
     C     F1PEOF        IFNE      ' '
     C                   MOVEL(P)  F1SHNM        FNNAME
     C                   ENDIF
      ***** END ADD ***** 01/23/06 ***********************************
      *
      ***** BEG ADD ***** 02/10/05 ***********************************
     C                   MOVE      F1FND         XX1FND            3
     C                   MOVEL(P)  '('           XXAFND            5
     C     XXAFND        CAT       XX1FND:0      XXAFND
     C     XXAFND        CAT       ')':0         XXAFND
     C     FNNAME        CAT       XXAFND:1      FNNAME
      ***** END ADD ***** 02/10/05 ***********************************
      *
     C                   MOVEL(P)  F1SHNM        FNSHNM
      *
      ***** BEG ADD ***** 02/10/05 ***********************************
     C     FNSHNM        CAT       XXAFND:1      FNSHNM
      ***** END ADD ***** 02/10/05 ***********************************
      *
     C                   MOVEL(P)  F1MAD1        FNMAD1
     C                   MOVEL(P)  F1MAD2        FNMAD2
     C                   MOVEL(P)  F1MCTY        FNMCTY
     C                   MOVEL(P)  F1MST         FNMST
      *
     C                   MOVEL     F1MZIP        XXZIP5            5
     C                   MOVE      F1MZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         FNMZIP
     C                   CAT(P)    XXZIP4:0      FNMZIP
      *
     C                   MOVEL(P)  F1PAD1        FNPAD1
     C                   MOVEL(P)  F1PAD2        FNPAD2
     C                   MOVEL(P)  F1PCTY        FNPCTY
     C                   MOVEL(P)  F1PST         FNPST
      *
     C                   MOVEL     F1PZIP        XXZIP5            5
     C                   MOVE      F1PZIP        XXZIP4            4
     C     XXZIP5        CAT(P)    '-':0         FNPZIP
     C                   CAT(P)    XXZIP4:0      FNPZIP
      *
     C                   Z-ADD     F1CO#         FNCO#
      * ADD PHONE # AND FAX #
     C                   MOVE      F1FAXA        XXAREA            3
     C                   MOVEL     F1FAX         XXPH1             3
     C                   MOVE      F1FAX         XXPH2             4
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        FNFAX
      *
     C     F1PH1         IFLE      *ZEROS
     C                   MOVE      F1AREA        XXAREA            3
     C                   MOVEL     F1PH2         XXPH1             3
     C                   MOVE      F1PH2         XXPH2             4
     C                   ELSE
     C                   MOVE      F1PH1A        XXAREA            3
     C                   MOVEL     F1PH1         XXPH1             3
     C                   MOVE      F1PH1         XXPH2             4
     C                   ENDIF
     C                   EXSR      SRPHN
     C                   MOVEL(P)  XXPHON        FNPHN
      *
      ***** BEG ADD ***** 07/20/07 ***********************************
     C* Get Group Region Number and Description
     C                   MOVE      *ZEROS        FNRGN#
     C                   MOVEL     '**NONE**'    FNRGND
     C     EXKY          CHAIN     PEX01                              63
     C*
     C  N63REKY          CHAIN     PRE01                              63
     C   63FNKY          CHAIN     PRE01                              63
     C*
     C     *IN63         IFEQ      *OFF
     C                   Z-ADD     RERGN#        FNRGN#
     C                   MOVEL(P)  REDESC        FNRGND
     C                   ENDIF                                                  .63 = ON
      ***** END ADD ***** 07/20/07 ***********************************
      *
      *      As of date
     C                   MOVEL     *BLANKS       D10X
     C                   TIME                    DSDTTM
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
     C                   MOVE      D10X          FNAODT
      ***** BEG ADD ***** 04/30/10 ***********************************
     C     FNKY          CHAIN     PF201                              90
     C     *IN90         IFEQ      *OFF
     C                   Z-ADD     F2CAR#        FNCAR#
     C                   ENDIF
      ***** END ADD ***** 04/30/10 ***********************************
      *
      *  Write to output file
      *
     C                   WRITE     PFN02
     C                   ENDSR                                                  SROUT
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
     C                   ENDSR                                                  SRPHN
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
