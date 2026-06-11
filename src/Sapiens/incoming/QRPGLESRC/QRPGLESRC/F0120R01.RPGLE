     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Program description:                                         *
     H*  This program will allow the user to setup various security   *
     H*  levels and other variable WCMC information for the Medical   *
     H*  case information.                                            *
     H*                                                               *
     H*  PROGRAMMER: C:94  STARTED BY: C:05                           *
     H*  CREATED:    01/08/98          PROJECT: MC157                 *
     H*  FINISHED:   07/01/98                                         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/28/18 C:180 PROJECT: 114MHV support 69834                 *
     H*                 Save FMCXP rrn to prevent it from being       *
     H*                 cleared by FEOD .                             *
     H*                                                               *
     H*  03/06/00 C:62 PROJECT: UR025                                 *
     H*                ADDED A PRE-CERT INFORMATION SCREENS           *
     H*                                                               *
     H*  01/20/99 C:53 PROJECT: MC178                                 *
     H*                ADDED OVERRIDE CLAIM SECURITY FIELDS           *
     H*                                                               *
     H*  01/29/99 C:53 PROJECT: MCMNT                                 *
     H*                WARNING MSG IN DISPLAY MODE FOR NEEDED ENTRY   *
     H*                                                               *
     H*  01/06/99 C:53 PROJECT: MC152                                 *
     H*                ACTIVATED PROVIDER BILLS SCREEN.               *
     H*                                                               *
     H*  11/17/98 C:62 PROJECT: MC145                                 *
     H*                STAFF HEADER NEEDS TO BE INITIALIZED.          *
     H*                                                               *
     H*  09/29/98 C:94 PROJECT: 87MFX                                 *
     H*                ADD CALL TO F0120R99(CLT MOD) BASED ON AN      *
     H*                INDICATOR                                      *
     H*                                                               *
     H*  09/09/98 C:62 PROJECT:MC157                                  *
     H*                -Make the security type fields required.       *
     H*                -Move F10=Search position to setofs to OTHFKS. *
     H*                                                               *
     H*****************************************************************
     H*  /COPY INDICATOR USAGE:                                       *
     H*   90      General error indicator                             *
     H*                                                               *
     H*  GENERAL INDICATOR USAGE:                                     *
     H*   99      General chain                                       *
     H*                                                               *
     H*  SCR01 INDICATOR USAGE:                                       *
     H*   10-17   ERROR MSG AND CURSOR POSITIONING                    *
     H*   71      CM nurse heading required                           *
     H*   76      Delete claim security level must > zero             *
     H*   77      Delete claim security type must be valid            *
     H*   78      Transfer claim security level must > zero           *
     H*   79      Transfer claim security type must be valid          *
     H*   80      Allow WCMS claim data update must be Y or N         *
     H*   81      Data has been updated since entry into this record  *
     H*   82      Allow MC case closure must be Y or N
     H*****************************************************************
     H**
     H**  Called from: F0060R   - Option 12
     H**               F0060R01 - A/R, Network and EDI not installed
     H**               F0060R02 - Network and EDI not installed
     H**               F0060R03 - EDI not installed
     H**               F0061R
     H**
     H**  Calls to:    F0121R01
     H**               F2380R
     H**
     H****************************************************************
     FF0120F01  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFMCXP     UF A E             DISK
     F                                     RENAME(PCX01:DBRCDF)
     F                                     INFDS(DBFDS)
     FFMSML     IF   E           K DISK
     FFMFNL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     FFTGTL     IF   E           K DISK
     F                                     RENAME(PGT01:PGT02)
     F*** BEGIN ADD *** 03/06/00 **************************************
     FSTCTL1    IF   E           K DISK
     F*** END   ADD *** 03/06/00 **************************************
      /COPY SCOPYMT01D
     D*
     D***PY SCOPYMT01I
     D*
     D DBFDS           DS
     D  DBRRN                397    400B 0
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Medical Case Control-
     D                                      Information')
     D COMAPI          C                   CONST('FA007R')
     D**** Begin Add *** 03/28/18 *************************************
     D SVRRN           S              9P 0 INZ
     D**** End   Add *** 03/28/18 *************************************

     I*** BEGIN DEL *** 03/06/00 **************************************
     I**********    2                     C         #OFSCR
     I*** END   DEL *** 03/06/00 **************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@SUB#            5 0
     C                   PARM                    @@FND             3 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*  based on User Parameter values, not screen/*ENTRY values
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C* Generic Table File Search Parm List
     C     GTFPL         PLIST
     C                   PARM      '2'           ##CALL            1
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    ##TBID
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*** Key list to WCMC Control File
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** Key list to Subscriber Master File
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C*
     C*** Key list to Group Master File
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C*
     C*** Key list to Generic Table File
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKTBID
     C                   KFLD                    KKKEY
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        KKSUB#
     C     *LIKE         DEFINE    CXFND         KKFND
     C     *LIKE         DEFINE    GTTBID        ##TBID
     C     *LIKE         DEFINE    GTTBID        KKTBID
     C     *LIKE         DEFINE    GTKEY         KKKEY
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
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   ADD       1             XXPAGE
     C*** END   ADD *** 03/06/00 **************************************
     C                   EXSR      NXTPGM                                       ADD 01/06/99
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     *IN90         IFEQ      *ON
     C*
     C                   SUB       1             XXPAGE
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Error checking for screen 1 (SCR01)
     C*================================================================
     C     ERRCK1        BEGSR
     C*
     C*  Edit CM nurse heading - Required
     C     SCHDNU        IFEQ      *BLANKS
     C                   SETON                                        71  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Claim deletion security level CANNOT BE < OR = 0
     C     SCCLDL        IFLE      *ZERO
     C                   SETON                                        76  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Claim deletion security type must be valid..Edit against
     C*  FTGTP via 'SECT'
     C*
     C**** BEGIN DEL *** 09/09/98 *************************************
     C********** SCCLDT    IFNE ' '
     C**** END   DEL *** 09/09/98 *************************************
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  SCCLDT        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        77  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   MOVEL     GTDESC        SCCLDD                         ADD 01/20/99
     C**** BEGIN DEL *** 09/09/98 *************************************
     C**********           ENDIF
     C**** END   DEL *** 09/09/98 *************************************
     C*
     C*  Claim transfer security level CANNOT BE < OR = 0
     C     SCCLTL        IFLE      *ZERO
     C                   SETON                                        78  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Claim transfer security type must be valid..Edit against
     C*  FTGTP via 'SECT'
     C*
     C**** BEGIN DEL *** 09/09/98 *************************************
     C********** SCCLTT    IFNE ' '
     C**** END   DEL *** 09/09/98 *************************************
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  SCCLTT        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        79  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   MOVEL     GTDESC        SCCLTD                         ADD 01/20/99
     C*
     C**** BEG ADD *** 01/20/99 *************************************
     C*
     C*  Claim override security level CANNOT BE < OR = 0
     C*
     C     SCDNSL        IFLE      *ZERO
     C                   SETON                                        83  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   END
     C*
     C*  Claim override security type must be valid..Edit against
     C*  FTGTP via 'SECT'
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  SCDNST        KKKEY
     C     GTKY          CHAIN     PGT02                              99
     C   99              SETON                                        84  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   MOVEL     GTDESC        SCDNSD                         ADD 01/20/99
     C*
     C**** END ADD *** 01/20/99 *************************************
     C**** BEGIN DEL *** 09/09/98 *************************************
     C**********           ENDIF
     C**** END   DEL *** 09/09/98 *************************************
     C*
     C*********************** FUTURE USE ***************************
     C*  Allow MC case closure-field update must be Y or N
     C********** SCCLMC    IFNE 'Y'
     C********** SCCLMC    ANDNE'N'
     C**********           SETON                     82  90
     C********** *IN90     CABEQ'1'       ERRSTP
     C**********           END
     C*********************** FUTURE USE ***************************
     C*
     C* Check to be sure screen has not been updated since entry
     C     CXDT          IFGT      SAVEDT                                       MAINDT>SVDT
     C     CXDT          OREQ      SAVEDT                                       MAINDT=SVDT
     C     CXTIME        ANDGT     SAVETM                                       MAINTM>SVTM
     C                   SETON                                        81  90
     C     *IN90         CABEQ     '1'           ERRSTP
     C                   ENDIF
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   Z-ADD     1             XXPAGE
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C     ERRSTP        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for screen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     SCRVDT        CHAIN     PCT01                              99
     C*
     C     *IN99         IFEQ      *ON
     C                   SETON                                        7190
     C                   ENDIF                                                  *IN85 IFEQ
     C*
     C     *IN90         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'I'
     C                   SETON                                        7190
     C                   ENDIF                                                  *IN90 IFEQ
     C*
     C                   Z-ADD     2             XXPAGE
     C*** END   ADD *** 03/06/00 **************************************
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
     C**** BEGIN DEL *** 09/09/98 *************************************
     C*
     C*** TURNING OFF INDICATORS FOR ERROR MSG AND CURSOR POSITIONING
     C*
     C**********           SETOF                     101112
     C**********           SETOF                     131415
     C**********           SETOF                     1617
     C*
     C**********           MOVEA'0000'    *IN,41
     C**** END   DEL *** 09/09/98 *************************************
     C*
     C                   SELECT
     C*------------
     C* SCR01: Delete Claim Security Type
     C     HFLD          WHENEQ    'SCCLDT'
     C                   MOVEL     'SECT'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        80  41
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCCLDT
     C                   MOVEL     GTDESC        SCCLDD
     C                   SETON                                        42
     C                   ENDIF
     C*
     C                   SETON                                        90
     C*
     C*------------
     C* SCR01: Transfer Claim Security Type
     C     HFLD          WHENEQ    'SCCLTT'
     C                   MOVEL     'SECT'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        80  43
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCCLTT
     C                   MOVEL     GTDESC        SCCLTD
     C                   SETON                                        44
     C                   ENDIF
     C*------------
     C* SCR01: Override Claim Security Type
     C     HFLD          WHENEQ    'SCDNST'
     C                   MOVEL     'SECT'        ##TBID
     C                   CALL      'F2380R'      GTFPL
     C*
     C     *INKL         IFEQ      *ON
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        80  45
     C                   ELSE
     C     ##RRN         CHAIN     PGT01                              99
     C                   MOVEL     GTKEY         SCDNST
     C                   MOVEL     GTDESC        SCDNSD
     C                   ENDIF
     C*
     C*** SETON  ERROR MSG ON FIELDS THAT CANNOT BE SEARCHED
     C*** USING INDICATORS 10 - 17
     C     HFLD          WHENEQ    'SCHDNU'
     C                   SETON                                        1090
     C     HFLD          WHENEQ    'SCHDS3'
     C                   SETON                                        1190
     C     HFLD          WHENEQ    'SCHDSU'
     C                   SETON                                        1290
     C     HFLD          WHENEQ    'SCHDCS'
     C                   SETON                                        1390
     C     HFLD          WHENEQ    'SCHDCA'
     C                   SETON                                        1490
     C     HFLD          WHENEQ    'SCHDFA'
     C                   SETON                                        1590
     C     HFLD          WHENEQ    'SCCLDL'
     C                   SETON                                        1690
     C     HFLD          WHENEQ    'SCCLTL'
     C                   SETON                                        1790
     C*** BEG ADD *** 01/20/98 ****************************
     C     HFLD          WHENEQ    'SCDNSL'
     C                   SETON                                        1890
     C*** END ADD *** 01/20/98 ****************************
     C                   ENDSL
     C*** BEGIN ADD *** 03/06/00 **************************************
     C* SCR02 SEARCH
     C*
     C                   SELECT
     C*
     C     HFLD          WHENEQ    'SCRVDT'
     C*
     C                   CALL      'SC580R'
     C                   PARM      *BLANKS       ##RMCD            5
     C                   PARM      *BLANKS       ##RTC             1
      *
     C     ##RMCD        IFNE      *BLANKS
     C*
     C                   MOVEL(P)  ##RMCD        SCRVDT
     C*
     C     SCRVDT        CHAIN     PCT01                              99
     C*
     C  N99              MOVEL(P)  CTDESC        SCRVDD
     C   99              MOVEL(P)  *BLANKS       SCRVDD
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*** END   ADD *** 03/06/00 **************************************
     C                   SETON                                        90
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     *INKL         IFEQ      *ON
     C*
     C                   SUB       1             XXPAGE
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C                   MOVE      KKSUB#        SCSUB#
     C     SMKY          CHAIN     PSM01                              99
     C                   MOVEL     SMNAME        SCSNAM
     C*
     C                   MOVE      KKFND         SCFND
     C     FNKY          CHAIN     PFN01                              99
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C     *IN40         IFEQ      *OFF
     C*
     C                   MOVEL     CXHDNU        SCHDNU
     C                   MOVEL     CXHDSU        SCHDSU
     C                   MOVEL     CXHDS3        SCHDS3
     C                   MOVEL     CXHDCS        SCHDCS
     C                   MOVEL     CXHDCA        SCHDCA
     C                   MOVEL     CXHDFA        SCHDFA
     C                   Z-ADD     CXCLDL        SCCLDL
     C                   MOVEL     CXCLDT        SCCLDT
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  CXCLDT        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCCLDD
     C                   ENDIF                                                  *IN60
     C*
     C                   Z-ADD     CXCLTL        SCCLTL
     C                   MOVEL     CXCLTT        SCCLTT
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  CXCLTT        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCCLTD
     C                   ENDIF                                                  *IN60
     C*
     C*
     C*** BEG ADD *** 01/20/98 ****************************
     C                   Z-ADD     CXDNSL        SCDNSL
     C                   MOVEL     CXDNST        SCDNST
     C*
     C                   MOVEL(P)  'SECT'        KKTBID
     C                   MOVEL(P)  CXDNST        KKKEY
     C     GTKY          CHAIN     PGT02                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDNSD
     C                   ENDIF                                                  *IN60
     C*** END ADD *** 01/20/98 ****************************
     C*
     C**************       MOVELCXCLMC    SCCLMC           **FUTURE USE
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   MOVEL(P)  CXRVDT        SCRVDT
     C*
     C     SCRVDT        CHAIN     PCT01                              99
     C*
     C  N99              MOVEL(P)  CTDESC        SCRVDD
     C   99              MOVEL(P)  *BLANKS       SCRVDD
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C                   ELSE                                                   *IN40
     C*
     C                   MOVE      *BLANKS       SCHDNU
     C                   MOVE      *BLANKS       SCHDSU
     C*** BEGIN ADD *** 11/17/98 **************************************
     C                   MOVE      *BLANKS       SCHDS3
     C*** END   ADD *** 11/17/98 **************************************
     C                   MOVE      *BLANKS       SCHDCS
     C                   MOVE      *BLANKS       SCHDCA
     C                   MOVE      *BLANKS       SCHDFA
     C                   Z-ADD     *ZEROS        SCCLDL
     C                   MOVE      *BLANK        SCCLDT
     C                   MOVE      *BLANKS       SCCLDD
     C                   Z-ADD     *ZEROS        SCCLTL
     C                   MOVE      *BLANK        SCCLTT
     C                   MOVE      *BLANKS       SCCLTD
     C*** BEG ADD *** 01/20/98 ****************************
     C                   Z-ADD     *ZEROS        SCDNSL
     C                   MOVE      *BLANKS       SCDNST
     C                   MOVE      *BLANKS       SCDNSD
     C*** END ADD *** 01/20/98 ****************************
     C**************       MOVE *BLANK    SCCLMC           **FUTURE USE
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   MOVEL(P)  *BLANKS       SCRVDT
     C                   MOVEL(P)  *BLANKS       SCRVDD
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C                   ENDIF                                                  *IN40
     C*
     C                   ENDSR                                                  MVDBSC
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C                   Z-ADD     ##CO#         CXCO#
     C                   Z-ADD     SCSUB#        CXSUB#
     C                   Z-ADD     SCFND         CXFND
     C                   MOVE      SCHDNU        CXHDNU
     C                   MOVE      SCHDSU        CXHDSU
     C                   MOVEL     SCHDS3        CXHDS3
     C                   MOVE      SCHDCS        CXHDCS
     C                   MOVE      SCHDCA        CXHDCA
     C                   MOVE      SCHDFA        CXHDFA
     C                   Z-ADD     SCCLDL        CXCLDL
     C                   MOVE      SCCLDT        CXCLDT
     C                   Z-ADD     SCCLTL        CXCLTL
     C                   MOVEL     SCCLTT        CXCLTT
     C*** BEG ADD *** 01/20/98 ****************************
     C                   Z-ADD     SCDNSL        CXDNSL
     C                   MOVEL     SCDNST        CXDNST
     C*** END ADD *** 01/20/98 ****************************
     C****************     MOVE SCCLMC    CXCLMC           **FUTURE USE
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   MOVEL(P)  SCRVDT        CXRVDT
     C*** END   ADD *** 03/06/00 **************************************
     C                   ENDSR                                                  MVSCDB
     C*================================================================
     C* NXTPGM - EXECUTE NEXT PROGRAM
     C*================================================================
     C     NXTPGM        BEGSR
     C*
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     XXPAGE        IFEQ      #OFSCR
     C*** END   ADD *** 03/06/00 **************************************
     C*** BEG ADD *** 01/06/99 *************
     C                   MOVE      @@MODE        ##MODE            1
     C*
     C     ##PRVB        IFEQ      'Y'
     C                   CALL      'F0121R01'
     C                   PARM      ' '           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   EXSR      GETDTM
     C                   ENDIF
     C*
     C                   ELSE
     C*** END ADD *** 01/06/99 *************
     C*** BEG ADD *** 09/29/98 ****************************************
     C     *INKL         IFEQ      *OFF
     C*
     C                   CALL      'F0120R99'                           60
     C                   PARM      ' '           ##CALL
     C     *INKC         PARM      '0'           ##KC
     C     *INKL         PARM      '0'           ##KL
     C                   PARM                    ##MODE
     C                   PARM      DBRRN         ##RRN
     C                   PARM                    KKSUB#
     C                   PARM                    KKFND
     C*
     C     *INKL         IFEQ      '1'
     C                   MOVE      *ON           *IN90
     C                   MOVE      *OFF          *INKL
     C                   EXSR      GETDTM
     C                   ENDIF
     C                   ENDIF
     C*** END ADD *** 09/29/98 ****************************************
     C                   ENDIF                                                  ADD 01/06/99
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
     C                   ENDSR                                                  NXTPGM
     C*================================================================
     C* OTHERR - Processing for other modes
     C*================================================================
     C     OTHERR        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Processing for other functions keys
     C*================================================================
     C     OTHFKS        BEGSR
     C**** BEGIN ADD *** 09/09/98 *************************************
     C*
     C*** TURNING OFF INDICATORS FOR ERROR MSG AND CURSOR POSITIONING
     C*
     C                   SETOFF                                       101112
     C                   SETOFF                                       131415
     C                   SETOFF                                       161718
     C*
     C                   MOVEA     '0000'        *IN(41)
     C**** END   ADD *** 09/09/98 *************************************
     C*** F10=SEARCH
     C     *INKJ         CASEQ     *ON           KJDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*** BEGIN ADD *** 03/06/00 **************************************
     C                   Z-ADD     2             #OFSCR            1 0
     C                   Z-ADD     0             XXPAGE            1 0
     C*** END   ADD *** 03/06/00 **************************************
     C*** BEG ADD *** 01/06/99 *************
      * CALL SYSTEM API TO ACCESS SYSTEM CONTROL RECORD
      *
     C                   CALL      'SA001R'
     C                   PARM                    ##WCIF            1            WC INST
     C                   PARM                    ##MCIF            1            MC INST
     C                   PARM                    ##WCIN            1            WC INTERFACE
     C                   PARM                    ##AR              1            MC AR INST
     C                   PARM                    ##NTWK            1            NETWORK INST
     C                   PARM                    ##UR              1            PRECERT INST
     C                   PARM                    ##MED             1            MEDMGMT INST
     C                   PARM                    ##EDI             1            EDI INST
     C                   PARM                    ##WCDK            1            WC DARTS KEY
     C                   PARM                    ##RHAB            1            REHAB INST
     C                   PARM                    ##MSS             1            MS SUPPORT
     C                   PARM                    ##FSCH            1            FEE SCHEDULE
     C                   PARM                    ##PRVB            1            PROVIDER BILLS
     C*** END ADD *** 01/06/99 *************
     C*
     C     @@MODE        COMP      'D'                                    09    INQUIRY
     C     @@MODE        COMP      'A'                                    40    ADD
     C*** BEGIN ADD *** 03/06/00 **************************************
     C     ##UR          IFNE      'Y'
     C*
     C                   Z-ADD     1             #OFSCR
     C*
     C                   ENDIF
     C*** END   ADD *** 03/06/00 **************************************
     C*
     C*** BEG ADD *** 11/29/99 *****************************
     C                   MOVE      *OFF          *IN46
     C     *IN09         IFEQ      *ON
     C     @@RRN         ANDEQ     *ZEROS
     C                   MOVE      *ON           *IN46
     C                   ENDIF
     C*** END ADD *** 11/29/99 *****************************
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C*
     C                   MOVEL     ##DATA        SCPRDS
     C                   MOVE      *ON           DSPTYP
     C*
     C                   Z-ADD     @@SUB#        KKSUB#
     C                   Z-ADD     @@FND         KKFND
     C*
     C                   MOVE      *ON           OUTFLG
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C*
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CXDT
     C                   MOVEL     DSTIME        CXTIME
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C                   EXSR      NXTPGM                                       ADD 01/06/99
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C*
     C                   MOVE      'A'           CXSTAT
     C                   MOVEL     Q1USER        CXUSER
     C                   MOVEL     Q1PGM         CXPGM
     C*
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     DSTIME        CXTIME
     C                   MOVEL     SRDTO         CXDT
     C*
     C                   MOVEL     Q1USER        CXIUSR
     C                   MOVEL     Q1PGM         CXIPGM
     C                   MOVEL     DSTIME        CXITIM
     C                   MOVEL     SRDTO         CXIDT
     C*
     C                   WRITE     DBRCDF
      /Free
       //*** Begin Add *** 03/28/18 ************************************
       SVRRN = DBRRN;
       //*** End   Add *** 03/28/18 ************************************
      /End-Free
     C                   FEOD      FMCXP
      /Free
       //*** Begin Add *** 03/28/18 ************************************
       DBRRN = SVRRN;
       //*** End   Add *** 03/28/18 ************************************
      /End-Free
     C                   EXSR      NXTPGM                                       ADD 01/06/99
     C                   ENDSR
