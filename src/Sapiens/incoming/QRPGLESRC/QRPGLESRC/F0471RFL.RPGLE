     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H**sy266_ovr*** dftactgrp(*no) actgrp('QILE')
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  Program: F0471RFL                                            *
     H*  Description: Pharmacy Header EDI Info update/display         *
     H*  Created: 12/23/05  C:79 Project: FL056 From F0471R           *
     H*****************************************************************
     H*  REVISIONS                                                    *
     H* 10/04/08 C:180 SUPPORT 65159
     H*                  Fourth position of FL Phy lic can be blank
     H*                                                               *
     H*  09/25/07  C:180  SUPP 59201
     H*                   Validate SCPLIC based on following formats:
     H*
     H*                   AANNNNNNNNNNN
     H*                   (OR)
     H*                   AAANNNNNNNNNN
     H*                   (OR)
     H*                   AAAANNNNNNNNN
     H*
     H*                   Left justify and space fill to end of field
     H*                   Key alpha prefix and numeric digits of
     H*                   license number
     H*                   Must be valid value
     H*                   DO NOT zero pad numeric portion
     H*
     H*  08/22/07 C:194   SUPPORT: 59635                              *
     H*                   Lift edit on License Number to make         *
     H*                   the License Number not a required field     *
     H*                                                               *
     H*  04/23/07 C:188 PROJECT:FL061                                 *
     H*                 SET EDITST TO ADJ OR POLICY STATE BASED       *
     H*                 ON EOCPST                                     *
     H*                                                               *
     H* 03/14/07 C:182 Project: FL067                                *
     H*               1.Added Pharmacists State License Number       *
     H*               2.Changed Fld Heading for 'Payment plan' to be *
     H*                 'Payment code'                               *
     H*                                                               *
     H*  04/01/06 C:79 Project: FL056                                 *
     H*                Change Payment Plan Edit                       *
     H*                                                               *
     H*  12/23/05 C:79 Project: FL056                                 *
     H*                Add Payment Plan                               *
     H*
     H*  03/30/05 C:79 Support: 46234                                 *
     H*                1. Add date carrier received bill              *
     H*                2. Correct so program ends w/o bombing         *
     H*****************************************************************
     H*
     FF0471FFL  CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     F*  Physical file definition (for maintaining record)
     FFAEPP     UF A E             DISK
     F                                     RENAME(PEP01:DBRCDF)
     F*
     F*  EOMB Transaction File
     FFAEOP     UF   E             DISK
     F*
     F*  WCMC Control File
     FFMCXL     IF   E           K DISK
     F*
     F*  State Code Table File
     FSTSTL     IF   E           K DISK
     F*
     F*  Generic Table File
     FFTABL     IF   E           K DISK
     F*
     F*  Bill Entry Control File
     FFMBEL     IF   E           K DISK
     F*
     F*  Provider Master File
     FFMPDL     IF   E           K DISK
     F*
     F*  EOMB Detail EOB/Notes Transaction File
     FFAEEL     IF   E           K DISK
     F*** BEG DEL *** 03/30/05 ****************************************
     F*****  FAEPL   IF  E           K        DISK
     F*****                                         KINFDS DBFDSA
     F*** END DEL *** 03/30/05 ****************************************
     FFTGTL     IF   E           K DISK
     FFTGTP     IF   E             DISK
     F                                     RENAME(PGT01:PGT02)
      /COPY SCOPYMT01D
     D*** BEG ADD *** 09/25/07 ****************************************
     D @A              S              1    DIM(26) CTDATA PERRCD(26)
     D @N              S              1    DIM(10) CTDATA PERRCD(10)
     D LL              S              1    DIM(11)
     D*** END ADD *** 09/25/07 ****************************************
     D*
     D***PY SCOPYMT01I
     D*
     D*** BEG DEL *** 03/30/05 ****************************************
     D***** DBFDSA      DS
     D*****                               B 397 4000DBRRNA
     D*** END DEL *** 03/30/05 ****************************************
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Pharmacy Header Info-
     D                                     rmation')
     D COMAPI          C                   CONST('FA007R')
     D #OFSCR          C                   CONST(1)
     D*** BEG ADD *** 09/25/07 ****************************************
     D                 DS
     D  SCPLIC                 1     13
     D  SCLIC1                 1      1
     D  SCLIC2                 2      2
     D  SCLIC3                 3      3
     D  SCLIC4                 4      4
     D  SCLICA                 3     13
     D  SCLICB                 4     13
     D  SCLICC                 5     13

     I*** END ADD *** 09/25/07 ****************************************

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0          for FAEPP
     C                   PARM                    @@RRN2            9 0          for FAEOP
     C                   PARM                    @@NWCD            5
     C                   PARM                    @@CHRG            9            TOTAL CHARGE
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C     PGMLS         PLIST
     C                   PARM      '2'           ##CALL            1
     C                   PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM                    XXTBID
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C*  Key List for WCMC Control File
     C*
     C     CXKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##SUB#
     C                   KFLD                    ##FND
     C*
     C** Key List for State Code Table File
     C*
     C     STKY          KLIST
     C                   KFLD                    ##KEY
     C*
     C** Key List for Specialty Code File
     C*
     C     SPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##KEY
     C*
     C** Key List for Fee Schedule Control File
     C*
     C********** SBKY      KLIST
     C**********           KFLD           KKKEY
     C*
     C** Key List for Bill Entry Control FIle
     C*
     C     BEKY          KLIST
     C                   KFLD                    KKAST
     C                   KFLD                    KKSYS
     C                   KFLD                    KKBTYP
     C                   KFLD                    KKSTYP
     C** Key List for Provider Master File
     C*
     C     PDKY          KLIST
     C                   KFLD                    KKFEIN
     C                   KFLD                    KKMOD
     C*
     C* KEY LIST FOR Header EOB/Notes File
     C*
     C     EEKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
     C*
     C     EPKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFYR
     C                   KFLD                    KKCASE
     C                   KFLD                    KKEOB#
     C*
     C     GTKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXKEY
     C*
     C     GIKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    XXTBID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CXSUB#        ##SUB#
     C     *LIKE         DEFINE    STKEY         ##KEY
     C     *LIKE         DEFINE    EPCO#         KKCO#
     C     *LIKE         DEFINE    EPSUB#        KKSUB#
     C     *LIKE         DEFINE    EPFND         KKFND
     C     *LIKE         DEFINE    EPFYR         KKFYR
     C     *LIKE         DEFINE    EPCASE        KKCASE
     C********** *LIKE     DEFN FBKEY     KKKEY
     C     *LIKE         DEFINE    BEAST         KKAST
     C     *LIKE         DEFINE    BESYS         KKSYS
     C     *LIKE         DEFINE    BEBTYP        KKBTYP
     C     *LIKE         DEFINE    BESTYP        KKSTYP
     C     *LIKE         DEFINE    PDFEIN        KKFEIN
     C     *LIKE         DEFINE    PDMOD         KKMOD
     C     *LIKE         DEFINE    EOEOB#        KKEOB#
     C     *LIKE         DEFINE    GTTBID        XXTBID                         ADD 01/26/00
     C     *LIKE         DEFINE    GTKEY         XXKEY                          ADD 01/26/00
     C*** BEG ADD *** 04/23/07 ****************************************
     C     *LIKE         DEFINE    EOAST         EDITST
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C/COPY SCOPYMT01C
     C*
     C*================================================================
     C* DATSBR - Date check subrountine
     C*================================================================
     C     DATSBR        BEGSR
     C                   ENDSR
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
     C*
     C*
     C     SCEDI         IFEQ      *BLANKS
     C                   SETON                                        7990
     C                   GOTO      CK1END
     C                   ELSE
     C                   MOVEL(P)  'FLES'        XXTBID
     C                   MOVEL(P)  SCEDI         XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        8090
     C                   GOTO      CK1END
     C                   ENDIF
     C                   ENDIF
     C*
     C*** BEG ADD *** 12/23/05 **************************************
     C     SCPPLN        IFEQ      *BLANKS
     C                   SETON                                        9072
     C                   GOTO      CK1END
     C                   ENDIF                                                  SCPPLN IFEQ BLANK
     C*
    C*
    C*** BEG ADD *** 03/14/07 **************************************
    C*
    C*** BEG   DEL *** 08/22/07 **************************************
     C*********  SCPLIC    IFEQ *BLANKS
     C*********            SETON                     9039
     C*********            GOTO CK1END
     C*********            ENDIF                           SCPLIC IFEQ BLANK
    C*** END   DEL *** 08/22/07 **************************************
    C*
    C*** END ADD *** 03/14/07 **************************************
    C*
     C****** BEG ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C     SCPLIC        IFNE      *BLANKS
     C                   MOVE      *ZEROS        LCNT              5 0
     C*
     C* FIRST TWO CHARACTERS OF LICENSE NUMBER MUST BE ALPHA
     C*
     C     SCLIC1        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT
      *
     C     SCLIC2        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
     C*
     C                   ADD       1             LCNT              5 0
      *
     C* 3rd position can be alpha or numeric
      *
     C     SCLIC3        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC3        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
      *
     C*
     C* 4TH position can be alpha or numeric
     C*
     C***** BEGIN ADD *****  10/04/08 *********************************
     C     SCLIC4        IFGT      *BLANKS
     C***** END ADD *******  10/04/08 *********************************
      *
     C     SCLIC4        LOOKUP    @A                                     60
     C     *IN60         IFEQ      *ON
      *
     C     LCNT          IFEQ      3
     C                   ADD       1             LCNT              5 0
     C                   ELSE
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ELSE
     C                   MOVE      *IN40         SV40              1
     C     SCLIC4        LOOKUP    @N                                     40
     C     *IN40         IFEQ      *OFF
     C                   SETON                                        9091
     C                   GOTO      CK1END
     C                   ENDIF
     C                   MOVE      SV40          *IN40
     C                   ENDIF
     C***** BEGIN ADD *****  10/04/08 *********************************
     C                   ENDIF
     C***** END ADD *******  10/04/08 *********************************
      *
     C                   CLEAR                   LL
      *
     C                   SELECT
     C     LCNT          WHENEQ    2
     C                   MOVEA     SCLICA        LL
     C     LCNT          WHENEQ    3
     C                   MOVEA     SCLICB        LL
     C     LCNT          WHENEQ    4
     C                   MOVEA     SCLICC        LL
     C                   ENDSL
      *
     C     13            SUB       LCNT          MAX               3 0
     C                   Z-ADD     MAX           IX                3 0
      * Find 1st non-blank
     C                   DO        MAX
     C     LL(IX)        IFNE      *BLANKS
     C                   LEAVE
     C                   ENDIF
     C                   SUB       1             IX
     C                   ENDDO
      * Check numeric part
      *
      *
     C     LL(1)         IFGT      *ZERO
      *
     C                   DO        IX            IY                3 0
     C     LL(IY)        LOOKUP    @N                                     60
     C     *IN60         IFEQ      *OFF
     C                   SETON                                        9092
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        9092
     C                   ENDIF
      *
     C     *IN90         IFEQ      *ON
     C                   GOTO      CK1END
     C                   ENDIF
      *
     C                   ENDIF
     C****** END ADD ******* 09/25/07 *****¢¢¢¢*****¢¢¢¢*****¢¢¢¢*****
     C     SCPPLN        IFNE      *BLANKS
     C*
     C*** BEG CHG *** 04/01/06 ****************************************
     C********** SCEDI     IFEQ 'E'
     C     SCEDI         IFEQ      'Y'
     C*** END CHG *** 04/01/06 ****************************************
     C                   MOVEL(P)  'FLPP'        XXTBID
     C                   ELSE
     C                   MOVEL(P)  'FLPA'        XXTBID
     C                   ENDIF
     C*
     C                   MOVEL(P)  SCPPLN        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *ON
     C                   SETON                                        9071
     C                   GOTO      CK1END
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C                   ENDIF                                                  SCPPLN <> ' '
     C*** END ADD *** 12/23/05 **************************************
     C*
     C     CK1END        ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for srceen 2 (SCR02)
     C*================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK3 - Error checking for srceen 3 (SCR03)
     C*================================================================
     C     ERRCK3        BEGSR
     C                   ENDSR
     C*================================================================
     C* KJDO - F10=SEARCH.
     C*================================================================
     C     KJDO          BEGSR
     C*
     C*** INITIALIZE POSITION CURSOR INDICATORS
     C*
     C                   SETOFF                                       313233
     C                   SETOFF                                       343536
     C                   SETOFF                                       3738
    C*** BEG ADD ***** 03/14/07 **************************************
    C*
     C                   SETOFF                                       3940
    C*
    C*** END ADD ***** 03/14/07 **************************************
     C*
     C*** CLEAR HOLD FIELDS
     C*
     C                   SELECT
     C*
     C*
     C     HFLD          WHENEQ    'SCSPCD'
     C                   MOVEL     SCSPCD        HLSPCD            3
     C                   CLEAR                   ##KEY1
     C                   CALL      'F2335R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM                    ##KEY1           10
     C                   PARM      ' '           ##RTCD
     C*
     C     ##RTCD        IFNE      '*'
     C                   MOVE      HLSPCD        SCSPCD
     C                   SETON                                        9036      pos cursor
     C                   ELSE
     C                   MOVEL     ##KEY1        SCSPCD
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C                   SETON                                        9037      pos cursor
     C                   ENDIF
     C     HFLD          WHENEQ    'SCEDI'
    C*** BEG DEL ***** 12/22/06 **************************************
     C*****************    SETON                     3190
    C*** BEG DEL ***** 12/22/06 **************************************
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL            1
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM      *ZEROS        ##RRN             9 0
     C                   PARM      'FLES'        ##TBID            4
     C*
     C     ##RRN         IFNE      *ZEROS
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTKEY         SCEDI
     C                   MOVEL(P)  GTDESC        SCDSDS
    C*** BEG ADD ***** 12/22/06 **************************************
     C                   SETON                                        9041
    C**  END ADD ***** 12/22/06 **************************************
     C                   ENDIF
    C**  BEG ADD ***** 12/22/06 **************************************
     C                   ELSE
     C                   SETON                                        9031
    C**  END ADD ***** 12/22/06 **************************************
     C                   ENDIF
     C*** BEGIN ADD *** 12/23/05 **************************************
     C     HFLD          WHENEQ    'SCPPLN'
     C*
     C                   MOVEL(P)  'FLPA'        ##TBID
     C*
     C                   CALL      'F2380R'
     C                   PARM      '2'           ##CALL
     C                   PARM      ' '           ##KC
     C                   PARM      ' '           ##KL
     C                   PARM      *ZEROS        ##RRN
     C                   PARM                    ##TBID
     C*
     C     ##KC          IFEQ      '1'
     C     ##KL          OREQ      '1'
     C     ##RRN         OREQ      *ZEROS
     C                   SETON                                        9038
     C                   ELSE
     C     ##RRN         CHAIN     PGT02                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTKEY         SCPPLN
     C                   MOVEL(P)  GTDESC        SCPDSC
    C*** BEG CHG ***** 12/22/06 **************************************
     C*****************    SETON                     9038
     C                   SETON                                        9042
    C**  END CHG ***** 12/22/06 **************************************
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  ##KC = '1'
     C*
     C*** END   ADD *** 12/23/05 **************************************
     C*
     C*** END   ADD *** 12/23/05 **************************************
     C*
     C     HFLD          WHENEQ    'SCPLIC'
     C*
     C                   SETON                                        9040
     C*
     C*** END   ADD *** 12/23/05 **************************************
     C*
     C                   ENDSL
     C*
     C                   EXSR      DATSBR
     C*
     C                   ENDSR
     C*================================================================
     C* KKDO - F11=EOB/Notes
     C*================================================================
     C     KKDO          BEGSR
     C                   Z-ADD     0             ##RRN
     C                   Z-ADD     @@RRN2        ##RRN2            9 0
     C                   CALL      'F0355R'
     C                   PARM      ' '           ##CALL
     C                   PARM      ' '           ##KC              1
     C                   PARM      ' '           ##KL              1
     C                   PARM                    ##RRN
     C                   PARM                    ##RRN2
     C                   SETON                                        90
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   CLEAR                   SCCHRG
     C                   CLEAR                   SCMRA
     C                   CLEAR                   SCNTWK
     C                   CLEAR                   SCOTHR
     C                   CLEAR                   SCOTH2
     C                   CLEAR                   SCPAID
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   CLEAR                   SCCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C*
     C* If in ADD mode and @@RRN is not zero, then user has used F12
     C* from the detail program.  This require a chain to the
     C* Header Info file in order to access the Header information.
     C*
     C     @@MODE        IFEQ      'A'
     C     @@RRN         ANDNE     *ZEROS
     C     @@RRN         CHAIN(N)  DBRCDF                             99
     C                   ENDIF                                                  @@MODE = 'A'
     C*
     C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
     C*
     C                   MOVE      EOFEIN        KKFEIN
     C                   MOVE      EOMOD         KKMOD
     C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
     C*    OTHERWISE, USE THE ON IN FMPDP
     C     EOSPCD        IFNE      *BLANKS
     C                   MOVEL     EOSPCD        SCSPCD
     C                   ELSE
     C     PDKY          CHAIN     PPD01                              99
     C  N99              MOVEL     PDSPCD        SCSPCD
     C                   ENDIF
     C*
     C                   MOVEL     SCSPCD        ##KEY
     C     SPKY          CHAIN     PSP01                              99
     C  N99              MOVEL     SPDESC        SCSCDS
     C*
     C                   MOVEL     EOAST         SCAST
     C                   MOVE(P)   EOZIPA        SCZIPA
     C*
     C*****      @@MODE    IFNE 'A'
     C*
     C*
     C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
     C*
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C                   Z-ADD     EOEOB#        KKEOB#
     C     EEKY          CHAIN     PEE01                              99
     C     *IN99         IFEQ      *OFF
     C                   SETON                                        68
     C                   ELSE
     C                   SETOFF                                       68
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C*
     C     EPEDI         IFNE      *BLANKS
     C                   MOVE      EPEDI         SCEDI
     C                   MOVEL(P)  'FLES'        XXTBID
     C                   MOVEL(P)  SCEDI         XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCDSDS
     C                   ELSE
     C                   CLEAR                   SCDSDS
     C                   ENDIF
     C                   ENDIF                                                  EPEDI NE *BLANKS
     C                   MOVEL     EPPHAR        SCPHAR
     C*
     C                   Z-ADD     EOCHRG        SCCHRG
     C                   Z-ADD     EOMRA         SCMRA
     C                   Z-ADD     EONDIS        SCNTWK
     C                   Z-ADD     EOOTHR        SCOTHR
     C                   Z-ADD     EOOTH2        SCOTH2
     C                   Z-ADD     EOPAID        SCPAID
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   Z-ADD     EOCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C*
     C*****                ENDIF                           @@MODE NE 'A'
     C*** BEG ADD *** 12/23/05 ****************************************
     C     EPPPLN        IFNE      *BLANKS
     C                   MOVE      EPPPLN        SCPPLN
     C*
     C                   MOVEL(P)  'FLPA'        XXTBID
     C*
     C                   MOVEL(P)  SCPPLN        XXKEY
     C     GTKY          CHAIN     PGT01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  GTDESC        SCPDSC
     C                   ELSE
     C                   CLEAR                   SCPDSC
     C                   ENDIF                                                  *IN99 IFEQ *ON
     C*
     C                   ENDIF                                                  EPPPLN IFNE BLANK
     C*** END ADD *** 12/23/05 ****************************************
    C*
    C*** BEG ADD *** 03/14/07 ****************************************
    C*
     C                   MOVE      EPPLIC        SCPLIC
    C*
    C*** END ADD *** 02/14/07 ****************************************
    C*
     C                   ENDSR
     C*================================================================
     C* MVSCDB - Move screen fields to data base fields
     C*================================================================
     C     MVSCDB        BEGSR
     C*
     C                   Z-ADD     EOCO#         EPCO#
     C                   Z-ADD     EOSUB#        EPSUB#
     C                   Z-ADD     EOFND         EPFND
     C                   Z-ADD     EOFYR         EPFYR
     C                   Z-ADD     EOCASE        EPCASE
     C                   MOVE      EOSS#         EPSS#
     C                   Z-ADD     EOACDT        EPACDT
     C*
     C                   MOVE      SCEDI         EPEDI
     C                   MOVE      SCPHAR        EPPHAR
     C                   MOVE      SCSPCD        EOSPCD
     C*
     C*****                Z-ADDSCCHRG    EOCHRG
     C*** BEG ADD *** 12/23/05 ****************************************
     C                   MOVE      SCPPLN        EPPPLN
     C*** END ADD *** 12/23/05 ****************************************
    C*
    C*** BEG ADD *** 03/14/07 ****************************************
    C*
     C                   MOVE      SCPLIC        EPPLIC
    C*
    C*** END ADD *** 03/14/07 ****************************************
    C*
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
     C     *INKJ         CASEQ     *ON           KJDO                           F10=search
     C     *INKK         CASEQ     *ON           KKDO                           F11=EOB/Notes
     C                   ENDCS
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*** BEG DEL *** 03/30/05 ****************************************
     C********
     C***Get*relative record number for header record and put in
     C***parm*@@RRN.  This will allow the ability to access header
     C***data*if user F12=Cancel from detail screen on an add.
     C********
     C********   @@MODE    IFEQ 'A'
     C********   EPKY      CHAINPEP01                99
     C********   *IN99     IFEQ *OFF
     C********             Z-ADDDBRRNA    @@RRN
     C********             ENDIF                           *IN99 = *OFF
     C********             ENDIF                           @@MODE = 'A'
     C********
     C**The*following call to the WRITER subroutine is necessary
     C**because if program bombs out in add mode before the user
     C**takes*action to write the header record, 'change' mode
     C**will*be looking to update a record that does not exist.
     C**Thus,*if the @@RRN (FAEUP) is blanks in change mode, we must
     C**call*the write subroutine to add the header record.
     C********(WILL BE EXECUTED IF USER HITS ENTER)
     C********
     C********   @@MODE    IFEQ 'C'
     C********   *INKC     ANDEQ*OFF
     C********   @@RRN     ANDEQ*ZEROS
     C********             EXSR WRITER
     C********             ENDIF                           @@MODE = 'C'
     C********
     C********
     C****Call Cancel/Pend program if user hits F3=EXIT
     C****If*in Add mode, enable user to cancel bill (##CANC = Y)
     C****Otherwise do not allow cancel capability (##CANC = N)
     C********
     C********   *INKC     IFEQ *ON
     C********   @@MODE    ANDNE'D'
     C********
     C********   @@MODE    IFEQ 'A'
     C********             MOVE 'Y'       ##CANC           able to cancel
     C********             ELSE
     C********             MOVE ' '       ##CANC           unable to cancel
     C********             ENDIF
     C********
     C**If*in*add mode and F3=Exit is hit, call writer subroutine
     C**to*write the header record
     C********
     C********   @@MODE    IFEQ 'A'
     C********   *INKC     ANDEQ*ON
     C********             EXSR WRITER
     C********             ENDIF                           @@MODE = 'A'
     C********
     C**If*in*'change' mode and F3=Exit is hit, call update subroutine
     C**to*update the header record with the changed info.
     C**This*will work on both SCR01 and SCR02.
     C********
     C********   @@MODE    IFEQ 'C'
     C********   *INKC     ANDEQ*ON
     C********   @@RRN     ANDNE*ZEROS
     C********             EXSR UPDATR
     C********             ENDIF                           @@MODE = 'C'
     C********
     C**The*following call to the WRITER subroutine is necessary
     C**because if program bombs out in add mode before the user
     C**takes*action to write the header record, 'change' mode
     C**will*be looking to update a record that does not exist.
     C**Thus,*if the @@RRN (FAEUP) is blanks in change mode, we must
     C**call*the write subroutine to add the header record.
     C********(WILL BE EXECUTED IF USER HITS F3=EXIT)
     C********
     C********   @@MODE    IFEQ 'C'
     C********   @@RRN     ANDEQ*ZEROS
     C********             EXSR WRITER
     C********             ENDIF                           @@MODE = 'C'
     C********
     C********             Z-ADD@@RRN2    ##RRN
     C********             CALL 'F0392R'
     C********   *INKL     PARM ' '       ##KL
     C********             PARM ' '       ##MODE  1
     C********             PARM           ##RRN   90
     C********             PARM           ##CANC  1
     C********             PARM           @@CHRG
     C********             PARM ' '       ##RTCD  1
     C**********           MOVE '1'       @@KC
     C********
     C********             ENDIF                           *INKC = *ON
     C*** END DEL *** 03/30/05 ****************************************
     C*
     C*
     C                   SETON                                        LR
     C*** BEG DEL *** 03/30/05 ****************************************
     C******************   MOVE '1'       OUTFLG
     C*** END DEL *** 03/30/05 ****************************************
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C** Get standard screen headings
     C*
     C     @@RRN2        CHAIN(N)  PEO01                              99
     C*
     C*** BEG ADD *** 04/23/07 ****************************************
     C*
     C*  SET EDITST TO POLICY OR ADJ STATE BASED ON CPST FLAG
     C*
     C     EOCPST        IFEQ      'C'
     C                   MOVEL(P)  EOPOST        EDITST
     C                   ELSE
     C                   MOVEL(P)  EOAST         EDITST
     C                   ENDIF
     C*
     C*** END ADD *** 04/23/07 ****************************************
     C*
     C                   MOVE      EOCO#         #1CO#
     C                   MOVE      EOSUB#        #1SUB#
     C                   MOVE      EOFND         #1FND
     C                   MOVE      EOFYR         #1FYR
     C                   MOVE      EOCASE        #1CASE
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C*
     C                   CALL      'FA702R'
     C                   PARM                    ##RTCD            1
     C                   PARM                    #1CO#             3
     C                   PARM                    #1SUB#            5
     C                   PARM                    #1FND             3
     C                   PARM                    #1FYR             3
     C                   PARM                    #1CASE            7
     C                   PARM                    #1L1             78
     C                   PARM                    #1L2             78
     C*
     C                   MOVEL     #1L1          SCHD1
     C                   MOVEL     #1L2          SCHD2
     C*
     C                   CLEAR                   ##RTCD
     C                   CLEAR                   #1L1
     C                   CLEAR                   #1L2
     C                   CALL      'FA707R'
     C                   PARM                    ##RTCD
     C                   PARM                    @@RRN2
     C                   PARM                    #1L1
     C                   MOVEL     #1L1          SCHD3
     C*
     C* CALL API FOR SYSTEM FLAGS
     C*
     C                   CALL      'SA001R'
     C                   PARM      ' '           ##WCIF            1
     C                   PARM      ' '           ##MCIF            1
     C                   PARM      ' '           ##WCIN            1
     C                   PARM      ' '           ##AR              1
     C                   PARM      ' '           ##NTWK            1
     C                   PARM      ' '           ##UR              1
     C                   PARM      ' '           ##MED             1
     C                   PARM      ' '           ##EDI             1
     C                   PARM      ' '           ##WCDK            1
     C                   PARM      ' '           ##RHAB            1
     C*
     C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
     C* NETWORK IS INSTALLED
     C*
     C     ##NTWK        IFEQ      'Y'
     C                   SETON                                        47
     C                   ELSE
     C                   SETOFF                                       47
     C                   ENDIF                                                  ##NTWK = 'Y'
     C*
     C** Get 'Reductions' headings
     C*
     C                   Z-ADD     EOSUB#        ##SUB#
     C     CXKY          CHAIN     PCX01                              99
     C                   MOVEL     CXHMRA        SCHMRA
     C                   MOVEL     CXHNET        SCHNET
     C                   MOVEL     CXHOT1        SCHOT1
     C                   MOVEL     CXHOT2        SCHOT2
     C*
     C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
     C** AND REQUIRED
     C*
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVELEOAST     ##KEY
     C                   MOVEL     EDITST        ##KEY
     C*** END CHG *** 04/23/07 ****************************************
     C     STKY          CHAIN     PST01                              99
     C     *IN99         IFEQ      *OFF
     C     ST1A4         ANDEQ     'Y'
     C                   SETON                                        30
     C*** BEG CHG *** 04/23/07 ****************************************
     C*****                MOVE EOAST     KKAST
     C                   MOVE      EDITST        KKAST
     C*** END CHG *** 04/23/07 ****************************************
     C                   MOVE      EOSYS         KKSYS
     C                   MOVE      EOBTYP        KKBTYP
     C                   MOVEL     EOSTYP        KKSTYP
     C*
     C     BEKY          CHAIN     PBE01                              98
     C  N98              MOVE      BEEDI         SCEDI
     C*
     C                   ELSE
     C*
     C                   SETOFF                                       30
     C                   ENDIF                                                  *IN99 IFEQ *OFF
     C*
     C** SET VARIOUS FIELDS
     C*
     C                   Z-ADD     EOCO#         KKCO#
     C                   Z-ADD     EOSUB#        KKSUB#
     C                   Z-ADD     EOFND         KKFND
     C                   Z-ADD     EOFYR         KKFYR
     C                   Z-ADD     EOCASE        KKCASE
     C*
     C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
     C*
     C     EONTWK        IFNE      *BLANKS
     C                   MOVEL(P)  EONTWK        SCNWCD
     C                   SETON                                        69
     C                   ELSE
     C                   SETOFF                                       69
     C                   ENDIF                                                  EONTWK
     C*
     C**SET*INITIAL*CURSOR POSITON TO PHARMACY NUMBER      DEL 01/26/00
     C*****************    SETON                     37    DEL 01/26/00
     C* SET INITIAL CURSOR POSITON TO BILLING TYPE CODE    ADD 01/26/00
     C                   SETON                                        38        ADD 01/26/00
     C*
     C* PROTECT SPECIALTY IF EORCN>0
     C*
     C     EORCN         IFGT      *ZERO
     C                   SETON                                        29
     C                   ELSE
     C*
     C                   SETOFF                                       29
     C                   ENDIF                                                  EORCN>0
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
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   EXSR      MVSCDB
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1PGM         EPPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C*
     C                   UPDATE    DBRCDF
     C                   ENDIF
     C*
     C** UPDATE EOMB TRANSACTION FILE
     C*
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVE      SCEDI         EOTAPE
     C*** BEG ADD *** 03/30/05 ****************************************
     C                   MOVE      SCCRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EOCRDT
     C*** END ADD *** 03/30/05 ****************************************
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRITER - Write data base record and after processing
     C*================================================================
     C     WRITER        BEGSR
     C*
     C* If in ADD mode and @@RRN (FAEPP) is not zeros, then the user
     C* has used F12 from detail screen.  Must delete previous written
     C* Header record and write the new one.  This prevents having two
     C* Header records for the bill.
     C*
     C     @@RRN         IFNE      *ZEROS
     C     @@RRN         CHAIN     DBRCDF                             99
     C     *IN99         IFEQ      *OFF
     C                   DELETE    DBRCDF
     C                   ENDIF                                                  *IN99 = *OFF
     C                   ENDIF                                                  @@RRN NE *ZEROS
     C*
     C                   CLEAR                   DBRCDF
     C                   EXSR      MVSCDB
     C                   Z-ADD     EOEOB#        EPEOB#
     C                   MOVE      'A'           EPSTAT
     C                   MOVEL     Q1USER        EPUSER
     C                   MOVEL     Q1USER        EPIUSR
     C                   MOVEL     Q1PGM         EPPGM
     C                   MOVEL     Q1PGM         EPIPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        EPTIME
     C                   Z-ADD     DSTIME        EPITIM
     C*
     C                   Z-ADD     DSDATE        SRDTI
     C                   MOVE      *BLANKS       SRDTO
     C                   MOVE      *BLANKS       SRERR
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         EPDT
     C                   MOVEL     SRDTO         EPIDT
     C*
     C                   WRITE     DBRCDF
     C     @@RRN2        CHAIN     PEO01                              99
     C     *IN99         IFEQ      *OFF
     C******               Z-ADDSCCHRG    EOCHRG
     C                   MOVEL(P)  SCSPCD        EOSPCD
     C                   MOVE      SCEDI         EOTAPE
     C                   MOVEL     Q1USER        EOUSER
     C                   MOVEL     Q1PGM         EOPGM
     C                   Z-ADD     DSTIME        EOTIME
     C                   MOVEL     SRDTO         EODT
     C                   UPDATE    PEO01
     C                   ENDIF
     C*
     C                   FEOD      FAEPP
     C*
     C                   MOVE      '1'           OUTFLG
     C                   ENDSR
**  TABLE @A  - ALPHABET
ABCDEFGHIJKLMNOPQRSTUVWXYZ
**
0123456789
