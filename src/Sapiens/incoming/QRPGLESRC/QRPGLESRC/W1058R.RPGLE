     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W1058R - WORK WITH OWNERS/OFFICERS  ALL EMPLOYERS
     H Bnddir('WTBNDDIR')
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO VIEW ALL     *
     H*                RECORDS TO THE OWNER/OFFICER FILE.             *
     H*                                                               *
     H*                                                               *
     H*  CREATED: 08/27/97  PROGRAMMER: C:62  PROJECT: WC682          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H* 06/09/20 C:298 Project: SY420
     H*                Mask SSN
     H*                                                               *
     H*  01/07/02 C:136 SUPPORT 30608                                 *
     H*                 2nd view w/n page if SS#'s are not assigned.  *
     H*                 Changed key for 2nd view to co#, ss#, fnd,    *
     H*                 emp#, div.                                    *
     H*                                                               *
     H*  03/17/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED FIELD OOSTAT TO OOEXFL TO CORRECT      *
     H*                F17=SUBSET PROBLEM.                            *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
     H*****************************************************************
     FW1058F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWMOOP     IF   E             DISK
     F                                     RENAME(POO01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWMOOL4    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(POO01:DBRCDF)
     F*
     F*  Secondary logical file definition (for displaying items)
      *** BEG CHG **** 01/07/02 ************************************************
     F**************** WMOOL5  IF  E           K        DISK
     FWMOOL10   IF   E           K DISK
      *** END CHG **** 01/07/02 ************************************************
     F                                     INFDS(DBFDS2)
     F                                     RENAME(POO01:DBRCDF2)
     FWMEML     IF   E           K DISK
     F                                     INFDS(DBFDSE)
     FWMALL     IF   E           K DISK
     F                                     INFDS(DBFDSA)
      /COPY SCOPYWW01D
      ***  Begin Add  ***  06/09/20  ****************************
     d/copy sPrMask
     d/copy sPrSecure
      ***  End   Add  ***  06/09/20  ****************************
     D*
     D HD              S             75    DIM(10) CTDATA PERRCD(1)             Column headings
     D OF              S              1    DIM(9) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(9) ALT(OF)
     D INF             S              1    DIM(31)                              F9 INFO LINE
     D NM1             S              1    DIM(80)
     D NM2             S              1    DIM(80)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D DBFDSE          DS
     D  DBRRNE               397    400B 0
     D DBFDSA          DS
     D  DBRRNA               397    400B 0
     D                 DS
     D  XXOSS#                 1     11
     D  XXNUM1                 1      3
     D  XXHYP1                 4      4
     D  XXNUM2                 5      6
     D  XXHYP2                 7      7
     D  XXNUM3                 8     11
     D*
     D                 DS
     D  OOOSS#                 1      9
     D  X1NUM1                 1      3
     D  X1NUM2                 4      5
     D  X1NUM3                 6      9
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Owners/Officers -
     D                                     all Employers')
     D SLMSGI          C                   CONST('WC39998')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D HI              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D NOSS#           C                   CONST('-----------')
     D NODATE          C                   CONST('----------')
     D NOTITL          C                   CONST('----------------
     D                                     -----')
     D NOENAM          C                   CONST('----------------
     D                                     ---------------')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W1057R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(5)
     D VIEWMX          C                   CONST(2)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCEMRR        ##RRN2            9 0
     C                   PARM      SCELFL        ##ELFL            1
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
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKOLSN
     C                   KFLD                    KKOFSN
     C*
     C     KEY3          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKOSS#
     *** BEG ADD **** 01/07/02 ************************************************
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     *** END ADD **** 01/07/02 ************************************************
     C*
     C     EMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C*
     C     ALKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKELID
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    OOOLSN        KKOLSN
     C     *LIKE         DEFINE    OOOFSN        KKOFSN
     C     *LIKE         DEFINE    OOOLSN        U1PDSC
     C     *LIKE         DEFINE    OOOLSN        U2PDSC
     C     *LIKE         DEFINE    OOOFSN        U3PDSC
     C     *LIKE         DEFINE    OOOFSN        U4PDSC
     C     *LIKE         DEFINE    OOOSS#        KKOSS#
     C     *LIKE         DEFINE    OOFND         KKFND
     C     *LIKE         DEFINE    OOEMP#        KKEMP#
     C     *LIKE         DEFINE    OODIV         KKDIV
     C     *LIKE         DEFINE    OOELID        KKELID
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHGNUM - SUBROUTINE TO CHANGE NUMBERS TO ALPHA WITHOUT ZEROS
     C*===============================================================
     C*
     C     CHGNUM        BEGSR
     C*
     C                   CLEAR                   NM1
     C*
     C     '0'           CHECK     XXNUM         XXZERO            3 0
     C     ' '           CHECKR    XXNUM         XXLEN             3 0
     C     XXZERO        SUB       1             XXBLNK            3 0
     C                   MOVEA     XXNUM         NM1
     C*
     C                   DO        XXBLNK        X                 3 0
     C                   MOVE      ' '           NM1(X)
     C                   ENDDO                                                  X->XXBLNK
     C*
     C                   CLEAR                   NM2
     C*
     C     EDTMSK        IFEQ      'DATE'
     C*
     C                   MOVEL(P)  XXNUM         XX10A            10
     C*
     C     XX10A         IFNE      *ZERO
     C     XX10A         ANDNE     *BLANK
     C*
     C                   Z-ADD     1             W                 3 0
     C*
     C                   DO        10            X
     C*
     C     X             IFEQ      3
     C     X             OREQ      6
     C*
     C                   MOVEL     '/'           NM2(X)
     C*
     C                   ELSE
     C*
     C                   MOVEL     NM1(W)        NM2(X)
     C                   ADD       1             W
     C*
     C                   ENDIF                                                  X=3/6
     C*
     C     X             IFEQ      1
     C     NM2(X)        ANDEQ     *BLANK
     C*
     C                   MOVEA     '0'           NM2(X)
     C*
     C                   ENDIF
     C*
     C                   ENDDO                                                  X->10
     C                   ENDIF                                                  NM1 <> BLANK
     C*
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHGNUM
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
     C*
B01  C     SSEXFL        IFNE      *BLANKS
     C     SSOLSN        ORNE      *BLANKS
     C     SSOFSN        ORNE      *BLANKS
     C     SSOSS#        ORNE      *BLANKS
     C     SSFND         ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM MDYY TO CYMD
     C*================================================================
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C     ERRCK1        BEGSR
     C                   ENDSR
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C     ERRCK2        BEGSR
     C*
     C     SSEXFL        IFNE      ' '
     C     SSEXFL        ANDNE     'I'
     C     SSEXFL        ANDNE     'E'
     C                   SETON                                        8990
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C     INZSST        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C     KEYINZ        BEGSR
     C   21              MOVE      *LOVAL        KKOLSN
     C   21              MOVE      *LOVAL        KKOFSN
     C   22              MOVE      *LOVAL        KKOSS#
     *** BEG ADD **** 01/07/02 ************************************************
     C   22              MOVE      *LOVAL        KKFND
     C   22              MOVE      *LOVAL        KKEMP#
     C   22              MOVE      *LOVAL        KKDIV
     *** END ADD **** 01/07/02 ************************************************
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C   21              MOVEL     SCPST1        KKOLSN
     C   21              MOVEL     SCPSTF        KKOFSN
     C   22              MOVEL     SCPST2        KKOSS#
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C   21              MOVEL     SCOLSN        KKOLSN
     C   21              MOVEL     SCOFSN        KKOFSN
     C   22              MOVEL     SCOSS#        KKOSS#
      *** BEG CHG **** 01/07/02 ************************************************
     C   22              Z-ADD     OOFND         KKFND
     C   22              Z-ADD     OOEMP#        KKEMP#
     C   22              Z-ADD     OODIV         KKDIV
      *** END CHG **** 01/07/02 ************************************************
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL     OOOLSN        SCOLSN
     C                   MOVEL     OOOFSN        SCOFSN
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****              MOVEL     OOOSS#        SCOSS#
      /Free
       SCOSS# = DisplaySS#(MaskData('SSN':'OOOSS#':OOOSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C*
     C                   MOVE      OOFND         KKFND
     C                   MOVE      OOEMP#        KKEMP#
     C                   MOVE      OODIV         KKDIV
     C                   MOVEL     OOELID        KKELID
     C*
     C                   CLEAR                   SCEMRR
     C                   CLEAR                   SCELFL
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C                   Z-ADD     DBRRNE        SCEMRR
     C*
     C     ALKY          CHAIN     PAL01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   Z-ADD     DBRRNA        SCEMRR
     C                   MOVE      ALELTP        SCELFL
     C                   ENDIF                                                  *IN99=*OFF
     C*
     C     OOOSS#        IFNE      *BLANKS
     C*
      *****  Begin Chg  ***  06/09/20  ***********************
     C*****              MOVEL     X1NUM1        XXNUM1
     C*****              MOVEL     '-'           XXHYP1
     C*****              MOVEL     X1NUM2        XXNUM2
     C*****              MOVEL     '-'           XXHYP2
     C*****              MOVEL     X1NUM3        XXNUM3
      /Free
       XXOSS# = DisplaySS#(MaskData('SSN':'OOOSS#':OOOSS#));
      /End-Free
      *****  End Chg  ***  06/09/20  ***********************
     C*
     C                   ELSE
     C*
     C                   MOVEL     NOSS#         XXOSS#
     C*
     C                   ENDIF
     C*
     C                   MOVEL     OOOLSN        XXOLSN           15
     C*
     C                   MOVEL     XXOLSN        XXDATA           31
     C                   MOVE      OOOFSN        XXDATA
     C*
     C* F9=CHANGE INFO
     C*                                                                 -
     C                   Z-ADD     1             A                 3 0
     C                   MOVE      *BLANK        SCDATA
     C                   CLEAR                   INF
     C*
     C** INFO# 1 = FND, EMPLOYER, AND DIVISION.                        US,
     C*
     C                   SELECT
     C     INFO#         WHENEQ    1
     C*
     C                   Z-ADD     2             A
     C                   MOVEL(P)  OOFND         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
     C*
     C                   Z-ADD     8             A
     C                   MOVEL(P)  OOEMP#        XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
     C*
     C                   Z-ADD     20            A
     C                   MOVEL(P)  OODIV         XXNUM
     C                   EXSR      CHGNUM
     C                   MOVEA     NM1           INF(A)
     C*
     C** INFO# 2 = EMPLOYER NAME.
     C*
     C     INFO#         WHENEQ    2
     C*
     C     EMKY          CHAIN     PEM01                              99
     C*
     C  N99              MOVEL     EMNAME        XXENAM           30
     C*
     C  N99              MOVEA     XXENAM        INF(A)
     C   99              MOVEA     NOENAM        INF(A)
     C*
     C** INFO# 3 = ENTITY/LOCATION# AND ENTITY/LOCATION NAME.
     C*
     C     INFO#         WHENEQ    3
     C*
     C                   MOVEA     OOELID        INF(A)
     C*
     C     ALKY          CHAIN     PAL01                              99
     C*
     C                   Z-ADD     12            A
     C  N99              MOVEL     ALNAME        XXANAM           20
     C  N99              MOVEA     XXANAM        INF(A)
     C*
     C** INFO# 4 = EFFECTIVE DATE, EXPIRATION DATE AND INC/EXC FLAG.
     C*
     C     INFO#         WHENEQ    4
     C*
     C     OOEFDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     OOEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEFDT            8 0
     C*
     C                   MOVEL(P)  XXEFDT        XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C*
     C                   ELSE
     C*
     C                   MOVEA     NODATE        INF(A)
     C*
     C                   ENDIF
     C*
     C     OOEXDT        IFNE      *ZEROS
     C*
     C                   Z-ADD     OOEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXEXDT            8 0
     C*
     C                   Z-ADD     13            A
     C                   MOVEL(P)  XXEXDT        XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     13            A
     C                   MOVEA     NODATE        INF(A)
     C*
     C                   ENDIF
     C*
     C                   Z-ADD     26            A
     C                   MOVEL     OOEXFL        INF(A)
     C*
     C** INFO# 5 = TITLE AND BIRTH DATE.
     C*
     C     INFO#         WHENEQ    5
     C*
     C                   MOVEL     OOTITL        XXTITL           20
     C*
     C     XXTITL        IFNE      *BLANKS
     C*
     C                   MOVEA     XXTITL        INF(A)
     C*
     C                   ELSE
     C*
     C                   MOVEA     NOTITL        INF(A)
     C*
     C                   ENDIF
     C*
     C     OODOB         IFNE      *ZEROS
     C*
     C                   Z-ADD     OODOB         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         XXDOB             8 0
     C*
     C                   Z-ADD     22            A
     C                   MOVEL(P)  XXDOB         XXNUM            80
     C                   MOVEL(P)  'DATE'        EDTMSK           10
     C                   EXSR      CHGNUM
     C                   MOVEA     NM2           INF(A)
     C*
     C                   ELSE
     C*
     C                   Z-ADD     22            A
     C                   MOVEA     NODATE        INF(A)
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C* F11=CHANGE VIEW
     C*                                                                 -
     C                   MOVE      *BLANK        XX43A            43
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C*
     C                   MOVEL(P)  XXDATA        XX43A
     C                   MOVE      XXOSS#        XX43A
     C*
     C     VIEW#         WHENEQ    2
     C*
     C                   MOVEL(P)  XXOSS#        XX43A
     C                   MOVE      XXDATA        XX43A
     C*
     C                   ENDSL
     C*
     C                   MOVEL(P)  XX43A         SCDATA
     C                   MOVEA     INF           XX31A            31
     C                   MOVE      XX31A         SCDATA
     C*
     C                   ENDSR
     C*================================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKI         CASEQ     *ON           KIDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C     PGMEND        BEGSR
     C*
     C*
     C                   ENDSR
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1          READE     DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1          READPE    DBRCDF2                                95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY2          SETGT     DBRCDF                             96
     C   22KEY3          SETGT     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY2          SETLL     DBRCDF                             96
     C   22KEY3          SETLL     DBRCDF2                            96
     C                   ENDSR
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C     SLCTRD        BEGSR
     C*
     C                   MOVEL     SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*===============================================================
     C* STRNG1 - SUBROUTINE TO SEARCH LAST NAMES
     C*===============================================================
     C*
     C     STRNG1        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U1PDSC
     C                   PARM      30            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U2PDSC
     C                   PARM      30            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRES            3 0
     C*
     C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRES=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*===============================================================
     C* STRNG2 - SUBROUTINE TO SEARCH FIRST NAMES
     C*===============================================================
     C*
     C     STRNG2        BEGSR
     C*
     C                   CALL      'QCLSCAN'
     C                   PARM                    U3PDSC
     C                   PARM      15            STRLEN            3 0
     C                   PARM      1             START             3 0
     C                   PARM                    U4PDSC
     C                   PARM      15            PATLEN            3 0
     C                   PARM      '0'           TRANS             1
     C                   PARM      '1'           TRIM              1
     C                   PARM      ' '           WILD              1
     C                   PARM                    SCNRS             3 0
     C*
     C     SCNRS         IFEQ      0
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SCNRS=0
     C*
     C                   ENDSR                                                  STRING
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C*
B01  C     SSOLSN        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  OOOLSN        U1PDSC
     C     LO:HI         XLATE(P)  SSOLSN        U2PDSC
     C*
     C                   EXSR      STRNG1
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
B01  C     SSOFSN        IFNE      *BLANKS
     C*
     C     LO:HI         XLATE(P)  OOOFSN        U3PDSC
     C     LO:HI         XLATE(P)  SSOFSN        U4PDSC
     C*
     C                   EXSR      STRNG2
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C     SSEXFL        IFNE      *BLANKS
     C*
     C*** BEGIN CHG *** 03/17/98 **************************************
     C********** SSEXFL    ANDNEOOSTAT
     C     SSEXFL        ANDNE     OOEXFL
     C*** END   CHG *** 03/17/98 **************************************
     C*
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSOSS#        IFNE      *BLANKS
     C     SSOSS#        ANDNE     OOOSS#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     OOFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
     C*
     C                   ENDSR
** Column headings
Last name       First name      SS#          Grp   Employer#     Div
SS#         Last name       First name       Grp   Employer#     Div
Last name       First name      SS#         Employer name
SS#         Last name       First name      Employer name
Last name       First name      SS#         Ent/Loc    Ent/Loc name
SS#         Last name       First name      Ent/Loc    Ent/Loc name
Last name       First name      SS#         Eff. date   Exp. date   I/E
SS#         Last name       First name      Eff. date   Exp. date   I/E
Last name       First name      SS#         Title                DOB
SS#         Last name       First name      Title                DOB
** Description--------------MISN
1011=Select                 NNY
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
209F9=Change info           YYY
211F11=Change view          YYY
212F12=Cancel               NNY
217F17=Subset               YYY
3
