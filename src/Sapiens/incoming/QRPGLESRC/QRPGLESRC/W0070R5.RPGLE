     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
     H/define CONVERT
     H*
     H/TITLE W0070R5 - EDI/NOI INFORMATION MAINTENANCE
     H*
     H*****************************************************************
     H*
     H*  CREATED: 06/18/97  for Version 3 BASE
     H*                    C:58  PROJECT: WC468
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*  06/09/20  C:318 Project: SY420
     H*                  Mask SSN
     H*                                                               *
     H*  06/09/14 C:180 SUPPORT WC136 85787  Increase size of DSPF    *
     H*                 variable SCMO to 5,0 - DSPF changes only.     *
     H*                                                               *
     H*  04/05/12 C:255 Project WC2087                                *
     H*                 Add changes for the fields added in display   *
     H*                 file on SCR08 for Medical only.               *
     H*                                                               *
     H*  04/22/10 C:247 SUPPORT 70202 - Claim type is displayed       *
     H*                 incorrectly on EDI screen (SCR08) when type   *
     H*                 is changed from M to I on accident info screen*
     H*                                                               *
     H*  03/24/10 C:232 Project: 139WNN                               *
     H*                 Update programs to use the new EDI Control API*
     H*                 WA076R instead of using WMIXP.                *
     H*                                                               *
     H*  03/23/10 C:235 Project: 139WNF                               *
     H*                 EDI SCREEN DESIGN                             *
     H*                                                               *
     H*  11/04/09 C:180 SUPPORT 70488
     H*                 Increase Impairment % field size to 5/2
     H*                 DSPF changes only.
     H*
     H*  04/20/09 C:190 Project: WC1473                               *
     H*                 Use the new logical(s) to get the description *
     H*                 by extension.                                    *
     H*                                                               *
     H* 04/07/09   C:189 SUPPORT 66028 - Remove F19 functionlity and  *
     H*                                    adjust keys                *
     H*                                                               *
     H*  10/19/07 C:191 Project: WC1584                               *
     H*                 EDI Release 3: Add new function keys          *
     H*                                                               *
     H*  05/25/07 C:191 PROJECT: WC1515                               *
     H*                 Check claims authority                        *
     H*                                                               *
     H* 05/14/07 C:191 Support: 58155                                 *
     H*                Add fund to call W0080R1 to fix an error with  *
     H*                A49 correction/change when claim fund is diff  *
     H*                from user params fund                          *
     H*                                                               *
     H* 05/01/07 C:185 Project:WC1177                                 *
     H*                EDI Release 3                                  *
     H*                                                               *
     H* 02/19/07 C:53  Support: 57213                                 *
     H*                Add SIC/NAICS changes to the display           *
     H*                                                               *
     H* 10/18/06 C:53  Support: 55398                                 *
     H*                Condition tpa field load on tpa flag           *
     H*                                                               *
     H* 06/01/06 C:65  PROJECT WC945                                  *
     H*                CHAIN FOR PAYROLL EXTENSION DESCRIPTION USING  *
     H*                EFFECTIVE DATE.                                *
     H*                                                               *
     H*  05/18/06 C:132 Support: WC132/52731                          *
     H*               - Changed Command key F18 to not display when   *
     H*                 in display mode. Also changed the screen to   *
     H*                 add indicator on F18 display line, and to not *
     H*                 function if pressed on screen if in display   *
     H*                 mode.                                         *
     H*                                                               *
     H* 04/24/06 C:53 Support: 52440                                  *
     H*               Remove EMDBA from processing. This field is no  *
     H*               longer used in base.                            *
     H*                                                               *
     H* 11/07/03 C:53 Project: WC1141                                 *
     H*               Add F18=A49 Correction/change                   *
     H*                                                               *
     H* 08/06/01 C:15 Support: WC048/28506                            *
     H*               Corrected array index error.                    *
     H*                                                               *
     H* 11/29/00 C:15 Support: WC082/25222                            *
     H*               Changed a date work field from 6.0 to 7.0 to    *
     H*               prevent invalid dates from displaying.          *
     H*                                                               *
     H* 12/08/99 C:15 Project: WC468                                  *
     H*               Changed the Log History search pgm to the new   *
     H*               version which allows the user to update the     *
     H*               log record.                                     *
     H*                                                               *
     H* 11/29/99 C:118 PROJECT: WC577                                 *
     H*                Made V3 changes to display file.               *
     H*                                                               *
     H* 10/06/99 C:90 PROJECT: WC945                                  *
     H*                FIND VALID EXTENSION WITH STATE EQUAL TO BLANK *
     H*                IF NOT VALID, THEN FIND VALID EXT WITH STATE   *
     H*                EQUAL TO THE RATING STATE.                     *
     H*                                                               *
     H* 11/10/98 C:15 Support: WC096/14948                            *
     H*               Added an indicator to the call of the state     *
     H*               specific screen in the case that the pgm does   *
     H*               not exist.                                      *
     H*                                                               *
     H* 09/22/98 C:15 Project: WC468                                  *
     H*               Added code to load the claim type.              *
     H*                                                               *
     H*                                                               *
     H* 08/12/98 C:65 Project: WC843                                  *
     H*               USE EXTENSION DESCRIPTION IF SPECIFIED.         *
     H*
     H*  07/23/98 C:33 SUPPORT: PER TLB
     H*                REMOVE PARM ON CALL TO STATE PROGRAMS
     H*
     H*  05/22/98 C:58 PROJECT: WC468
     H*               -Various corrections and rearrangements
     H*               -Added WDF2L to get Group FEIN
     H*
     H*  11/12/97 C:58 PROJECT: WC468
     H*               -Add screens, reduce crowding, per SAA standards;
     H*                load all display data in initialization subroutine.
     H*
     H*  10/30/97 C:58 PROJECT: WC468
     H*               -Added Co and Fund to W0270R5 parameters.
     H*
     H*****************************************************************
     H* INDICATOR USAGE:
     H*
     H*    06  CLAIM KEY PRESENT -- GO TO SCR02 DISPLAY
     H*   N06  CLAIM KEY NEEDED: DO SCR01 TO SELECT
     H*
     H*    69  EXCESS REINSURANCE
     H*
     H*****************************************************************
     H*
     FW0070F5   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     INFDS(FIDS01)
     FSMCOL     IF   E           K DISK
     FSSYLF     IF   E           K DISK
     FWMPXL1    IF   E           K DISK
     FWMEPL7E   IF   E           K DISK
     FWMFNL     IF   E           K DISK
     F                                     INFDS(DBFDS1)
     FWMUPL     IF   E           K DISK
     FWMCML1    IF   E           K DISK
     F                                     INFDS(FIDS02)
     FWMCMP     IF   E             DISK
     F                                     RENAME(PCM01:PCM02)
     FWMLLL     IF   E           K DISK
     FWMLGL     IF   E           K DISK
     FWMEML     IF   E           K DISK
     FWMALL     IF   E           K DISK
     FWDELL     IF   E           K DISK
     FWDACL     IF   E           K DISK
     FWMCDL     IF   E           K DISK
     FWMCHL     IF   E           K DISK
     FWMTHL     IF   E           K DISK
     F*
     F*     Group auxiliary data
     FWDF2L     IF   E           K DISK
     F*
     F*  PAYROLL CLASS CODE FILE
     FWMPRL     IF   E           K DISK
     FWTABL     IF   E           K DISK
     F                                     IGNORE(PAT01)
     F                                     IGNORE(PSP01)
     F                                     IGNORE(PNC01)
     F                                     IGNORE(PRC01)
     F                                     IGNORE(PRT01)
     F                                     IGNORE(PPC01)
     F                                     IGNORE(PB201)
     F                                     IGNORE(PB701)
     F                                     IGNORE(PBS01)
     F                                     IGNORE(PTT01)
     F                                     IGNORE(PCC01)
     F                                     IGNORE(PBC01)
     F                                     IGNORE(PCS01)
     F                                     IGNORE(PCN01)
     F                                     IGNORE(PRS01)
     F                                     IGNORE(PLR01)
     F                                     IGNORE(PVS01)
     F                                     IGNORE(PTR01)
     F                                     IGNORE(PHY01)
     F                                     IGNORE(PSY01)
     F                                     IGNORE(PBA01)
     F                                     IGNORE(PID01)
     F                                     IGNORE(PIS01)
     FWTGTL     IF   E           K DISK
     FWTDCL     IF   E           K DISK
     FWMRSL     IF   E           K DISK
     F*
     D*________________________________________________________________
     D CF              S              1    DIM(99)
     D*================================================================
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D FIDS02          DS
     D  DBRRN                397    400B 0
     D DBFDS1          DS
     D  DBRRN1               397    400B 0
     D/COPY SCOPYMT01I
     D                 DS
     D  CMACTM                 1      6  0
     D  XXACTM                 1      4  0
     D                 DS
     D  SCAZIP                 1      9  0
     D  SCZIP1                 1      5  0
     D  SCZIP2                 6      9  0
     D*
     D                 DS
     D  SCFD01                 1      4
     D  SCFD02                 5      8
     D  SCFD03                 9     12
     D  SCFD04                13     16
     D  SCFD05                17     20
     D  SCFD06                21     24
     D  SCFD07                25     28
     D  SCFD08                29     32
     D  SCFD09                33     36
     D  SCFD10                37     40
     D  SCFD11                41     44
     D  SCFD12                45     48
     D  SCFD13                49     52
     D  SCFD14                53     56
     D  SCFD15                57     60
     D  SCFD16                61     64
     D  SCFD17                65     68
     D  SCFD18                69     72
     D  SCFD19                73     76
     D  SCFD20                77     80
     D  SCFD21                81     84
     D  SCFD22                85     88
     D  SCFD23                89     92
     D  SCFD24                93     96
     D  SCFD25                97    100
     D  SCFD26               101    104
     D  SCFD27               105    108
     D  SCFD28               109    112
     D  SCFD29               113    116
     D  SCFD30               117    120
     D  SCFD31               121    124
     D  SCFD32               125    128
     D  SCFD33               129    132
     D  SCFD34               133    136
     D  SCFD35               137    140
     D  SCFD36               141    144
     D  SCFD37               145    148
     D  SCFD38               149    152
     D  SCFD39               153    156
     D  SCFD40               157    160
     D  SCFD41               161    164
     D  SCFD42               165    168
     D  SCFD43               169    172
     D  SCFD44               173    176
     D  SCFD45               177    180
     D  SCFD46               181    184
     D  SCFD47               185    188
     D  SCFD48               189    192
     D  SCFD49               193    196
     D  SCFD50               197    200
     D  SCFD51               201    204
     D  SCFD52               205    208
     D  SCFD53               209    212
     D  SCFD54               213    216
     D  SCFD55               217    220
     D  SCFD56               221    224
     D  SCFD57               225    228
     D  SCFD58               229    232
     D  SCFD59               233    236
     D  SCFD60               237    240
     D  SCFD61               241    244
     D  SCFD62               245    248
     D  SCFD63               249    252
     D  SCFD64               253    256
     D  SCFD65               257    260
     D  SCFD66               261    264
     D  SCFD67               265    268
     D  SCFD68               269    272
     D  SCFD69               273    276
     D  SCFD70               277    280
     D  SCFD71               281    284
     D  SCFD72               285    288
     D  SCFD73               289    292
     D  SCFD74               293    296
     D  SCFD75               297    300
     D  SCFD76               301    304
     D  SCFD77               305    308
     D  SCFD78               309    312
     D  SCFD79               313    316
     D  SCFD80               317    320
     D  SCFD81               321    324
     D  SCFD82               325    328
     D  SCFD83               329    332
     D  SCFD84               333    336
     D  SCFD85               337    340
     D  SCFD86               341    344
     D  SCFD87               345    348
     D  SCFD88               349    352
     D  SCFD89               353    356
     D  SCFD90               357    360
     D  SCFD91               361    364
     D  SCFD92               365    368
     D  SCFD93               369    372
     D  SCFD94               373    376
     D  SCFD95               377    380
     D  SCFD96               381    384
     D  SCFD97               385    388
     D  SCFD98               389    392
     D  SCFD99               393    396
     D  FD                     1    396
     D                                     DIM(99)
      /If Not Defined (WEB)
     D OPTNM           C                   CONST('Claims, EDI/NOI I-
     D                                     nformation Mainte-
     D                                     nance')
      /Endif
      /If Defined (WEB)
     D OPTNM           C                   CONST('EDI/NOI Information -
     D                                     Maintenance')
      /Endif
     D XCEXRE          C                   CONST('EXCS REIN ')
     D XCBC01          C                   CONST('FILE STATUS IS OUT, -
     D                                     USER - XXXXXXXXXX')
     D XCBC02          C                   CONST('FILE STATUS IS IN')
      ** Beg Add *** 04/05/12 *****************************************
     D IXmltd          s              3  0
      ** End Add *** 04/05/12 *****************************************
      ***  Begin Add  ***  06/09/20  *******************
     D/copy sPrSecure
     D/copy sPrMask
      ***  End   Add  ***  06/09/20  *******************
     C*
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C                   GOTO      SCR01T
     C                   ENDIF
     C*
     C     STRT1         TAG
     C*
      * If Inquiry parm passed set on *IN09 to not display F18 command
     C     QQCALL        IFEQ      '1'
     C                   MOVE      *ON           *IN09
     C                   ELSE
     C                   MOVE      *OFF          *IN09
     C                   ENDIF
     C*
     C*   Retrieve the User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C  N60              MOVE      UPFND         XXFND
     C*
     C*   Retrieve Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   MOVE      *BLANKS       COADR1
     C                   MOVE      *BLANKS       COADR2
     C                   MOVE      *BLANKS       COCTY
     C                   MOVE      *BLANKS       COST
     C                   MOVE      *ZEROS        COZIP
     C                   MOVE      *ZEROS        COFEIN
     C                   ENDIF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   Retrieve Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *BLANKS       FNSFIN
     C                   ENDIF
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C*   Display Formats
     C*
     C     STRT          TAG
     C*
     C                   MOVE      *OFF          @RDFG
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR01'
     C                   EXFMT     SCR01
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C     @SF02         CASEQ     *OFF          SRLSC2
     C                   ENDCS
     C                   EXFMT     SCR02
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C     @SF03         CASEQ     *OFF          SRLSC3
     C                   ENDCS
     C                   EXFMT     SCR03
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C     @SF04         CASEQ     *OFF          SRLSC4
     C                   ENDCS
     C                   EXFMT     SCR04
     C*
     C     Q2RECF        WHENEQ    'SCR05'
     C     @SF05         CASEQ     *OFF          SRLSC5
     C                   ENDCS
     C                   EXFMT     SCR05
     C*
     C     Q2RECF        WHENEQ    'SCR06'
     C     @SF06         CASEQ     *OFF          SRLSC6
     C                   ENDCS
     C                   EXFMT     SCR06
     C*
     C     Q2RECF        WHENEQ    'SCR07'
     C                   EXFMT     SCR07
     C*
     C     Q2RECF        WHENEQ    'SCR08'
     C                   EXFMT     SCR08
     C*
     C                   OTHER
     C                   GOTO      TERM
     C*
     C                   ENDSL
     C*
     C*   Process Command Keys
     C*
     C     *IN11         IFEQ      *ON
     C*
     C                   SELECT
     C*
     C*   F3=Exit
     C*
     C     *INKC         WHENEQ    *ON
     C                   MOVE      'A'           QQRTCD
     C                   GOTO      TERM
     C*
     C*   F12=Cancel
     C*
     C     *INKL         WHENEQ    *ON
     C*
     C                   SELECT
     C*
     C     Q2RECF        WHENEQ    'SCR02'
     C*
     C     *IN06         IFEQ      *ON
     C                   GOTO      TERM
     C                   ENDIF
     C*
     C                   MOVEL     'SCR01   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR03'
     C                   MOVEL     'SCR02   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR04'
     C                   MOVEL     'SCR03   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR05'
     C                   MOVEL     'SCR04   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR06'
     C                   MOVEL     'SCR05   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR07'
     C                   MOVEL     'SCR06   '    Q2RECF
     C*
     C     Q2RECF        WHENEQ    'SCR08'
     C                   MOVEL     'SCR07   '    Q2RECF
     C*
     C                   OTHER
     C*
     C                   ENDSL
     C*
     C                   GOTO      STRT
     C*
     C*   CMD 10 - Search
     C*
     C     *INKJ         WHENEQ    *ON
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C*
     C                   CALL      'W0070R'
     C                   PARM      '2'           @@CALL            1
     C     *INKC         PARM                    @@KC              1
     C     *INKL         PARM                    @@KL              1
     C                   PARM                    @@MODE            1
     C                   PARM      *ZEROS        @@RRN             9 0
     C                   PARM      'CN'          @@SORT            2
     C                   PARM      DBRRN1        @@RRN2            9 0
     C                   PARM      SCYR          @@FYR             3 0
     C*
     C     *INKC         CABEQ     *ON           TERM
     C*
     C     *INKL         IFEQ      *ON
     C     @@RRN         OREQ      *ZEROS
     C                   GOTO      STRT
     C                   ELSE
     C*
     C     @@RRN         CHAIN     WMCMP                              60
     C*
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   Z-ADD     0             @@RRN
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C*
     C                   MOVE      CMFYR         SCYR
     C                   MOVE      CMCASE        SCCASE
     C*
     C                   MOVE      *ZEROS        QQEMP#
     C                   MOVE      *ZEROS        QQDIV
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   GOTO      SCR01T
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*
     C*   CMD 11 - EDI Log History
     C*
     C     *INKK         WHENEQ    *ON
     C                   MOVE      *ON           @RDFG
     C*
     C     Q2RECF        IFNE      'SCR01'
     C*
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      XXFND         QQFND
     C                   MOVE      XXCO#         QQCO#
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'W0875R'
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    QQCALL
     C                   PARM      ' '           ##MODE            1
     C                   PARM                    XXCO#
     C                   PARM                    XXFND
     C                   PARM                    SCYR
     C                   PARM                    SCCASE
     C                   PARM                    ##RRN             9 0
     C*
     C     *INKC         CABEQ     '1'           TERM
     C     *INKL         CABEQ     '1'           STRT
     C*
     C                   ENDIF
     C*
     C*   CMD 15 - BODY PART MAINTENANCE
     C*
     C     *INKP         WHENEQ    *ON
     C                   MOVE      *ON           @RDFG
     C*
     C     Q2RECF        IFNE      'SCR01'
     C*
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'W0070R7'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*   CMD 16 - AVERAGE WEEKLY WAGE (AWW) SEARCH
     C*
     C     *INKQ         WHENEQ    *ON
     C                   MOVE      *ON           @RDFG
     C*
     C     Q2RECF        IFNE      'SCR01'
     C*
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'W0070R8'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*   CMD 17 - BENEFIT ADJUSTMENT MAINTENANCE
     C*
     C     *INKR         WHENEQ    *ON
     C                   MOVE      *ON           @RDFG
     C*
     C     Q2RECF        IFNE      'SCR01'
     C*
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      XXFND         QQFND
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      'W0070R9'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*
     C*   CMD 18 - A49 CORRECTION/CHANGE
     C*
     C     *INKS         WHENEQ    *ON
     C                   MOVE      *ON           @RDFG
     C*
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      XXFND         QQFND
     C*
     C                   CALL      'W0080R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQFND
     C                   PARM      *BLANKS       QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*
     C*   CMD 13 - Change User Parameters
     C*
     C     *INKM         WHENEQ    *ON
     C                   CALL      'W0006R'
     C                   GOTO      STRT1
     C*
     C*   F19=Weekly Amounts
     C*
     C*   F20=Reduced Earnings
     C*
     C     *INKU         WHENEQ    *ON
     C     Q2RECF        IFEQ      'SCR02'
     C*
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      XXFND         QQFND
     C*
     C                   CALL      'W0070R1'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*   F21=Concurrent Employer
     C*
     C     *INKV         WHENEQ    *ON
     C     Q2RECF        IFEQ      'SCR02'
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      XXFYR         QQFYR
     C                   MOVE      CMCASE        QQCASE
     C                   MOVE      XXFND         QQFND
     C*
     C                   CALL      'W0070R1A'
     C                   PARM                    QQPRDS
     C                   PARM                    QQCALL
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C*
     C                   ENDIF
     C*
     C*   F24=More Keys
     C*
     C     *INKY         WHENEQ    *ON
     C     Q2RECF        IFEQ      'SCR02'
     C     *IN71         IFEQ      *OFF
     C                   SETON                                        71
     C                   ELSE
     C                   SETOFF                                       71
     C                   ENDIF                                                  *IN71=*OFF
     C                   ENDIF                                                  Q2RECF=SCR02
     C                   GOTO      STRT
     C*
     C                   OTHER
     C*
     C                   GOTO      STRT
     C*
     C                   ENDSL
     C*
     C     @RDFG         CABEQ     *ON           STRT
     C*
     C                   ENDIF                                                  END CMD KEYS
     C*
      **************************************************************
     C*
     C*   Execute Appropriate Screen Routine
     C*
     C     Q2RECF        IFEQ      'SCR01'
     C                   MOVE      SCYR          QQFYR
     C                   MOVE      SCCASE        QQCASE
     C                   GOTO      SCR01T
     C                   ENDIF
     C*
     C     Q2RECF        CABEQ     'SCR02'       SCR02T
     C     Q2RECF        CABEQ     'SCR03'       SCR03T
     C     Q2RECF        CABEQ     'SCR04'       SCR04T
     C     Q2RECF        CABEQ     'SCR05'       SCR05T
     C     Q2RECF        CABEQ     'SCR06'       SCR06T
     C     Q2RECF        CABEQ     'SCR07'       SCR07T
     C     Q2RECF        CABEQ     'SCR08'       SCR08T
     C*
     C*   Process Prompt Screen
     C*
     C     SCR01T        TAG
     C*
     C                   MOVE      SCYR          XXFYR
     C                   MOVE      SCCASE        XXCASE
     C     CMKY          CHAIN     PCM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF
      *
     C*** Call claims security API to check claim authority
     C                   MOVE      CMCO#         ##CO#
     C                   MOVE      CMFND         ##FND
     C                   MOVE      CMFYR         ##FYR
     C                   MOVE      CMCASE        ##CASE
     C                   MOVE      *BLANKS       ##RTCD
     C*
     C                   CALL      'WA067R'      CUSPL
     C     ##RTCD        IFEQ      '2'
     C*** Not Authorised when RTCD = '2'
     C                   MOVE      *ON           *IN21
     C                   GOTO      STRT
     C                   ENDIF                                                     RTCD = '2'
     C* ---------------------------------------------------------------
     C*  SET UP EXCESS REIN DISPLAY
     C*
     C     CMEXCS        IFNE      'Y'
     C                   Z-ADD     DBRRN         ##RRN
     C                   CALL      'WA010R'
     C                   PARM                    ##RRN             9 0          FOR WMCMP
     C                   PARM                    XXFNDR           13 2
     C                   PARM                    XXEXRE            1
     C                   END
     C*
     C     XXEXRE        IFEQ      'Y'
     C     CMEXCS        OREQ      'Y'
     C                   MOVE      *ON           *IN69                          DISPLAY
     C                   ELSE
     C                   MOVE      *OFF          *IN69                          -- NOT
     C                   END
      *
     C*
     C     CMJRST        IFNE      *BLANK
     C                   MOVEL     CMJRST        SCJRST
     C                   END
     C     SCJRST        IFEQ      *BLANK
     C                   MOVE      'FL'          SCJRST
     C                   END
     C*
     C                   EXSR      SRLDB
     C                   EXSR      SRSCHD
     C*
     C*   Load all screen database data
     C                   EXSR      SRLSC2
     C                   EXSR      SRLSC3
     C                   EXSR      SRLSC4
     C                   EXSR      SRLSC5
     C                   EXSR      SRLSC6
     C*
     C                   MOVE      *ON           @SF02
     C                   MOVE      *ON           @SF03
     C                   MOVE      *ON           @SF04
     C                   MOVE      *ON           @SF05
     C                   MOVE      *ON           @SF06
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR02'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   Claims Administrator Information
     C*
     C     SCR02T        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR03'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   Insured Information
     C*
     C     SCR03T        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR04'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   Accident Information
     C*
     C     SCR04T        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR05'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   Employee Information
     C*
     C     SCR05T        TAG
     C*
     C                   MOVE      *BLANK        Q2RECF
     C                   MOVEL     'SCR06'       Q2RECF
     C                   GOTO      STRT
     C*
     C*   Employment Information
     C*
     C     SCR06T        TAG
     C                   MOVEL     'SCR07'       Q2RECF
     C                   GOTO      STRT
     C     SCR07T        TAG
     C                   MOVEL     'SCR08'       Q2RECF
     C                   GOTO      STRT
     C     SCR08T        TAG
     C*
     C*
     C*   Retrieve the State NOI program name
     C*
     C     IXFROI        IFNE      *BLANKS
     C                   MOVEL     'W0070R'      XXPGM
     C                   MOVEL     IXFROI        WKPGM             4
     C                   MOVE      WKPGM         XXPGM
     C*
     C                   MOVE      SCAZIP        QQAZIP            9 0          Acc zip code
     C*
     C                   MOVE      QQCALL        QSCALL
     C                   MOVE      QQPRDS        QSPRDS
     C                   MOVE      XXCO#         QQCO#                          COMPANY
     C                   MOVE      SCFND         QQFND                          FUND ID
     C                   MOVE      SCYR          QQFYR                          FUND YR
     C                   MOVE      SCCASE        QQCASE
     C                   MOVE      *BLANKS       QQRTCD
     C*
     C                   CALL      XXPGM         SNOIPL                 99
     C*
     C     *IN99         IFEQ      '0'
     C                   MOVE      QSCALL        QQCALL
     C                   MOVE      QSPRDS        QQPRDS
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C     QQRTCD        CABEQ     'B'           STRT
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   Call the program to handle the EDI Log Record
     C*
     C                   MOVE      XXCO#         QQCO#                          COMPANY
     C                   MOVE      SCFND         QQFND                          FUND ID
     C                   MOVE      SCYR          QQFYR                          FUND YR
     C                   MOVE      SCCASE        QQCASE                         CASE/CLAIM
     C                   MOVE      *BLANKS       QQRTCD                         RETURN FLAG
     C*
     C                   CALL      'W0070R6 '    LOGFPL
     C*
     C     QQRTCD        CABEQ     'A'           TERM
     C*
     C     QSFYR         IFEQ      *ZEROS
     C     QSCASE        ANDEQ     *ZEROS
     C                   MOVE      QSFYR         SCYR
     C                   MOVE      QSCASE        SCCASE
     C                   MOVE      *BLANKS       Q2RECF
     C                   MOVEL     'SCR01'       Q2RECF
     C                   GOTO      STRT
     C                   ENDIF
     C*
     C     TERM          TAG
     C                   EXSR      SREOJ
     C/EJECT
     C*   ROUTINE - *INZSR
     C*
     C     *INZSR        BEGSR
     C*
     C*   Define Parameter Lists
     C*
     C     *ENTRY        PLIST
     C                   PARM                    QQPRDS           40
     C                   PARM                    QQCALL            1
     C                   PARM                    QQCO#             3
     C                   PARM                    QQFND             3
     C                   PARM                    QQFYR             3
     C                   PARM                    QQCASE            7
     C                   PARM                    QQRTCD            1
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
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C*          State NOI parm list
     C*
     C     SNOIPL        PLIST
     C                   PARM                    QQCALL
     C                   PARM                    QQCO#
     C                   PARM                    QQFND
     C                   PARM                    QQFYR
     C                   PARM                    QQCASE
     C                   PARM                    QQRTCD
     C*
     C*          EDI Log Parm List
     C*
     C     LOGFPL        PLIST
     C                   PARM                    QQCO#                          COMPANY
     C                   PARM                    QQFND                          FUND ID
     C                   PARM                    QQFYR                          FUND YR
     C                   PARM                    QQCASE                         CASE/CLAIM
     C                   PARM                    QQACTF            1            ACTION FLAG
     C                   PARM                    QQMGID            7            MSG ID
     C                   PARM                    QQRTCD
     C*
     C*** Parameter list for claims security API
     C     CUSPL         PLIST
     C                   PARM                    ##CO#             3
     C                   PARM                    ##FND             3
     C                   PARM                    ##FYR             3
     C                   PARM                    ##CASE            7
     C                   PARM      Q1USER        ##USR            10
     C                   PARM                    ##RTCD            1
     C     IXAPI         PLIST
     C                   PARM                    XXCO#
     C                   PARM                    XXJRST            2
     C                   PARM                    XXTRID            3
     C                   PARM                    CMFND
     C                   PARM      *BLANKS       @RTNCD            1
     C                   PARM      *BLANKS       @EDIF             3
     C                   PARM      *BLANKS       IXELT             1
     C                   PARM      *ZEROS        IXELTD            3 0
     C                   PARM      *BLANKS       IXFROI            2
     C                   PARM      *ZEROS        IXSDUE            3 0
     C                   PARM      *BLANKS       IXRLS             1
     C                   PARM      *ZEROS        IXCSDT            7 0
     C                   PARM      *BLANKS       IXEADC            5
     C                   PARM      *BLANKS       IXHCPR            5
     C                   PARM      *BLANKS       IXFNRC            5
     C                   PARM      *BLANKS       IXHCED            1
      *
     C*
     C*================================================================
     C*   Define Key Lists
     C*================================================================
     C*
     C*     SMCOL - Company Master
     C*
     C     COKEY         KLIST
     C                   KFLD                    XXCO#
      *
      *  PAYROLL DESCRIPTION EXTENSION FILE
      *
     C     PXKY          KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
     C                   KFLD                    EPEFDT
     C     PXKY1         KLIST
     C                   KFLD                    EPCO#
     C                   KFLD                    EPCLAS
     C                   KFLD                    EPEXT
     C                   KFLD                    XXST
      *
      * RATE SET CODE FILE
      *
     C     RSKY          KLIST
     C                   KFLD                    ELCO#
     C                   KFLD                    ELRTCD
      *
      * PAYROLL FILE
      *
     C     EPKY          KLIST
     C                   KFLD                    CMCO#
     C                   KFLD                    CMFND
     C                   KFLD                    CMFYR
     C                   KFLD                    CMEMP#
     C                   KFLD                    CMDIV
     C                   KFLD                    CMCLAS
     C                   KFLD                    CMSPEC
     C                   KFLD                    CMEXT
     C*
     C*     WMFNL - Fund Master
     C*
     C     FNKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*     WMUPL - User Parameter File
     C*
     C     UPKEY         KLIST
     C                   KFLD                    Q1USER
     C*
     C*     SSYLF - System Control File
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C*     WMCML - Claim Master File
     C*
     C     CMKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WMLLP - Legal Master
     C*
     C     LLKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WMCHL - Claim Status History
     C*
     C     CHKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WMTHL - Claim Type History
     C*
     C     THKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WTABL - Work Comp Table File
     C*
     C     XTBKY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTKEY
     C*
     C*     WTGTL - Generic Table File
     C*
     C     XTKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXTBID
     C                   KFLD                    XXTKEY
     C*
     C*     WTDCL - Data Element Criteria Table
     C*
     C     DCKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCJRST
     C*
     C*     WDACL - Aux Compensation Payee File
     C*
     C     ACKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXFYR
     C                   KFLD                    XXCASE
     C*
     C*     WDF2L - Auxiliary Fund Master
     C*
     C     F2KY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C*
     C*   KEY LISTS FOR THE PAYROLL CLASS FILE
     C*
     C     PRKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCCLAS
     C                   KFLD                    XXEFDT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCCLAS
     C*
     C*
     C*   Define work fields
     C*
     C     *LIKE         DEFINE    QQCALL        QSCALL
     C     *LIKE         DEFINE    QQPRDS        QSPRDS
     C     *LIKE         DEFINE    QQFYR         QSFYR
     C     *LIKE         DEFINE    QQCASE        QSCASE
     C     *LIKE         DEFINE    GTTBID        QQTID
     C     *LIKE         DEFINE    GTKEY         QQTKEY
     C     *LIKE         DEFINE    Q1PGM         QQPGM
     C     *LIKE         DEFINE    Q1PGM         XXPGM
     C     *LIKE         DEFINE    Q1JOBN        QQJOB#
     C     *LIKE         DEFINE    Q1USER        QQUSER
     C     *LIKE         DEFINE    Q1JOB         QQJOB
     C     *LIKE         DEFINE    UPCO#         XXCO#
     C     *LIKE         DEFINE    UPFND         XXFND
     C     *LIKE         DEFINE    EMEMP#        QQEMP#
     C     *LIKE         DEFINE    EMDIV         QQDIV
     C     *LIKE         DEFINE    CMFYR         XXFYR
     C     *LIKE         DEFINE    CMCASE        XXCASE
     C     *LIKE         DEFINE    GTTBID        XXTBID
     C     *LIKE         DEFINE    GTKEY         XXTKEY
     C     *LIKE         DEFINE    *IN60         @RDFG
     C     *LIKE         DEFINE    *IN60         @SF02
     C     *LIKE         DEFINE    *IN60         @SF03
     C     *LIKE         DEFINE    *IN60         @SF04
     C     *LIKE         DEFINE    *IN60         @SF05
     C     *LIKE         DEFINE    *IN60         @SF06
     C     *LIKE         DEFINE    *IN60         @INZF
     C     *LIKE         DEFINE    CMACDT        XXEFDT
     C     *LIKE         DEFINE    PXST          XXST
     C*                                                                *********
     C                   CLEAR                   SCR01
     C                   CLEAR                   SCR02
     C                   CLEAR                   SCR03
     C                   CLEAR                   SCR04
     C                   CLEAR                   SCR05
     C                   CLEAR                   SCR06
     C*
     C*   Initialize Screen Fields
     C*
     C                   MOVE      QQCO#         XXCO#
     C                   MOVE      QQFND         XXFND
     C                   MOVE      Q1USER        SCUSER
     C                   MOVE      Q1PGM         SCPGM
     C*
     C                   MOVEL     XCEXRE        SCMSG1
     C*
     C                   MOVEL(P)  OPTNM         ##DATA
     C*
     C                   CALL      'SRCENTER'
     C                   PARM                    ##DATA           80
     C                   PARM      40            ##LENG            3 0
     C                   MOVEL     ##DATA        SCPRDS
     C*
     C                   MOVE      *BLANKS       SCMODE
     C                   MOVE      'Display'     SCMODE                         Inquiry MODE
     C                   TIME                    DSDTTM
     C*
     C*
     C*   Retrieve the User Parameter File
     C*
     C     UPKEY         CHAIN     PUP01                              60
     C     XXCO#         IFEQ      0
     C                   MOVE      UPCO#         XXCO#
     C                   MOVE      UPFND         XXFND
     C                   ENDIF
     C*
     C*   Retrieve Company Record
     C*
     C     COKEY         CHAIN     PCO01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       CONAME
     C                   MOVE      *BLANKS       COADR1
     C                   MOVE      *BLANKS       COADR2
     C                   MOVE      *BLANKS       COCTY
     C                   MOVE      *BLANKS       COST
     C                   MOVE      *ZEROS        COZIP
     C                   MOVE      *ZEROS        COFEIN
     C                   ENDIF
     C                   MOVEL(P)  CONAME        SRDFLD
     C                   CALL      'SRCENTER'
     C                   PARM                    SRDFLD           80
     C                   PARM      40            SRDLEN            3 0
     C                   MOVEL     SRDFLD        SCCONM
     C*
     C*   Retrieve Fund Record
     C*
     C     FNKEY         CHAIN     PFN01                              60
     C     *IN60         IFEQ      *ON
     C                   MOVE      *BLANKS       FNNAME
     C                   MOVE      *BLANKS       FNSFIN
     C                   ENDIF
     C                   MOVE      XXFND         SCFND
     C                   MOVEL     FNNAME        SCFNAM
     C                   MOVEL     FNNAME        SCGNAM
     C*
     C     FNNAME        IFEQ      *BLANKS
     C                   MOVEL     *ALL'*'       SCFNAM
     C                   ENDIF
     C*
     C*   Retrieve System Control Record
     C*
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C*
     C*   Initialize for Displays
     C*
     C     @INZF         IFNE      *ON
     C*
     C*          Make sure that there are no blanks in these fields.
     C*
     C     ' ':'0'       XLATE     QQFYR         QQFYR
     C     ' ':'0'       XLATE     QQCASE        QQCASE
     C*
     C                   MOVE      QQFYR         QSFYR
     C                   MOVE      QQCASE        QSCASE
     C*
     C     QQFYR         IFLE      *ZEROS
     C     QQCASE        ORLE      *ZEROS
     C                   MOVE      *OFF          *IN06
     C                   MOVEL     'SCR01'       Q2RECF
     C                   ELSE
     C                   MOVE      *ON           *IN06
     C                   MOVEL     'SCR02'       Q2RECF
     C                   ENDIF
     C*
     C                   MOVE      *ON           @INZF
     C                   ENDIF
     C*
     C                   MOVE      QQFYR         SCYR
     C                   MOVE      QQCASE        SCCASE
     C*
     C                   ENDSR
     C/EJECT
     C*
     C**************************************************************************
     C*   ROUTINE - SREOJ
     C*
     C     SREOJ         BEGSR
     C*
     C                   MOVE      QSFYR         QQFYR
     C                   MOVE      QSCASE        QQCASE
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C*   ROUTINE - SRSCHD
     C*
     C     SRSCHD        BEGSR
     C*
     C                   MOVE      Q1USER        SCUSER
     C*
     C*  ACCIDENT DATE
     C*
     C     CMACDT        IFNE      *ZERO
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                         YMD --> MDY
     C                   MOVEL(P)  SRDTO         SCACDT
     C                   ELSE
     C                   MOVE      *BLANK        SCACDT
     C                   END
     C*
     C*  IF CLAIM WAS CONVERTED FROM ANOTHER SYSTEM, DISPLAY OLD CLAIM #
     C*
     C     CMCVT#        IFNE      *BLANK
     C                   MOVEL     CMCVT#        SCCVT
     C                   MOVE      *ON           *IN64
     C                   END
     C*  FYR-CASE
     C                   MOVE(P)   CMFYR         XAFYR             3
     C                   MOVE(P)   CMCASE        XACASE            7
     C     XAFYR         CAT(P)    '-':0         SCYRCS
     C     SCYRCS        CAT       XACASE:0      SCYRCS
     C*
     C     CMFYR         IFEQ      *ZERO
     C     CMCASE        OREQ      *ZERO
     C                   MOVE      *BLANK        SCYRCS
     C                   END
     C*
     C*  AGENGY CLAIM# (OR, SATE CLAIM#)
     C*
     C                   MOVE      *BLANK        SCSCL#
     C                   MOVEL(P)  CMACL#        SCSCL#
     C*
     C     CDLSN         CAT(P)    ',':0         SCCNAM                         LAST, FIRST M.
     C     SCCNAM        CAT       CDFSN:1       SCCNAM
     C     CDMID         IFNE      *BLANK
     C     SCCNAM        CAT       CDMID:1       SCCNAM
     C     SCCNAM        CAT       '.':0         SCCNAM
     C                   END
     C*
     C                   MOVEL     CDSS#         XCSS#
     C***  Begin Chg  ***  06/09/20  *******************************
     C*                  Z-ADD     XCSS#         SCSS#
     C                   EVAL      SCSS#=%Char(XCSS#)
     C                   IF        XCSS#<>*ZEROS
     C                   EVAL      SCSS#=Maskdata('SSN':'SCSS#':%trim(SCSS#))
     C                   EVAL      SCSS#=DisplaySS#(SCSS#)
     C                   ELSE
     C                   EVAL      SCSS#=*Blanks
     C                   ENDIF
     C***  End Chg  ***  06/09/20  *******************************
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   ENDSR
     C/EJECT
     C**************************************************************************
     C*   ROUTINE - SRLDB
     C*
     C     SRLDB         BEGSR
     C*
     C*   Define Key Lists
     C*
     C*          WMEML - Employer Master
     C*
     C     EMKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C*
     C*          WMALL - Additional Entity/Location File
     C*
     C     ALKEY         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXELID
     C*
     C*          WMCDL - System Control File
     C*
     C     CDKEY         KLIST
     C                   KFLD                    XXSS#
     C*
     C*
     C*          WMIXL - EDI Claims Control Maintenance File
     C*
     C     IXKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXJRST
     C                   KFLD                    XXTRID
     C*
     C*          WDELL - Employer Detail File
     C*
     C     ELKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    XXFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV
     C                   KFLD                    XXFYR
     C*
     C*   Define Work Fields
     C*
     C     *LIKE         DEFINE    EMEMP#        XXEMP#
     C     *LIKE         DEFINE    EMDIV         XXDIV
     C     *LIKE         DEFINE    ALELID        XXELID
     C     *LIKE         DEFINE    CDSS#         XXSS#
     C     *LIKE         DEFINE    CMACDT        XXDATE
     C*
     C*  INITIALIZE SCREEN AND WORK FIELDS
     C*
     C                   MOVE      *BLANKS       XCFND             3
     C                   MOVE      *BLANKS       XCFYR             3
     C                   MOVE      *BLANKS       XCCASE            7
     C                   MOVE      *BLANKS       XCDIV             3
     C                   MOVE      *BLANKS       XCEMP#            5
     C                   MOVE      *ZEROS        XXDATE
     C                   MOVE      *ZEROS        XCSS#             9 0
     C                   MOVE      *ZEROS        P                 5 0
     C                   MOVE      *OFF          @SF02
     C                   MOVE      *OFF          @SF03
     C                   MOVE      *OFF          @SF04
     C                   MOVE      *OFF          @SF05
     C                   MOVE      *OFF          @SF06
     C                   Z-ADD     0             XXAZIP            9 0
     C*
     C*   Retrieve the Employer Master Record
     C*
     C                   MOVE      CMEMP#        XXEMP#
     C                   MOVE      CMDIV         XXDIV
     C     EMKEY         CHAIN     PEM01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PEM01
     C                   ENDIF
     C*
     C                   MOVEL     EMNAME        SCENAM
     C                   Z-ADD     EMEMP#        SCEMP#
     C                   Z-ADD     EMDIV         SCDIV
     C*
     C*   Retrieve the Employer Detail Record
     C*
     C     ELKY          CHAIN     PEL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PEL01
     C                   ENDIF
     C*
     C*   Retrieve the Address Location Record
     C*
     C                   MOVE      CMCTG1        XXELID
     C     ALKEY         CHAIN     PAL01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PAL01
     C                   ENDIF
     C                   MOVEL(P)  ALNAME        SCLOCD
     C*
     C*   Retrieve the Claimant Demographics Record
     C*
     C                   MOVE      CMSS#         XXSS#
     C     CDKEY         CHAIN     PCD01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   CLEAR                   PCD01
     C                   ENDIF
     C*
     C*   Retrieve the Data Element Criteria
     C*
     C                   MOVE      *BLANKS       CF
     C                   MOVE      *BLANKS       FD
     C*
     C     DCKY          CHAIN     PDC01                              60
     C     *IN60         DOWEQ     *OFF
     C     DCSET         IFEQ      '148'
     C     DCSET         OREQ      *BLANKS
     C                   MOVE      DCDN#         P
     C     P             IFLE      99
     C     P             ANDGT     0
     C     CF(P)         IFEQ      *BLANKS
     C                   MOVE      DCCRTR        CF(P)
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C     DCKY          READE     PDC01                                  60
     C                   ENDDO
     C*
     C     1             DO        99            P
     C     CF(P)         IFNE      *BLANKS
     C                   MOVEL     '('           FD(P)
     C                   CAT       CF(P):0       FD(P)
     C                   CAT       '):':0        FD(P)
     C                   ELSE
     C                   MOVE      ':'           FD(P)
     C                   ENDIF
     C                   ENDDO
     C*
     C                   MOVE      SCJRST        XXJRST
     C                   MOVE      '148'         XXTRID
     C*
     C                   CALL      'WA076R'      IXAPI                  99
     C     @RTNCD        IFEQ      'Y'
     C                   MOVE      '0'           *IN60
     C                   ELSE
     C                   MOVE      '1'           *IN60
     C                   ENDIF
     C*
     C*
     C*
     C**  IF THE ACCIDENT DATE IS NOT WITHIN THE COVERAGE PERIOD, DEFAULT
     C**  THE CLM DENIAL FLAG TO 'Y'.
     C*
     C     EMEFD1        IFNE      0
     C     CMACDT        ANDGE     EMEFD1
     C     EMCAD1        IFEQ      0
     C     EMCAD1        ORNE      0
     C     CMACDT        ANDLT     EMCAD1
     C                   GOTO      PASSED
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMEFD2        IFNE      0
     C     CMACDT        ANDGE     EMEFD2
     C     EMCAD2        IFEQ      0
     C     EMCAD2        ORNE      0
     C     CMACDT        ANDLT     EMCAD2
     C                   GOTO      PASSED
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMEFD3        IFNE      0
     C     CMACDT        ANDGE     EMEFD3
     C     EMCAD3        IFEQ      0
     C     EMCAD3        ORNE      0
     C     CMACDT        ANDLT     EMCAD3
     C                   GOTO      PASSED
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMEFD4        IFNE      0
     C     CMACDT        ANDGE     EMEFD4
     C     EMCAD4        IFEQ      0
     C     EMCAD4        ORNE      0
     C     CMACDT        ANDLT     EMCAD4
     C                   GOTO      PASSED
     C                   ENDIF
     C                   ENDIF
     C*
     C     EMEFD5        IFNE      0
     C     CMACDT        ANDGE     EMEFD5
     C     EMCAD5        IFEQ      0
     C     EMCAD5        ORNE      0
     C     CMACDT        ANDLT     EMCAD5
     C                   GOTO      PASSED
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      'Y'           SCDNY
     C     PASSED        TAG
     C*
     C*   Retrieve the EDI Log File Record
     C*
     C                   EXSR      SRGLGR
     C*
     C     @LGRF         IFEQ      *OFF
     C                   MOVE      *BLANKS       LGTSET
     C                   MOVE      *BLANKS       LGMTC
     C                   MOVE      *ZEROS        LGDT
     C                   MOVE      *ZEROS        LGDUE
     C                   ENDIF
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
     C*   ROUTINE - SRLSC2
     C*
     C     SRLSC2        BEGSR
     C*
     C*   Initialize Screen Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     @SF02         IFEQ      *OFF
     C                   MOVEL     CMACL#        SCACL#
     C                   ENDIF
     C*
     C                   MOVE      LGTSET        SCTSET
     C*
     C                   MOVE      *BLANKS       SCSETD
     C     SCTSET        IFNE      *BLANKS
     C                   MOVEL     'TSET'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCTSET        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCSETD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      LGMTC         SCMTC
     C*
     C                   MOVE      *BLANKS       SCTCD
     C     SCMTC         IFNE      *BLANKS
     C     SCTSET        IFEQ      '148'
     C                   MOVEL     'FMTC'        XXTBID
     C                   ELSE
     C                   MOVEL     'SMTC'        XXTBID
     C                   ENDIF
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCMTC         XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCTCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     LGMTCD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMTDT
     C                   Z-ADD     LGDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDUE
     C*
     C     SCDUE         IFEQ      *ZEROS
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE            8 0
     C     CMRCVD        IFGT      *ZEROS
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SRDATE
     C                   ENDIF                                                  CMRCVD > 0
     C     SRDATE        IFEQ      *ZEROS
     C                   MOVE      UDATE         SRDATE
     C                   ENDIF                                                  CMACDT > 0
     C                   MOVE      SRDATE        SCDUE
     C*
     C                   Z-ADD     IXSDUE        SRDAYS            5 0
     C     SRDATE        IFGT      *ZEROS
     C     SRDAYS        ANDGT     *ZEROS
     C                   MOVE      *BLANKS       SRERR             1
     C*
     C                   CALL      'SRFUTR8'
     C                   PARM                    SRDATE
     C                   PARM                    SRDAYS
     C                   PARM                    SRERR
     C                   MOVE      SRDATE        SCDUE
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVEL     COADR1        SCGAD1
     C                   MOVEL     COADR2        SCGAD2
     C                   MOVEL     COCTY         SCGCTY
     C                   MOVEL     COST          SCGST
     C                   MOVE      COZIP         SCGZIP
     C*
     C*   Get group auxiliary for FEIN
     C*
     C     F2KY          CHAIN     PF201                              60
     C     *IN60         IFEQ      *ON
     C                   Z-ADD     0             F2FEIN
     C                   ENDIF
     C*
     C     F2TPA         IFEQ      'Y'
     C                   MOVE      COFEIN        SC3EIN
     C                   MOVEL     CONAME        SCADMN
     C                   ELSE
     C                   MOVE      *BLANKS       SC3EIN
     C                   MOVE      *BLANKS       SCADMN
     C                   ENDIF
     C*
     C                   MOVE      F2FEIN        SCGFEI
     C*
     C                   MOVE      *BLANKS       SCACLC
     C                   MOVEL     XXCO#         SCACLC
     C                   MOVE      SCFND         XCFND
     C                   MOVE      *ZEROS        XCFYR
     C                   MOVE      SCYR          XCFYR
     C                   MOVE      SCCASE        XCCASE
     C     SCACLC        CAT       XCFND:0       SCACLC
     C     SCACLC        CAT       XCFYR:0       SCACLC
     C     SCACLC        CAT       XCCASE:0      SCACLC
     C*
     C                   MOVE      *ON           @SF02
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
     C*   ROUTINE - SRLSC3
     C*
     C     SRLSC3        BEGSR
     C*
     C*   Initialize Screen Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     ALFEIN        IFGT      *ZEROS
     C                   MOVE      ALFEIN        SCFEIN
     C                   ELSE
     C                   MOVE      EMFEIN        SCFEIN
     C                   ENDIF
     C*
     C                   MOVEL     EMNAME        SCINSN
     C*
     C     ALDBA         IFGT      *BLANKS
     C                   MOVEL     ALDBA         SCEMPN
     C                   ELSE
     C*
     C                   MOVEL     EMNAME        SCEMPN
     C*
     C                   ENDIF
     C*
     C     ALPAD1        IFGT      *BLANKS
     C                   MOVEL     ALPAD1        SCADR1
     C                   MOVEL     ALPAD2        SCADR2
     C                   MOVEL     ALPCTY        SCCTY
     C                   MOVEL     ALPST         SCST
     C                   MOVEL     ALPZIP        SCZIP
     C                   ELSE
     C     ALMAD1        IFGT      *BLANKS
     C                   MOVEL     ALMAD1        SCADR1
     C                   MOVEL     ALMAD2        SCADR2
     C                   MOVEL     ALMCTY        SCCTY
     C                   MOVEL     ALMST         SCST
     C                   MOVEL     ALMZIP        SCZIP
     C                   ELSE
     C     EMPAD1        IFGT      *BLANKS
     C                   MOVEL     EMPAD1        SCADR1
     C                   MOVEL     EMPAD2        SCADR2
     C                   MOVEL     EMPCTY        SCCTY
     C                   MOVEL     EMPST         SCST
     C                   MOVEL     EMPZIP        SCZIP
     C                   ELSE
     C     EMMAD1        IFGT      *BLANKS
     C                   MOVEL     EMMAD1        SCADR1
     C                   MOVEL     EMMAD2        SCADR2
     C                   MOVEL     EMMCTY        SCCTY
     C                   MOVEL     EMMST         SCST
     C                   MOVEL     EMMZIP        SCZIP
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C                   ENDIF
     C*
     C*   Load data to new screen fields
     C                   MOVEL     SCEMPN        SCENME
     C                   MOVEL     SCADR1        SCEAD1
     C                   MOVEL     SCADR2        SCEAD2
     C                   MOVEL     SCCTY         SCECTY
     C                   MOVEL     SCST          SCEST
     C                   MOVEL     SCZIP         SCEZIP
     C*
     C                   MOVE      SCZIP         XXAZIP            9 0
     C*
     C                   MOVE      FNSFIN        SCSLFI
     C*
     C     EMNAIC        IFNE      *BLANKS
     C     CMJRST        ANDNE     'AR'
     C     CMJRST        ANDNE     'KY'
     C                   MOVEL(P)  EMNAIC        SCSIC
     C                   ELSE
     C*
     C     CMSIC         IFNE      *BLANKS
     C                   MOVE      CMSIC         SCSIC
     C                   ELSE
     C                   MOVE      EMSIC         SCSIC
     C                   ENDIF
     C*
     C     SCSIC         IFNE      *BLANKS
     C*
     C                   MOVE      *BLANKS       SCSICD
     C     SCSIC         IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTBID
     C                   MOVEL     'SIC '        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCSIC         XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCSICD
     C                   ENDIF
     C                   ENDIF
     C*
     C     CMJRST        IFEQ      'AR'
     C     CMJRST        OREQ      'FL'
     C     CMJRST        OREQ      'KY'
     C     CMJRST        OREQ      'MO'
     C     CMJRST        OREQ      'TN'
     C     CMJRST        OREQ      'CA'
     C                   CAT       'SC':0        SCSIC
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCINSC
     C*
     C                   MOVE      *BLANKS       SCELID
     C     ALELID        IFGT      *BLANKS
     C                   MOVEL     ALELID        SCELID
     C                   ELSE
     C                   MOVEL     EMEMP#        SCELID
     C                   MOVE      EMDIV         XCDIV
     C     SCELID        CAT       XCDIV:0       SCELID
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCPOLN
     C                   MOVE      ELFND         XCFND
     C                   MOVE      *ZEROS        XCFYR
     C                   MOVE      ELFYR         XCFYR
     C                   MOVE      ELEMP#        XCEMP#
     C                   MOVE      EMDIV         XCDIV
     C                   MOVEL     XCFND         SCPOLN
     C     SCPOLN        CAT       XCEMP#:0      SCPOLN
     C     SCPOLN        CAT       XCFYR:0       SCPOLN
     C*
     C                   Z-ADD     ELPFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPFDT
     C                   Z-ADD     ELPTDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPTDT
     C*
     C     @SF03         IFEQ      *OFF
     C                   MOVEL     CMECS#        SCINSC
     C                   ENDIF
     C                   MOVE      *ON           @SF03
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
     C*   ROUTINE - SRLSC4
     C*
     C     SRLSC4        BEGSR
     C*
     C*   Initialize Screen Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     @SF04         IFEQ      *OFF
     C                   MOVE      CMEADR        SCEADR
     C                   MOVE      CMIPCT        SCIPCT
     C                   MOVE      CMITRT        SCITRT
     C                   Z-ADD     IXELTD        SCELTD
     C     CMLOST        IFGE      IXELTD
     C                   MOVEL     'Y'           SCGT1
     C                   ELSE                                                   X LOST/ELDT
     C                   MOVEL     'N'           SCGT1
     C                   ENDIF                                                  e LOST/ELDT
     C                   Z-ADD     CMLOST        SCLOST
      ** Beg Add *** 04/05/12 *****************************************
      /Free
       SCmltd = IXmltd;

       If CMlost >= IXmltd;
          SCgt2 = 'Y';
       Else;
          SCgt2 = 'N';
       EndIf;

       SCmo = CMlost;
      /End-Free
      ** End Add *** 04/05/12 *****************************************
     C                   MOVE      CMODIS        SCODIS
     C                   MOVE      CMDNY         SCDNY
     C                   MOVE      CMACQR        SCACQR
     C                   Z-ADD     CMACZ1        SCZIP1
     C                   Z-ADD     CMACZ2        SCZIP2
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCITRD
     C     SCITRT        IFNE      *BLANKS
     C                   MOVEL     'INIT'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCITRT        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCITRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     CMACDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACD0
     C                   Z-ADD     XXACTM        SCACTM
     C*
     C                   MOVE      CMPART        SCPART
     C                   MOVE      *BLANKS       SCPARD
     C     SCPART        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCPART        XXTKEY
     C     XTBKY         CHAIN     PPA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     PADESC        SCPARD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      CMNATR        SCNATR
     C                   MOVE      *BLANKS       SCNATD
     C     SCNATR        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCNATR        XXTKEY
     C     XTBKY         CHAIN     PNA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     NADESC        SCNATD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      CMCAUS        SCCAUS
     C                   MOVE      *BLANKS       SCCAUD
     C     SCCAUS        IFNE      *BLANKS
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCCAUS        XXTKEY
     C     XTBKY         CHAIN     PCA01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     CADESC        SCCAUD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   Z-ADD     CMDTRP        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTRP
     C                   Z-ADD     CMRCVD        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRCVD
     C                   MOVEL     CMACDS        SCACDS
     C                   MOVE      CMACD2        SCACDS
     C*
     C                   MOVE      *ON           @SF04
     C*
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
     C*   ROUTINE - SRLSC5
     C*
     C     SRLSC5        BEGSR
     C*
     C*   Initialize Screen Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     @SF05         IFEQ      *OFF
     C                   MOVE      CDMSTS        SCMSTS
     C                   MOVE      CMDBDT        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDBDT
     C                   MOVE      CMDOD         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOD
     C                   Z-ADD     CDAREA        SCAREA
     C                   Z-ADD     CDPHN#        SCPHN#
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCMSTD
     C     SCMSTS        IFNE      *BLANKS
     C                   MOVEL     'MSTS'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCMSTS        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCMSTD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCEMPN
     C                   MOVEL     CDLSN         SCEMPN
     C     CDLSN         IFNE      *BLANKS
     C     SCEMPN        CAT       ',':0         SCEMPN
     C     SCEMPN        CAT       CDFSN:1       SCEMPN
     C     SCEMPN        CAT       CDMID:1       SCEMPN
     C                   ELSE
     C                   MOVEL     CDFSN         SCEMPN
     C     SCEMPN        CAT       CDMID:1       SCEMPN
     C                   ENDIF
     C*
     C                   MOVEL     CDADR1        SCADR1
     C                   MOVEL     CDADR2        SCADR2
     C                   MOVEL     CDCTY         SCCTY
     C                   MOVE      CDST          SCST
     C                   MOVE      CDZIP         SCZIP
     C                   Z-ADD     CDDTOB        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTOB
     C                   MOVE      CDSEX         SCSEX
     C*
     C                   MOVE      *BLANKS       SCSEXD
     C     SCSEX         IFNE      *BLANKS
     C                   MOVEL     'GNDR'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCSEX         XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCSEXD
     C                   ENDIF
     C                   ENDIF
     C*
     C*
     C*   Retrieve # of Dependents
     C*
     C                   MOVE      *ZEROS        SCDEP
     C     ACKY          CHAIN     PAC01                              60
     C     *IN60         DOWEQ     *OFF
     C     ACREL         IFNE      *BLANK
     C                   ADD       1             SCDEP
     C                   ENDIF
     C     ACKY          READE     PAC01                                  60
     C                   ENDDO
     C*
     C     SCDEP         IFEQ      *ZERO
     C                   Z-ADD     CM#DEP        SCDEP
     C                   ENDIF
     C*
     C                   MOVE      *ON           @SF05
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
     C*   ROUTINE - SRLSC6
     C*
     C     SRLSC6        BEGSR
     C*
     C*   Initialize Screen Fields
     C*
     C                   TIME                    DSDTTM
     C*
     C     @SF06         IFEQ      *OFF
     C                   MOVE      CMPREX        SCPREX
     C                   MOVE      CMRLW         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRLW
     C                   MOVE      CMRTWQ        SCRTWQ
     C                   MOVE      CMESTS        SCESTS
     C                   MOVE      CMOCDS        SCOCDS
     C                   MOVE      CMWGPD        SCWGPD
     C                   MOVE      CMRTW         XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCRTW
     C*
     C     LLKY          CHAIN     PLL01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      LLDTAP        XXDATE
     C                   Z-ADD     XXDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCREP
     C                   ENDIF
     C*
     C*
     C                   MOVE      CMAGCP        SCAGCP
     C                   MOVE      CMGT1         SCGT1
     C                   MOVEL     CMOCDS        SCOCDS
     C                   MOVE      CMDNY         SCDNY
     C                   MOVE      CMACQR        SCACQR
     C                   MOVE      CMCRT         SCCRT
     C                   MOVE      CMPOI         SCFSPD
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCESTD
     C     SCESTS        IFNE      *BLANKS
     C                   MOVEL     'EMST'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCESTS        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCESTD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCWGDD
     C     SCWGPD        IFNE      *BLANKS
     C                   MOVEL     'FRWP'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCWGPD        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCWGDD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCRTWD
     C     SCRTWQ        IFNE      *BLANKS
     C                   MOVEL     'RTWQ'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCRTWQ        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCRTWD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      *BLANKS       SCAGCD
     C     SCAGCP        IFNE      *BLANKS
     C                   MOVEL     'AGCP'        XXTBID
     C                   MOVE      *BLANKS       XXTKEY
     C                   MOVEL     SCAGCP        XXTKEY
     C     XTKY          CHAIN     PGT01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     GTDESC        SCAGCD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      CMCLAS        SCCLAS
     C                   MOVE      CMSPEC        SCSPEC
     C                   MOVE      CMEXT         SCEXT
     C*
     C*  GET MOST CURRENT CLASS CODE DESCRIPTION
     C*
     C                   Z-ADD     CMACDT        XXEFDT
     C                   MOVE      *BLANK        SCCLSD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  60
     C     *IN60         IFEQ      *OFF
     C     PRSTAT        ANDEQ     'A'
     C                   MOVEL(P)  PRDESC        SCCLSD
     C                   END
     C*
     C     EPKY          CHAIN     PEP01                              60
     C     *IN60         IFEQ      *OFF
     C     EPEXT         ANDNE     *BLANKS
     C                   MOVE      *BLANKS       XXST
     C     PXKY          SETGT     PPX01
     C     PXKY1         READPE    PPX01                                  60
     C  N60              MOVEL(P)  PXDESC        SCCLSD
     C   60RSKY          CHAIN     PRS01                              61
     C  N61
     CAN 60              MOVE      RSJRST        XXST
     C**N61*60** PXKY      CHAINPPX01                60
     C  N61
     CAN 60PXKY          SETGT     PPX01
     C  N61
     CAN 60PXKY1         READPE    PPX01                                  60
     C  N61
     CANN60              MOVEL(P)  PXDESC        SCCLSD
     C                   ENDIF                                                  *IN60 IFEQ
     C                   Z-ADD     CMHIDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDOH
     C                   MOVE      CMAWW         SCWAGE                         Avg wkly wge
     C                   MOVE      CMDAYS        SCDAYS                         Days wrk
     C                   Z-ADD     CMDTLW        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCDTLW
     C                   Z-ADD     CMMMI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCMMI
     C*
     C     CMFSAL        IFEQ      'Y'                                          Full salary
     C                   MOVE      CMFSAL        SCFSAL                          paid code
     C                   ELSE
     C                   MOVE      'N'           SCFSAL
     C                   ENDIF
     C*
     C*        Determine Claim Type
     C*
     C                   MOVE      CMLTFL        SCCLMT
     C     CMLTFL        IFEQ      'R'
     C                   MOVE      'N'           SCCLMT
     C                   ENDIF                                                  E LTFL=R
     C*
     C     CMKY          SETGT     PTH01
     C     CMKY          READPE    PTH01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C*
     C     THFROM        IFNE      *BLANK
     C     THTO          ANDEQ     'M'
     C                   MOVE      'B'           SCCLMT
     C                   ENDIF
     C*
     C     THFROM        IFNE      *BLANK
     C     THTO          ANDEQ     'I'
     C                   MOVE      'L'           SCCLMT
     C                   ENDIF
     C                   ENDIF                                                  *60 = 0
     C*
     C*          Determine Claim Status
     C*
     C                   MOVE      CMSTAT        SCCLMS
     C     CHKEY         SETGT     PCH01
     C     CHKEY         READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C*
     C     CHOCST        IFEQ      'C'
     C     CHKEY         READPE    PCH01                                  60
     C     CHKEY         READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'X'           SCCLMS
     C                   ELSE
     C                   MOVE      'C'           SCCLMS
     C                   ENDIF
     C                   ENDIF
     C*
     C     SCCLMS        IFEQ      'O'
     C     CHKEY         READPE    PCH01                                  60
     C     *IN60         IFEQ      '0'
     C                   MOVE      'R'           SCCLMS
     C                   ELSE
     C                   MOVE      'O'           SCCLMS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C*   Claim type
     C                   MOVEL     CMLTFL        SCCLT
     C     CMLTFL        IFEQ      'R'
     C                   MOVEL     'N'           SCCLT
     C                   ENDIF                                                  E LTFL=R
     C*
     C*
     C                   MOVE      *ON           @SF06
     C*
     C                   ENDSR
     C/EJECT
      **************************************************************************
      **************************************************************************
     C*   ROUTINE - SRGLGR
     C*
     C     SRGLGR        BEGSR
     C*
     C     *LIKE         DEFINE    *IN60         @LGRF
     C                   MOVE      *OFF          @LGRF
     C*
     C     CMKY          SETGT     PLG01
     C     CMKY          READPE    PLG01                                  60
     C*
     C     *IN60         IFEQ      *OFF
     C                   MOVE      *ON           @LGRF
     C                   ENDIF
     C*
     C                   ENDSR
