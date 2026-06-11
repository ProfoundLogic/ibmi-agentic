     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H/TITLE W2295R - WW MCO Group Codes
     H*****************************************************************
     H*                                                               *
     H* CALLED BY: W0010R                                             *
     H*            W0051R02                                           *
     H*                                                               *
     H* CALLS TO:  W2295R02                                           *
     H*                                                               *
     H* CREATED:                                                      *
     H*                                                               *
     H* 10/28/99 C:61  PROJECT WC958                                  *
     H*****************************************************************
     H*                                                               *
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 02/11/02 C:136 SUPPORT 31196                                  *
     H*                TOP LINE OF HEADER DOESN'T DISPLAY WHEN FILE   *
     H*                IS EMPTY                                       *
     H*                                                               *
     H* XX/XX/XX C:XX  PROJECT XXXXX                                  *
     H*                XXX.......................................XXX  *
     H*                                                               *
     H* PLEASE ADD TO TOP OF REVISIONS                                *
     H*****************************************************************
     H*                                                               *
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  XX - XX  XXX............................XXX                  *
     H*  XX - XX  XXX............................XXX                  *
     H*                                                               *
     H*  60       GENERAL READ/CHAIN                                  *
     H*****************************************************************
      *
     FW2295F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
      *
      *  Physical file definition (for refreshing subfile record)
     FWMMGP     IF   E             DISK
     F                                     RENAME(PMG01:PFRCDF)
      *
      *  Primary logical file definition (for displaying items)
     FWMMGL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PMG01:DBRCDF)
      *
      *  Secondary logical file definition (for displaying items)
     FWMMGL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PMG01:DBRCDF2)
      *
      *  Fund
     FWMFNL     IF   E           K DISK
     FWMFNP     IF   E             DISK
     F                                     RENAME(PFN01:PFN02)
      *
      /COPY SCOPYWW01D
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(11) ALT(OF)
      *
     D***PY SCOPYWW01E
      *
      *
     D***PY SCOPYWW01I
      *
      *
      *
     D                 DS
     D  SC1                    1     75
     D  XXMCO1                 1      5
     D  XXEFD1                 7     17
     D  XXEXD1                19     29
     D  XXNM1                 31     58
     D  XXPH1                 60     73
      *
     D                 DS
     D  SC2                    1     75
     D  XXNM2                  1     26
     D  XXEFD2                29     39
     D  XXEXD2                41     51
     D  XXMCO2                53     58
     D  XXPH2                 60     73
      *
      *
      *
      *================================================================
      *  /COPY required constants
      *================================================================
     D DTMSK           C                   CONST('DATE')
     D HDLT1           C                   CONST('MCO    Effective  Ex-
     D                                     piration')
     D HDLT2           C                   CONST('Effective  Expiratio-
     D                                     n  MCO')
     D OPTNM           C                   CONST('MCO Group Codes')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D ACDPGM          C                   CONST('W2295R02')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(10)
      *
      *
      *
      *================================================================
      *  PLIST definitions
      *================================================================

     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN1            9 0          WMMGP
     C                   PARM                    @@RRN2            9 0          WMFNP
     C                   PARM                    @@EFDT            7 0
      *
      *  If called in select mode (@@CALL = '2') and @@EFDT not
      *  zero, then only displays groups effective as of @@EFDT
      *
      *
      *
      *  Add/change/display program parameter list
      *
     C     ACDPL         PLIST
     C     *INKC         PARM      *BLANK        ##KC              1
     C     *INKL         PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0          WMMGP
     C                   PARM      @@RRN2        ##RRN2            9 0          WMFNP
      *
      *  Company file/fund (group) retrieve program parameter list
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
      *
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
      *
      *
      *
      *================================================================
      *  KLIST definitions
      *================================================================
     C     KEY           KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
      *
     C     KEY1          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKMCO
     C                   KFLD                    KKEFDT
      *
     C     KEY2          KLIST
     C                   KFLD                    KKCO#
     C                   KFLD                    KKFND
     C                   KFLD                    KKNAME
     C                   KFLD                    KKEFDT
      *
     C     FNKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
      *
      *
      *
      *================================================================
      *  *LIKE DEFN statements
      *================================================================
     C     *LIKE         DEFINE    MGCO#         KKCO#
     C     *LIKE         DEFINE    MGFND         KKFND
     C     *LIKE         DEFINE    MGMCO         KKMCO
     C     *LIKE         DEFINE    MGEFDT        KKEFDT
     C     *LIKE         DEFINE    MGNAME        KKNAME
      *
     C/COPY SCOPYWW01C
      *
      *
      *
      *================================================================
      * CHKSSM - Set indicator 11 for subset message
      *================================================================
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
      *
      *
      *
      *================================================================
      * ERRCK1 - Additional errors on subfile selection
      *================================================================
     C     ERRCK1        BEGSR
      *
      * Refresh WW screen after delete
      *
     C     SCSEL         IFEQ      '4'
     C                   MOVE      *ON           AUTORF
     C                   ENDIF
      *
     C                   ENDSR                                                  ERRCK1
      *
      *
      *
      *================================================================
      * ERRCK2 - Error checking for the subset screen
      *================================================================
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
      *
      *
      *
      *================================================================
      * INZSST - Initialize the subset fields
      *================================================================
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
      *
      *
      *
      *================================================================
      * KEYINZ - Initialize key values for program start
      *================================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE      *LOVAL        KKMCO
     C                   MOVE      *HIVAL        KKEFDT
      *
     C                   MOVE      *LOVAL        KKNAME
      *** BEG ADD *** 02/11/02 *************************************************
     C                   SELECT
      *
     C     *IN21         WHENEQ    *ON
     C     INFO#         OREQ      1
     C     VIEW#         ANDEQ     1
     C                   MOVE(P)   *BLANKS       SCHDLT
     C                   CAT(P)    HDLT1:4       SCHDLT
      *
     C     *IN22         WHENEQ    *ON
     C                   MOVE(P)   *BLANKS       SCHDLT
     C                   CAT(P)    HDLT2:33      SCHDLT
      *
     C                   ENDSL
      *** END ADD *** 02/11/02 *************************************************
     C                   ENDSR                                                  KEYINZ
      *
      *
      *
      *================================================================
      * KEYSET - Set key values when doing "position to"
      *================================================================
     C     KEYSET        BEGSR
      *
     C                   SELECT
      *
     C     VIEW#         WHENEQ    1
     C                   MOVEL(P)  SCPST1        KKMCO
      *
     C     SCPSTB        IFNE      *BLANKS
     C                   MOVE      SCPSTB        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKEFDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKEFDT
     C                   ENDIF
      *
      *
     C     VIEW#         WHENEQ    2
     C                   MOVEL(P)  SCPST2        KKNAME
      *
     C     SCPSTC        IFNE      *BLANKS
     C                   MOVE      SCPSTC        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL(P)  SRDTO         KKEFDT
      *
     C                   ELSE
      *
     C                   MOVE      *HIVAL        KKEFDT
     C                   ENDIF
      *
      *
     C                   ENDSL
      *
     C                   ENDSR                                                  KEYSET
      *
      *
      *
      *================================================================
      * KEYSFL - Set key values from subfile records for paging
      *================================================================
     C     KEYSFL        BEGSR
      *
     C                   MOVEL(P)  SCMCO         KKMCO
     C                   Z-ADD     SCEFDT        KKEFDT
     C                   MOVEL(P)  SCNAME        KKNAME
      *
     C                   ENDSR                                                  KEYSFL
      *
      *
      *
      *================================================================
      * MVDBSC - Move data base fields to screen fields
      *================================================================
     C     MVDBSC        BEGSR
      *
     C                   MOVE(P)   *BLANKS       SC1
     C                   MOVE(P)   *BLANKS       SC2
      *
     C                   MOVEL(P)  MGMCO         SCMCO
     C                   Z-ADD     MGEFDT        SCEFDT
     C                   MOVEL(P)  MGNAME        SCNAME
      *
      *  Get MCO code
      *
     C                   MOVEL(P)  MGMCO         XXMCO1
     C                   MOVEL(P)  MGMCO         XXMCO2
      *
      *  Get name
      *
     C                   MOVEL(P)  MGNAME        XXNM1
     C                   MOVEL(P)  MGNAME        XXNM2
      *
      *  Get effective date
      *
     C                   Z-ADD     SCEFDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##EFDT
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##EFDT           80
     C                   PARM                    ##MASK           80
      *
     C                   MOVEL(P)  ##EFDT        XXEFD1
     C                   MOVEL(P)  ##EFDT        XXEFD2
      *
      *  Get expiration date
      *
     C                   Z-ADD     MGEXDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL(P)  SRDTO         ##EXDT
      *
     C                   CALL      'SA020R'
     C                   PARM                    ##EXDT           80
     C                   PARM                    ##MASK           80
      *
     C                   MOVEL(P)  ##EXDT        XXEXD1
     C                   MOVEL(P)  ##EXDT        XXEXD2
      *
      *  Get phone number
      *
     C                   MOVEL(P)  MGPHN#        XXPHN1            3
     C                   MOVE(P)   MGPHN#        XXPHN2            4
     C                   MOVE(P)   MGAREA        XXAREA            3
      *
     C     '('           CAT(P)    XXAREA:0      XXPHN            14
     C                   CAT(P)    ')':0         XXPHN
     C                   CAT(P)    XXPHN1:1      XXPHN
     C                   CAT(P)    '-':0         XXPHN
     C                   CAT(P)    XXPHN2:0      XXPHN
      *
     C                   MOVEL(P)  XXPHN         XXPH1
     C                   MOVEL(P)  XXPHN         XXPH2
      *** BEG DEL **** 02/11/02 ************************************************
     C**********           SELEC
     C********** VIEW#     WHEQ 1
     C********** INFO#     ANDEQ1
     C**********           MOVE *BLANKS   SCHDLT    P
     C**********           CAT  HDLT1:4   SCHDLT    P
     C**********           MOVELSC1       SCDATA    P
     C********** VIEW#     WHEQ 2
     C********** INFO#     ANDEQ1
     C**********           MOVE *BLANKS   SCHDLT    P
     C**********           CAT  HDLT2:33  SCHDLT    P
     C**********           MOVELSC2       SCDATA    P
     C**********           ENDSL
      *** END DEL **** 02/11/02 ************************************************
      *** BEG ADD **** 02/11/02 ************************************************
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  SC1           SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  SC2           SCDATA
     C                   ENDSL
      *** END ADD **** 02/11/02 ************************************************
      *
     C                   ENDSR                                                  MVDBSC
      *
      *
      *
      *================================================================
      * OTHFKS - Process other function keys
      *================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
      *
      *
      *
      *================================================================
      * OTHOPT - Process other options
      *================================================================
     C     OTHOPT        BEGSR
     C                   ENDSR                                                  OTHOPT
      *
      *
      *
      *================================================================
      * PGMEND - End of program processing
      *================================================================
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
      *
      *
      *
      *================================================================
      * PGMSTR - Start of program processing
      *================================================================
     C     PGMSTR        BEGSR
      *
     C                   MOVEL(P)  DTMSK         ##MASK
     C                   Z-ADD     *ZERO         @@RRN1
      *
      *  Get Fund name and KEY information
      *
     C     @@RRN2        IFNE      *ZERO
     C     @@RRN2        CHAIN     PFN02                              60
      *
     C                   ELSE
      *
     C     FNKY          CHAIN     PFN01                              60
     C                   ENDIF                                                  @@RRN2
      *
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     FNFND         SCFND
     C                   MOVEL(P)  FNNAME        SCFNNM
     C                   Z-ADD     FNCO#         KKCO#
     C                   Z-ADD     FNFND         KKFND
      *
     C                   ELSE
      *
     C                   Z-ADD     *ZERO         SCFND
     C                   MOVEL(P)  *BLANKS       SCFNNM
     C                   Z-ADD     *ZERO         KKCO#
     C                   Z-ADD     *ZERO         KKFND
     C                   ENDIF                                                  *IN60
      *
      *
      *
     C                   ENDSR                                                  PGMSTR
      *
      *
      *
      *================================================================
      * READ   - Read next record from file
      *================================================================
     C     READ          BEGSR
     C   21KEY           READE     DBRCDF                                 95
     C   22KEY           READE     DBRCDF2                                95
     C                   ENDSR                                                  READ
      *
      *
      *
      *================================================================
      * READP  - Read previous record from file
      *================================================================
     C     READP         BEGSR
     C   21KEY           READPE    DBRCDF                                 95
     C   22KEY           READPE    DBRCDF2                                95
     C                   ENDSR                                                  READP
      *
      *
      *
      *================================================================
      * SETGT  - Set greater than on file
      *================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C   22KEY2          SETGT     DBRCDF2                            96
     C                   ENDSR                                                  SETGT
      *
      *
      *
      *================================================================
      * SETLL  - Set lower limits on file
      *================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
     C   22KEY2          SETLL     DBRCDF2                            96
     C                   ENDSR                                                  SETLL
      *
      *
      *
      *================================================================
      * SF1UPD - Update subfile record when item is deleted
      *================================================================
     C     SF1UPD        BEGSR
     C                   ENDSR                                                  SF1UPD
      *
      *
      *
      *================================================================
      * SLCTRD - Move fields for selected record to parameters
      *================================================================
     C     SLCTRD        BEGSR
     C                   Z-ADD     SCDBRR        @@RRN1
     C                   ENDSR                                                  SLCTRD
      *
      *
      *
      *================================================================
      * SUBSET - Determine if record should be put in subfile
      *================================================================
     C     SUBSET        BEGSR
      *
     C     @@CALL        IFEQ      '2'
      *
     C     @@EFDT        IFLT      MGEFDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@EFDT      <>*ZEROS
      *
     C     MGEXDT        IFNE      *ZERO
     C     @@EFDT        ANDGT     MGEXDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  @@EFDT      <>*ZEROS
      *
     C                   ENDIF                                                  @@CALL      <>*ZEROS
      *
     C                   ENDSR                                                  SUBSET
      *
      *
      *
** Column headings
Opt code     date        date     Name                         Phone#
Opt Name                           date        date     code   Phone#
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change View          YYY
212F12=Cancel               YYY
217F17=Subset               NNN
3
