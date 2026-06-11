     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3102R - W/W AGENCY BILLING STATMENTS
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM DISPLAYS THE AGENCY BILLING INFORMATION          *
     H*                                                               *
     H* CALLED FROM:                                                  *
     H*   MENU                                                        *
     H* CALLS TO:                                                     *
     H*   W3102R02                                                    *
     H*                                                               *
     H*  CREATED: 04/05/99  C:33  PROJECT: 96WAD                      *
     H*  IN BASE: 07/19/99  C:33  PROJECT: WC899                      *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  04/21/17  C:309 Support: PHW46852                            *
     H*                  Remove Warning Msg for Clt 089 For           *
     H*                  "W/W Agency Billing Statement"               *
     H*                                                               *
     H*  01/07/09  C:220 SUPPORT: 66413                               *
     H*                  Remove Warning Msg for Clt 132 when accessing*
     H*                  W/W Agency Billing Statement                 *
     H*                                                               *
     H*  11/05/04  C:33  Project: WC1033                              *
     H*                  Include calculations for Agency Billing.     *
     H*                                                               *
     H*  08/24/01  C:136 SUPPORT: WA107 28446                         *
     H*                  Position to always repositions to last record*
     H*                                                               *
     H*  01/20/00  C:33  PROJECT: WC899                               *
     H*                  DO NOT ALLOW DELETION IF ANY OF THE DETAIL   *
     H*                  RECORDS WERE ENETERED ON THE CHECK SCREEN.   *
     H*****************************************************************
     FW3102F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWSASP     IF   E             DISK
     F                                     RENAME(PAS01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWSASL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAS01:DBRCDF)
     FWSASL1    IF   E           K DISK
     F                                     INFDS(DBFDS2)
     F                                     RENAME(PAS01:DBRCDF2)
     F***  BEGIN ADD  ***  01/20/00  *********************************
     FWDABL     IF   E           K DISK
     F***  END   ADD  ***  01/20/00  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(13) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(13) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D                 DS
     D  VIEW1                  1     14
     D  V1STDT                 1      7
     D  V1AGT#                10     14
     D                 DS
     D  VIEW2                  1     14
     D  V2AGT#                 1      5
     D  V2STDT                 8     14
     D                 DS
     D  INFO1                  1     58
     D  I1DUE                  1      8
     D  I1BILL                11     25
     D  I1PAID                28     42
     D  I1BAL                 44     58
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agency Billing State-
     D                                     ments')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D DLT             C                   CONST('* * * Deleted')
     D ACDPGM          C                   CONST('W3102R02')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(2)
     D SF1PAG          C                   CONST(12)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM                    @#CALL            1
     C***  Begin Add  ***  11/05/04  **************************************
     C                   PARM                    @@AGT#            5
     C                   PARM                    @@KC              1
     C                   PARM                    @@KL              1
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDY         PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      '*MDY'        SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     AGTPL         PLIST
     C                   PARM                    ##CO#
     C                   PARM                    ##FND
     C                   PARM      *ZEROS        ##EMP#            9 0
     C                   PARM      *ZEROS        ##DIV             5 0
     C                   PARM      *ZEROS        ##FYR             3 0
     C                   PARM      ASAGT#        ##AGT#            5 0
     C     SCANAM        PARM                    ##ANAM           60
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKSTDT
     C                   KFLD                    KKAGT#
     C*
     C     KEY2          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKAGT#
     C                   KFLD                    KKSTDT
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     KEY2S         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKAGT#
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C***  BEGIN ADD  ***  01/20/00  *********************************
     C     ABKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSTDT
     C                   KFLD                    SCAGT#
     C***  END   ADD  ***  01/20/00  *********************************
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @#CALL        @@CALL
     C     *LIKE         DEFINE    ASSTDT        KKSTDT
     C     *LIKE         DEFINE    ASAGT#        KKAGT#
     C     *LIKE         DEFINE    ASSTDT        S5STDT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSBAL         IFNE      *BLANK
     C     SSAGT#        ORNE      *ZEROS
     C     SSSTDT        ORNE      *ZEROS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSBAL<>*BLANK
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C***  BEGIN ADD  ***  01/20/00  *********************************
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     SCSEL         WHENEQ    '4'
     C*
     C     ABKY          CHAIN     PAB01                              60
     C*
     C     *IN60         DOWEQ     *OFF
     C*
     C     ABARTY        IFEQ      'P'
     C     ABARTY        OREQ      'R'
     C     ABARTY        OREQ      'V'
     C                   SETON                                        71  90
     C                   LEAVE
     C                   ENDIF                                                  ABARTY=P/R/V
     C*
     C     ABKY          READE     PAB01                                  60
     C*
     C                   ENDDO
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C***  END   ADD  ***  01/20/00  *********************************
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C  N90SSSTDT        IFNE      *ZERO
     C*
     C                   Z-ADD     SSSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DATCHK
     C                   MOVEL     SRDTO         SSSTDT
     C*
     C                   CALL      'SRCVTMY'     DTCYMD
     C                   MOVEL     SRDTO         S5STDT
     C*
     C     SRERR         IFEQ      *ON
     C                   SETON                                        71  90
     C                   ENDIF                                                  SRERR=*ON
     C*
     C                   ENDIF                                                  SSSTDT<>*ZERO
     C*
     C  N90SSBAL         IFNE      *BLANKS
     C     SSBAL         ANDNE     'Y'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SSBAL<>*BLANKS
     C*
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSAGT#
     C                   CLEAR                   SSSTDT
     C                   MOVE      'Y'           SSBAL
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     SGLAGT        IFEQ      *ON
     C                   MOVE      @@AGT#        SSAGT#
     C                   ENDIF
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKSTDT
     C                   CLEAR                   KKAGT#
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     SGLAGT        IFEQ      *ON
     C                   MOVE      @@AGT#        KKAGT#
     C                   ENDIF
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
      *** BEG CHG *** 08/24/01 ****************************************
     C******************** MOVE SCPST1    KKSTDT
     C******************** MOVE SCPSTA    KKAGT#
     C                   MOVE(P)   SCPST1        SRDTI
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********           CALL 'SRCVTDAT'DTCYMD
     C                   CALL      'SRCVTMY'     DTCYMD
     C***  End   Chg  ***  11/05/04  **************************************
     C                   MOVEL     SRDTO         KKSTDT
     C                   MOVE(P)   SCPSTA        KKAGT#
      *** END CHG *** 08/24/01 ****************************************
     C***  Begin Add  ***  11/05/04  **************************************
     C                   CALL      'SRCVTMY'     DTCYMD
     C     SGLAGT        IFEQ      *ON
     C                   MOVE      @@AGT#        KKAGT#
     C                   ENDIF
     C***  End   Add  ***  11/05/04  **************************************
     C     *IN22         WHENEQ    *ON
      *** BEG CHG *** 08/24/01 ****************************************
     C******************** MOVE SCPST2    KKSTDT
     C******************** MOVE SCPSTB    KKAGT#
     C                   MOVE(P)   SCPST2        KKAGT#
     C                   MOVE(P)   SCPSTB        SRDTI
     C***  Begin Chg  ***  11/05/04  **************************************
     C**********           CALL 'SRCVTDAT'DTCYMD
     C                   CALL      'SRCVTMY'     DTCYMD
     C***  End   Chg  ***  11/05/04  **************************************
     C                   MOVEL     SRDTO         KKSTDT
      *** END CHG *** 08/24/01 ****************************************
     C***  Begin Add  ***  11/05/04  **************************************
     C     SGLAGT        IFEQ      *ON
     C                   MOVE      @@AGT#        KKAGT#
     C                   ENDIF
     C***  End   Add  ***  11/05/04  **************************************
     C                   ENDSL
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCSTDT        KKSTDT
     C                   Z-ADD     SCAGT#        KKAGT#
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     ASSTDT        SCSTDT
     C                   Z-ADD     ASAGT#        SCAGT#
     C*
     C                   Z-ADD     ASSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY                 99
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'RPMY'        EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         V1STDT
     C                   MOVEL     ##NUM         V2STDT
     C*
     C                   MOVEL(P)  ASAGT#        ##NUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         V1AGT#
     C                   MOVEL     ##NUM         V2AGT#
     C*
     C                   Z-ADD     ASDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY                  99
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE6'       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I1DUE
     C*
     C                   MOVEL(P)  ASBILL        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I1BILL
     C*
     C                   MOVEL(P)  ASPAID        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I1PAID
     C*
     C                   MOVEL(P)  ASBAL         ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I1BAL
     C*
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C                   MOVEL(P)  VIEW1         SCDATA
     C     *IN22         WHENEQ    *ON
     C                   MOVEL(P)  VIEW2         SCDATA
     C                   ENDSL
     C*
     C                   MOVE      INFO1         SCDATA
     C*
     C                   CALL      AGTAPI        AGTPL                  99
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OPT7   - Processing for Opt 7
     C*================================================================
     C*
     C     OPT7          BEGSR
     C*
     C  N09              MOVE      ' '           ##MODE
     C   09              MOVE      '1'           ##MODE
     C*
     C                   CALL      'W3103R'                             99
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C*
     C                   ENDSR                                                  OPT7
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '7'           OPT7
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C     Q1PRMS        IFGE      3
     C                   MOVE      *INKC         @@KC
     C                   ENDIF                                                  Q1PRMS>=3
     C*
     C     Q1PRMS        IFGE      4
     C                   MOVE      *INKL         @@KL
     C                   ENDIF                                                  Q1PRMS>=4
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   CLEAR                   @@CALL
     C*
     C***  Begin Add  ***  01/07/08  **************************************
     C*
     C     CLTID         IFEQ      '132'
     C                   MOVEL     *OFF          SRCWDW
     C                   ENDIF
     C*
     C***  End   Add  ***  01/07/08  **************************************
     C***** Begin Add *** 04/21/17 ****************************************
     C*
     C     CLTID         IFEQ      '089'
     C                   Eval      @incc = *Hival
     C                   ENDIF
     C***** End   Add *** 04/21/17 ****************************************
     C*
     C     Q1PRMS        IFGE      1
     C                   MOVE      @#CALL        @@CALL
     C                   ENDIF                                                  Q1PRMS>=1
     C*
     C***  Begin Add  ***  11/05/04  **************************************
     C                   MOVE      *OFF          SGLAGT            1
     C     Q1PRMS        IFGE      2
     C     @@AGT#        ANDNE     *BLANKS
     C     @@AGT#        ANDNE     *ZEROS
     C                   MOVE      *ON           SGLAGT
     C                   MOVE      @@AGT#        KKAGT#
     C                   ENDIF                                                  Q1PRMS>=2
     C                   MOVE      SGLAGT        *IN98
     C***  End   Add  ***  11/05/04  **************************************
     C*
     C     @@CALL        COMP      '1'                                    09
     C*
     C                   MOVE      *ON           *IN12
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C***  Begin Chg  ***  11/05/04  **************************************
     C***21***** ##CO#     READEDBRCDF                   95
     C***22***** ##CO#     READEDBRCDF2                  95
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     SGLAGT        ANDEQ     *ON
     C     KEY2S         READE     DBRCDF2                                95
     C                   Z-ADD     DBRRN2        DBRRN
     C     *IN21         WHENEQ    *ON
     C     ##CO#         READE     DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     ##CO#         READE     DBRCDF2                                95
     C                   ENDSL
     C***  End   Chg  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C***  Begin Chg  ***  11/05/04  **************************************
     C***21***** ##CO#     REDPEDBRCDF                   95
     C***22***** ##CO#     REDPEDBRCDF2                  95
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     SGLAGT        ANDEQ     *ON
     C     KEY2S         READPE    DBRCDF2                                95
     C                   Z-ADD     DBRRN2        DBRRN
     C     *IN21         WHENEQ    *ON
     C     ##CO#         READPE    DBRCDF                                 95
     C     *IN22         WHENEQ    *ON
     C     ##CO#         READPE    DBRCDF2                                95
     C                   ENDSL
     C***  End   Chg  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C***  Begin Chg  ***  11/05/04  **************************************
     C***21***** KEY1      SETGTDBRCDF               96
     C***22***** KEY2      SETGTDBRCDF2              96
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     SGLAGT        ANDEQ     *ON
     C     KEY2          SETGT     DBRCDF2                            96
     C     *IN21         WHENEQ    *ON
     C     KEY1          SETGT     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY2          SETGT     DBRCDF2                            96
     C                   ENDSL
     C***  End   Chg  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C***  Begin Chg  ***  11/05/04  **************************************
     C***21***** KEY1      SETLLDBRCDF               96
     C****BEG*CHG *** 08/24/01 ****************************************
     C***21******KEY2***** SETLLDBRCDF2              96
     C***22***** KEY2      SETLLDBRCDF2              96
     C****END*CHG *** 08/24/01 ****************************************
     C                   SELECT
     C     *IN21         WHENEQ    *ON
     C     SGLAGT        ANDEQ     *ON
     C     KEY2          SETLL     DBRCDF2                            96
     C     *IN21         WHENEQ    *ON
     C     KEY1          SETLL     DBRCDF                             96
     C     *IN22         WHENEQ    *ON
     C     KEY2          SETLL     DBRCDF2                            96
     C                   ENDSL
     C***  End   Chg  ***  11/05/04  **************************************
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DLT           SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C** **                Z-ADDSCDBRR    @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSAGT#        IFNE      *ZERO
     C     SSAGT#        ANDNE     ASAGT#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSAGT#<>*ZERO
     C*
     C     SSSTDT        IFNE      *ZERO
     C     S5STDT        ANDNE     ASSTDT
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSSTDT<>*ZERO
     C*
     C     SSBAL         IFEQ      'Y'
     C     ASBAL         ANDEQ     *ZERO
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSBAL=Y
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Month/Yr Agent  Due Date          Billed             Paid         Balance
Opt Agent  Month/Yr Due Date          Billed             Paid         Balance
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1077=Detail                 YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
211F11=Change view          YYY
212F12=Cancel               NYY
215F15=Fold/Truncate        YYY
217F17=Subset               YYY
3
