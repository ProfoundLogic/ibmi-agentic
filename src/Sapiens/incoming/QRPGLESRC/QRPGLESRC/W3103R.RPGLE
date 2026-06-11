     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W3103R - W/W AGENCY BILLING DETAIL
     H*****************************************************************
     H* PROGRAM DESCRIPTION:                                          *
     H*                                                               *
     H* THIS PROGRAM DISPLAYS THE AGENCY BILLING DETAIL INFORMATION   *
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
     H*  01/20/00  C:33  PROJECT: WC899                               *
     H*                  DO NOT ALLOW FOR DELETION OF CHK TYPE RECORDS*
     H*                                                               *
     H*  12/15/99  C:33  PROJECT: WC899                               *
     H*                  REMOVE OPTION 2=CHANGE.                      *
     H*****************************************************************
     FW3103F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FWDABP     IF   E             DISK
     F                                     RENAME(PAB01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FWDABL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAB01:DBRCDF)
     FWSASP     IF   E             DISK
     FWMEML     IF   E           K DISK
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
     D  VIEW1                  1     19
     D  V1FND                  1      3
     D  V1EMP#                 5     13
     D  V1DIV                 15     19
     D                 DS
     D  INFO1                  1     54
     D  I1ARCD                 2      3
     D  I1AMT                  6     19
     D  I1RATE                20     26
     D  I1COMM                27     40
     D  I1NET                 41     53
     D  I1MADJ                54     54
     D                 DS
     D  INFO2                  1     54
     D  I2ARCD                 2      3
     D  I2TRDT                 6     15
     D  I2PODT                17     26
     D  I2AMT                 28     42
     D  I2MADJ                54     54
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM           C                   CONST('Agency Billing Detai-
     D                                     l')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D DLT             C                   CONST('* * * Deleted')
     D ACDPGM          C                   CONST('W3103R02')
     D COMAPI          C                   CONST('WA001R')
     D AGTAPI          C                   CONST('WA002R')
     D USPAPI          C                   CONST('W0006R')
     D CHGNUM          C                   CONST('SA020R')
     D INFOMX          C                   CONST(2)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      ASRRN         ##RRN1            9 0
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    ##NUM            80
     C                   PARM                    EDTMSK           10
     C*
     C     AGTPL         PLIST
     C                   PARM      ASCO#         ##CO#
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
     C     KEY           KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    ASSTDT
     C                   KFLD                    ASAGT#
     C*
     C     KEY1          KLIST
     C                   KFLD                    ASCO#
     C                   KFLD                    ASSTDT
     C                   KFLD                    ASAGT#
     C                   KFLD                    KKFND
     C                   KFLD                    KKEMP#
     C                   KFLD                    KKDIV
     C                   KFLD                    KKFYR
     C                   KFLD                    KKDT
     C                   KFLD                    KKTIME
     C*
     C     EMKY          KLIST
     C                   KFLD                    ABCO#
     C                   KFLD                    ABFND
     C                   KFLD                    ABEMP#
     C                   KFLD                    ABDIV
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    @@RRN         ASRRN
     C     *LIKE         DEFINE    ABFND         KKFND
     C     *LIKE         DEFINE    ABEMP#        KKEMP#
     C     *LIKE         DEFINE    ABDIV         KKDIV
     C     *LIKE         DEFINE    ABFYR         KKFYR
     C     *LIKE         DEFINE    ABDT          KKDT
     C     *LIKE         DEFINE    ABTIME        KKTIME
     C     *LIKE         DEFINE    I2AMT         XXAMT
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSEMP#        ORNE      *ZEROS
     C     SSDIV         ORNE      *ZEROS
     C     SSARCD        ORNE      *BLANKS
     C     SSMADJ        ORNE      *BLANKS
     C     SSOPEN        ORNE      *BLANKS
     C                   MOVE      *ON           *IN11
     C                   ENDIF                                                  SSFND<>*ZEROS
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* GETAS  - Get WSASP record and load header
     C*================================================================
     C*
     C     GETAS         BEGSR
     C*
     C     ASRRN         CHAIN     WSASP                              60
     C*
     C                   Z-ADD     ASAGT#        SCAGT#
     C*
     C                   CALL      AGTAPI        AGTPL                  99
     C*
     C                   Z-ADD     ASSTDT        SRDTI
     C                   CALL      'SRCVTMY'     DTMDYY                 99
     C                   MOVEL     SRDTO         SCSTDT
     C*
     C                   Z-ADD     ASDUE         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                 99
     C                   MOVEL     SRDTO         SCDUE
     C*
     C                   Z-ADD     ASBILL        SCBILL
     C                   Z-SUB     ASPAID        SCPAID
     C                   Z-ADD     ASBAL         SCBAL
     C*
     C                   ENDSR                                                  GETAS
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C     SCOPEN        COMP      'Y'                                    31
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     SCSEL         WHENEQ    '2'
     C*
     C     SCMADJ        IFNE      'Y'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCMADJ<>Y
     C*_______________________________________________________________
     C*
     C     SCSEL         WHENEQ    '4'
     C*
     C     SCMADJ        IFNE      'Y'
     C                   SETON                                        71  90
     C                   ENDIF                                                  SCMADJ<>Y
     C*
     C***  BEGIN ADD   ***  01/20/00  ********************************
     C  N90SCARTY        IFEQ      'P'
     C     SCARTY        OREQ      'V'
     C     SCARTY        OREQ      'R'
     C                   SETON                                        72  90
     C                   ENDIF                                                  SCARTY=P/V/R
     C***  END   ADD   ***  01/20/00  ********************************
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C                   CLEAR                   SSFND
     C                   CLEAR                   SSEMP#
     C                   CLEAR                   SSDIV
     C                   CLEAR                   SSARCD
     C                   CLEAR                   SSMADJ
     C                   CLEAR                   SSOPEN
     C*
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKFND
     C                   CLEAR                   KKEMP#
     C                   CLEAR                   KKDIV
     C                   CLEAR                   KKFYR
     C                   CLEAR                   KKDT
     C                   CLEAR                   KKTIME
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   MOVE      SCPST1        KKFND
     C                   MOVE      SCPS1A        KKEMP#
     C                   MOVE      SCPS1B        KKDIV
     C*
     C                   CLEAR                   KKFYR
     C                   CLEAR                   KKDT
     C                   CLEAR                   KKTIME
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCEMP#        KKEMP#
     C                   Z-ADD     SCDIV         KKDIV
     C                   Z-ADD     SCFYR         KKFYR
     C                   Z-ADD     SCDT          KKDT
     C                   Z-ADD     SCTIME        KKTIME
     C*
     C                   ENDSR                                                  KEYSFL
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     ABFND         SCFND
     C                   Z-ADD     ABEMP#        SCEMP#
     C                   Z-ADD     ABDIV         SCDIV
     C                   Z-ADD     ABFYR         SCFYR
     C                   Z-ADD     ABDT          SCDT
     C                   Z-ADD     ABTIME        SCTIME
     C                   MOVEL     ABMADJ        SCMADJ
     C                   MOVE      ABOPEN        SCOPEN
     C***  BEGIN ADD  ***  01/20/00  *********************************
     C                   MOVE      ABARTY        SCARTY
     C***  END   ADD  ***  01/20/00  *********************************
     C*
     C                   CLEAR                   VIEW1
     C                   CLEAR                   INFO1
     C                   CLEAR                   INFO2
     C*
     C                   CLEAR                   EMNAME
     C     EMKY          CHAIN     PEM01                              60
     C                   MOVEL(P)  EMNAME        SCENAM
     C*
     C                   MOVEL(P)  ABFND         ##NUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         V1FND
     C*
     C                   MOVEL(P)  ABEMP#        ##NUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         V1EMP#
     C*
     C                   MOVEL(P)  ABDIV         ##NUM
     C                   MOVEL(P)  *BLANKS       EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         V1DIV
     C*
     C                   MOVEL     ABARCD        I1ARCD
     C                   MOVEL     ABARCD        I2ARCD
     C*
     C                   MOVEL(P)  ABAMT         ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         XXAMT
     C                   MOVE      XXAMT         I1AMT
     C                   MOVE      XXAMT         I2AMT
     C*
     C                   MOVEL(P)  ABRATE        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I1RATE
     C*
     C                   MOVEL(P)  ABCOMM        ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         XXAMT
     C                   MOVE      XXAMT         I1COMM
     C*
     C                   MOVEL(P)  ABNET         ##NUM
     C                   MOVEL(P)  'X,XXX.XX'    EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         XXAMT
     C                   MOVE      XXAMT         I1NET
     C*
     C                   Z-ADD     ABTRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                 99
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I2TRDT
     C*
     C                   Z-ADD     ABPODT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY                 99
     C                   MOVEL(P)  SRDTO         ##NUM
     C                   MOVEL(P)  'DATE'        EDTMSK
     C                   CALL      CHGNUM        CHGPL                  99
     C                   MOVEL     ##NUM         I2PODT
     C*
     C     ABMADJ        IFEQ      'Y'
     C                   MOVEL     'M'           I1MADJ
     C                   MOVEL     'M'           I2MADJ
     C                   ENDIF                                                  ABMADJ=Y
     C*
     C     ABOPEN        COMP      'Y'                                    31
     C*
     C                   SELECT
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     1
     C                   MOVEL(P)  VIEW1         SCDATA
     C                   MOVE      INFO1         SCDATA
     C     VIEW#         WHENEQ    1
     C     INFO#         ANDEQ     2
     C                   MOVEL(P)  VIEW1         SCDATA
     C                   MOVE      INFO2         SCDATA
     C                   ENDSL
     C*
     C                   EXSR      GETAS
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OPT7   - Processing for Option 7
     C*================================================================
     C*
     C     OPT7          BEGSR
     C*
     C* OPTION 7=OPEN ITEM
     C*
     C                   MOVE      'O'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
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
     C     SCSEL         CASEQ     '7'           OPT7                     90
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C     @@CALL        COMP      '1'                                    09
     C*
     C                   MOVE      *ON           *IN12
     C*
     C                   Z-ADD     @@RRN         ASRRN
     C                   CLEAR                   @@RRN
     C*
     C                   EXSR      GETAS
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C                   MOVE      *OFF          *IN31
     C*
     C   21KEY           READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C                   MOVE      *OFF          *IN31
     C*
     C   21KEY           READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
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
     C                   Z-ADD     SCDBRR        @@RRN
     C*
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C     SSFND         IFNE      *ZEROS
     C     SSFND         ANDNE     ABFND
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSFND<>*ZEROS
     C*
     C     SSEMP#        IFNE      *ZEROS
     C     SSEMP#        ANDNE     ABEMP#
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSEMP#<>*ZEROS
     C*
     C     SSDIV         IFNE      *ZEROS
     C     SSDIV         ANDNE     ABDIV
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSDIV<>*ZEROS
     C*
     C     SSARCD        IFNE      *BLANKS
     C     SSARCD        ANDNE     ABARCD
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSARCD<>*BLANKS
     C*
     C     SSMADJ        IFNE      *BLANKS
     C     SSMADJ        ANDNE     ABMADJ
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSMADJ<>*BLANKS
     C*
     C     SSOPEN        IFNE      *BLANKS
     C     SSOPEN        ANDNE     ABOPEN
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF                                                  SSOPEN<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
     C*
** Column headings
Opt Grp  Employer   Div Code        Amount   Rate   Comm Amount  Net Amt Due
Opt Grp  Employer   Div Code Trans Date  Post Date         Amount
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 YNN
1055=Display                YYY
1077=Open Item              YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
209F9=Change info           YYY
212F12=Cancel               YYY
215F15=Fold/truncate        YYY
217F17=Subset               YYY
3
