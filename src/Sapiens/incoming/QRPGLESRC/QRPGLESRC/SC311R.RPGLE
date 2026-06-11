     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*****************************************************************
     H/TITLE SC311R - WORK WITH CUSTOM GROUP ID FIELDS
     H*****************************************************************
     H**   PROGRAM DESCRIPTION:                                      **
     H**     This program displays and maintains the Custom Group ID **
     H**     Code fields.  This program processes only one record and**
     H**     creates all the subfile records from that one record.   **
     H**     Since it still uses the work with skeleton, all pro-    **
     H**     cessing is done in the MVDBSC routine and controlled    **
     H**     with loops and indicators.  The Work With is prevented  **
     H**     from reading additional records with SNGITM and turning **
     H**     off *IN21.                                              **
     H**                                                             **
     H**   CALLED FROM:                                              **
     H**     MENU   - Work with Custom Group ID Codes - option 7     **
     H**   CALLS TO:                                                 **
     H**     SC311R01 - Custom Group ID Codes field information      **
     H**     SC315R - Table Back-up                                  **
     H**                                                             **
     H**   PROGRAMMER: C:86                                          **
     H**   CREATED:   01/28/98      PROJECT: DR048                   **
     H*****************************************************************
     H** REVISIONS:
     H*
     H* 02/01/01 C:33 PROJECT: DR143
     H*               ADD GROUP TO SCREEN.
     H*
     H* 12/28/99 C:62 PROJECT:PR001
     H*               SWAP OUT WA001R WITH SA005R
     H*
     H*  08/06/98  C:94  PROJECT: DR049
     H*                  CHANGED *ENTRY PARMS SO THAT F3 WOULD GO TO
     H*                  MENU
     H*****************************************************************
     H** INDICATOR USAGE:                                            **
     H**   INDICATOR            FUNCTION DESCRIPTION                 **
     H**      01   Used on the PAGEDOWN keyword
     H**      02   Used on the PAGEUP keyword
     H**      03   Used to condition SFLCLR
     H**     N03   Used to condition SFLDSPCTL
     H**      04   Used to condition SFLDSP
     H**      05   Used to condition DSPATR(RI) on the sel field
     H**     N06   Used to condition DSPATR(PC) on the pos to field
     H**      07   Used to condition DSPATR(PC) on the sel field
     H**      08   Used to condition SFLNXTCHG
     H**      09   Reserved for maintenance/inquiry conditions
     H**     N10   Used to condition SFLEND (on message subfile)
     H**      11   Used to indicate the list is subsetted
     H**      12   Used to condition SFLDROP/SFLFOLD
     H**      13   Used to condition SFLCLR (on confirm delete)
     H**     N13   Used to condition SFLDSPCTL (on confirm delete)
     H**      14   Used to condition SFLDSP (on confirm delete)
     H**      15   Used to condition SFLEND (on confirm delete)
     H**      21   View 1
     H**      22 - 29 are reserved for other views
     H**      60   Used for chaining
     H**      75 - 88 are reserved for error screen error indicators
     H**      89   Used to position cursor after search
     H**      90   Used as general purpose error indicator
     H**      91   Used to display CPF5203; roll up/down past last rcd
     H**      92   Used to condition SFLMSGID for more than one opt 1
     H**      95   Used to condition SFLEND; also used to indicate EOF
     H**           and BOF on the file being read.
     H**      99   Used for misc purpose; error on calls, on CHAINs
     H**
     H*****************************************************************
     FSC311F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMCGP     IF   E             DISK
     F                                     RENAME(PCG01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F***  BEGIN CHG  ***  02/01/01  *********************************
     F**********SMCGL   IF  E           K        DISK
     FSMCGL1    IF   E           K DISK
     F***  END   CHG  ***  02/01/01  *********************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PCG01:DBRCDF)
     F***  BEGIN ADD  ***  02/01/01  *********************************
     FSSYLF     IF   E           K DISK
     F***  END   ADD  ***  02/01/01  *********************************
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(2) CTDATA PERRCD(1)              Column headings
     D FN              S              4    DIM(20) CTDATA PERRCD(20)            FIELD NAMES
     D AT              S              6    DIM(20) CTDATA PERRCD(10)            ATTRIBUTES
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D*
     D*** Arrays for 10A/6.0N/9.2N fields
     D*
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Custom Group ID Fiel-
     D                                     ds')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('SCMESG')
     D ACDPGM          C                   CONST('SC311R01')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(9)
     D*================================================================
     D*
     D                 DS
     D  DS                     1    200
     D                                     DIM(20)                              USR ASGN FLD DSC
     D  CGDA01                 1     10
     D  CGDA02                11     20
     D  CGDA03                21     30
     D  CGDA04                31     40
     D  CGDA05                41     50
     D  CGDA06                51     60
     D  CGDA07                61     70
     D  CGDA08                71     80
     D  CGDA09                81     90
     D  CGDA10                91    100
     D  CGDD01               101    110
     D  CGDD02               111    120
     D  CGDD03               121    130
     D  CGDD04               131    140
     D  CGDD05               141    150
     D  CGDN01               151    160
     D  CGDN02               161    170
     D  CGDN03               171    180
     D  CGDN04               181    190
     D  CGDN05               191    200
     D*
     D                 DS
     D  TL                     1     20
     D                                     DIM(20)                              TABLE BACKUP FLDS
     D  CGTA01                 1      1
     D  CGTA02                 2      2
     D  CGTA03                 3      3
     D  CGTA04                 4      4
     D  CGTA05                 5      5
     D  CGTA06                 6      6
     D  CGTA07                 7      7
     D  CGTA08                 8      8
     D  CGTA09                 9      9
     D  CGTA10                10     10
     D  CGTD01                11     11
     D  CGTD02                12     12
     D  CGTD03                13     13
     D  CGTD04                14     14
     D  CGTD05                15     15
     D  CGTN01                16     16
     D  CGTN02                17     17
     D  CGTN03                18     18
     D  CGTN04                19     19
     D  CGTN05                20     20
     D*
     D                 DS
     D  SL                     1     60  0
     D                                     DIM(20)                              SECURITY LEVELS
     D  CGSA01                 1      3  0
     D  CGSA02                 4      6  0
     D  CGSA03                 7      9  0
     D  CGSA04                10     12  0
     D  CGSA05                13     15  0
     D  CGSA06                16     18  0
     D  CGSA07                19     21  0
     D  CGSA08                22     24  0
     D  CGSA09                25     27  0
     D  CGSA10                28     30  0
     D  CGSD01                31     33  0
     D  CGSD02                34     36  0
     D  CGSD03                37     39  0
     D  CGSD04                40     42  0
     D  CGSD05                43     45  0
     D  CGSN01                46     48  0
     D  CGSN02                49     51  0
     D  CGSN03                52     54  0
     D  CGSN04                55     57  0
     D  CGSN05                58     60  0
     D*
     D                 DS
     D  ME                     1     20
     D                                     DIM(20)                              MANDATORY ENTRY FIEL
     D  CGMA01                 1      1
     D  CGMA02                 2      2
     D  CGMA03                 3      3
     D  CGMA04                 4      4
     D  CGMA05                 5      5
     D  CGMA06                 6      6
     D  CGMA07                 7      7
     D  CGMA08                 8      8
     D  CGMA09                 9      9
     D  CGMA10                10     10
     D  CGMD01                11     11
     D  CGMD02                12     12
     D  CGMD03                13     13
     D  CGMD04                14     14
     D  CGMD05                15     15
     D  CGMN01                16     16
     D  CGMN02                17     17
     D  CGMN03                18     18
     D  CGMN04                19     19
     D  CGMN05                20     20
     D*
     D                 DS
     D  EV                     1     20
     D                                     DIM(20)                              EDIT VALUES
     D  CGED01                11     11
     D  CGED02                12     12
     D  CGED03                13     13
     D  CGED04                14     14
     D  CGED05                15     15
     D  CGEN01                16     16
     D  CGEN02                17     17
     D  CGEN03                18     18
     D  CGEN04                19     19
     D  CGEN05                20     20
     D*
     D                 DS
     D  EP                     1    200
     D                                     DIM(20)                              EDIT PROGRAMS
     D  CGPA01                 1     10
     D  CGPA02                11     20
     D  CGPA03                21     30
     D  CGPA04                31     40
     D  CGPA05                41     50
     D  CGPA06                51     60
     D  CGPA07                61     70
     D  CGPA08                71     80
     D  CGPA09                81     90
     D  CGPA10                91    100
     D  CGPD01               101    110
     D  CGPD02               111    120
     D  CGPD03               121    130
     D  CGPD04               131    140
     D  CGPD05               141    150
     D  CGPN01               151    160
     D  CGPN02               161    170
     D  CGPN03               171    180
     D  CGPN04               181    190
     D  CGPN05               191    200
     D*
     D                 DS
     D  DB                     1     60
     D                                     DIM(6)                               USR ASGN FLD DSC
     D  CGDB01                 1     10
     D  CGDB02                11     20
     D  CGDB03                21     30
     D  CGDB04                31     40
     D  CGDB05                41     50
     D  CGDB06                51     60
     D*
     D                 DS
     D  TB                     1      6
     D                                     DIM(6)                               TABLE BACKUP FLDS
     D  CGTB01                 1      1
     D  CGTB02                 2      2
     D  CGTB03                 3      3
     D  CGTB04                 4      4
     D  CGTB05                 5      5
     D  CGTB06                 6      6
     D*
     D                 DS
     D  SB                     1     18  0
     D                                     DIM(6)                               SECURITY LEVELS
     D  CGSB01                 1      3  0
     D  CGSB02                 4      6  0
     D  CGSB03                 7      9  0
     D  CGSB04                10     12  0
     D  CGSB05                13     15  0
     D  CGSB06                16     18  0
     D*
     D                 DS
     D  MB                     1      6
     D                                     DIM(6)                               MANDATORY ENTRY FIEL
     D  CGMB01                 1      1
     D  CGMB02                 2      2
     D  CGMB03                 3      3
     D  CGMB04                 4      4
     D  CGMB05                 5      5
     D  CGMB06                 6      6
     D*
     D                 DS
     D  PB                     1     60
     D                                     DIM(6)                               EDIT PROGRAMS
     D  CGPB01                 1     10
     D  CGPB02                11     20
     D  CGPB03                21     30
     D  CGPB04                31     40
     D  CGPB05                41     50
     D  CGPB06                51     60

     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1            Blank = Maint
     C*                                                    '1' = Inquiry
     C*                                                    '2' = Search
     C                   PARM                    @@CSID
     C                   PARM                    @@RTCD            1
     C*** BEG ADD *** 08/06/98 **************************************
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*** END ADD *** 08/06/98 **************************************
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM                    @@GRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM      '0'           ##KC              1
     C     *INKL         PARM      '0'           ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      SCFLD         ##FLD             4
     C                   PARM      SCATTR        ##ATTR            6
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FND   30
     C*** END   DEL *** 12/28/99 **************************************
     C                   PARM                    ##CONM           40
     C*** BEGIN DEL *** 12/28/99 **************************************
     C**********           PARM           ##FNAM 40
     C*** END   DEL *** 12/28/99 **************************************
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C     KEY1          KLIST
     C                   KFLD                    KKCSID
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   KFLD                    KKGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    CGCSID        KKCSID
     C     *LIKE         DEFINE    CGCSID        @@CSID
     C     *LIKE         DEFINE    CGRFMT        KKRFMT
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C     *LIKE         DEFINE    CGGRP         KKGRP
     C     *LIKE         DEFINE    CGGRP         @@GRP
     C     *LIKE         DEFINE    DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C     CHKSSM        BEGSR
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
     C*
     C                   MOVE      @@CSID        KKCSID
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   KKGRP
     C*
     C     Q1PRMS        IFGE      6
     C                   MOVE      @@GRP         KKGRP
     C                   ENDIF                                                  Q1PRMS>=6
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C                   MOVEL     *OFF          *IN21
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGGRP         SCGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   MOVEL     CGCSID        SCCSID
     C                   MOVEL     CGDESC        SCDESC
     C                   MOVEL     CGRFMT        SCRFMT
     C                   MOVEL     CGRCDS        SCSECL
     C                   MOVEL     CGSTAT        SCSTAT
     C     SNGITM        IFEQ      *OFF
     C     SCRFMT        IFEQ      'B'
     C                   MOVE      *ON           *IN87
     C                   MOVEL(P)  HD(2)         SCHDL1
     C                   EXSR      LOADB                                        LOAD RCDS FROM FILE
     C                   ELSE
     C                   EXSR      LOADA                                        LOAD RCDS FROM FILE
     C                   MOVEL(P)  HD(1)         SCHDL1
     C                   ENDIF
     C                   ELSE
     C     SCRFMT        IFEQ      'B'
     C                   MOVE      *ON           *IN87
     C                   MOVEL(P)  HD(2)         SCHDL1
     C                   EXSR      LOADB2                                       LOAD RCDS FROM FILE
     C                   ELSE
     C                   EXSR      LOADA2                                       LOAD RCDS FROM FILE
     C                   ENDIF
     C                   ENDIF                                                  FORMAT = B
     C                   MOVE      *ON           SNGITM
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADA SUBROUTINE                                    L O A D A ***
     C*====================================================================*
     C     LOADA         BEGSR
     C                   DO        20            Q                 2 0
     C                   MOVEL     FN(Q)         SCFLD
     C                   MOVEL     AT(Q)         SCATTR
     C                   MOVEL     DS(Q)         SCHEAD
     C                   MOVEL     TL(Q)         SCTBUP
     C                   MOVEL     SL(Q)         SCSCLV
     C                   MOVEL     ME(Q)         SCMANE
     C                   MOVEL     EP(Q)         SCEDTP
     C                   MOVEL     EV(Q)         SCEDTV
     C*
     C     Q             IFNE      20
     C                   WRITE     SCR01S
     C                   ADD       1             SFRRN1               04
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADA2 SUBROUTINE                              L O A      D A ***
     C*====================================================================*
     C     LOADA2        BEGSR
     C                   MOVE      SFRRN1        P                 2 0
     C                   MOVEL     FN(P)         SCFLD
     C                   MOVEL     AT(P)         SCATTR
     C                   MOVEL     DS(P)         SCHEAD
     C                   MOVEL     TL(P)         SCTBUP
     C                   MOVEL     SL(P)         SCSCLV
     C                   MOVEL     ME(P)         SCMANE
     C                   MOVEL     EP(P)         SCEDTP
     C                   MOVEL     EV(P)         SCEDTV
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADB SUBROUTINE  - USED TO REFRESH                       D B ***
     C*====================================================================*
     C     LOADB         BEGSR
     C                   DO        6             B                 2 0
     C                   MOVE      B             R                 2
     C     'UB'          CAT       R             SCFLD
     C                   MOVEL     '35   A'      SCATTR
     C                   MOVEL     DB(B)         SCHEAD
     C                   MOVEL     TB(B)         SCTBUP
     C                   MOVEL     SB(B)         SCSCLV
     C                   MOVEL     MB(B)         SCMANE
     C                   MOVEL     PB(B)         SCEDTP
     C*
     C     B             IFNE      6
     C                   WRITE     SCR01S
     C                   ADD       1             SFRRN1               04
     C                   ENDIF
     C*
     C                   ENDDO
     C                   ENDSR
     C*====================================================================*
     C* * *   LOADB2 SUBROUTINE  - USED TO REFRESH                      D B ***
     C*====================================================================*
     C     LOADB2        BEGSR
     C                   MOVE      SFRRN1        B                 2 0
     C                   MOVE      B             R                 2
     C     'UB'          CAT       R             SCFLD
     C                   MOVEL     '35   A'      SCATTR
     C                   MOVEL     DB(B)         SCHEAD
     C                   MOVEL     TB(B)         SCTBUP
     C                   MOVEL     SB(B)         SCSCLV
     C                   MOVEL     MB(B)         SCMANE
     C                   MOVEL     PB(B)         SCEDTP
     C                   ENDSR
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C                   ENDSR
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C     OTHOPT        BEGSR
     C*
     C*          Process option 7 - TABLE BACK-UP
     C*          ------------------------------------------------------
     C     SCSEL         IFEQ      '7'
     C     SCTBUP        ANDEQ     'Y'
     C                   MOVE      SCCSID        QQCSID            2
     C                   MOVE      SCFLD         QQFLD             4
     C                   MOVE      *BLANK        QQRTC             1
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   MOVE      SCGRP         QQGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C                   CALL      'SC315R'
     C                   PARM      @@CALL        ##CALL            1
     C                   PARM                    QQCSID
     C                   PARM                    QQFLD
     C                   PARM                    QQRTC
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   PARM                    QQGRP             3 0
     C***  END   ADD  ***  02/01/01  *********************************
     C                   ELSE
     C                   MOVE      *ON           *IN75
     C                   ENDIF                                                  SEL = '7'
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
     C*
     C***  BEGIN ADD  ***  02/01/01  *********************************
     C                   CLEAR                   DRFL11
     C     'SC'          CHAIN     SDR01                              60
     C     DRFL11        COMP      'Y'                                    41
     C                   MOVEL     DRFL11        USEGRP
     C***  END   ADD  ***  02/01/01  *********************************
     C*
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C***  BEGIN CHG  ***  02/01/01  *********************************
     C********** KKCSID    CHAINDBRCDF               95
     C     KEY1          CHAIN     DBRCDF                             95
     C***  END   CHG  ***  02/01/01  *********************************
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
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
     C                   ENDSR
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C     SUBSET        BEGSR
     C                   ENDSR
** Column headings
Opt Field  Attrib   Heading    Back-up  Level      Entry     Program     Value
Opt Field  Attrib   Heading    Back-up  Level      Entry     Program
** FN = FIELD NAMES
UA01UA02UA03UA04UA05UA06UA07UA08UA09UA10UD01UD02UD03UD04UD05UN01UN02UN03UN04UN05
** AT = FIELD ATTRIBUTES
10   A10   A10   A10   A10   A10   A10   A10   A10   A10   A
 7/0 N 7/0 N 7/0 N 7/0 N 7/0 N 9/2 N 9/2 N 9/2 N 9/2 N 9/2 N         +5
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 NNN
1055=Display                YYN
1077=Table back-up          YNN
203F3=Exit                  YYY
205F5=Refresh               NNN
206F6=Add                   NNN
210F10=Search               NNN
212F12=Cancel               YYY
217F17=Subset               NNN
3
