     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H****************************************************************
     H/TITLE W7173R - WORK WITH SPOOL FILES
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL        *
     H*                THE SPOOL FILES FOR SELECTED USER              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W7172R -                                                   *
     H*  CALLS TO:                                                    *
     H*                                                               *
     H*  CREATED: 06/27/06  PROGRAMMER: C:168 PROJECT: 129WNN         *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  09/27/06  C:33   Seq#: WA129/54958                           *
     H*                   Subset needs to display not-printed records.*
     H*                                                               *
     H*  08/24/06  C:33  Project: 129WNN    WA129/54358               *
     H*                  Allow default subset to only show those items*
     H*                   not printed.                                *
     H*****************************************************************
     H*  INDICATOR USAGE:                                             *
     H*                                                               *
     H*  01 - Used on the PAGEDOWN keyword                            *
     H*  02 - Used on the PAGEUP keyword                              *
     H*  03 - Used to condition SFLCLR                                *
     H* N03 - Used to condition SFLDSPCTL                             *
     H*  04 - Used to condition SFLDSP                                *
     H*  05 - Used to condition DSPATR(RI) on the selection field     *
     H* N06 - Used to condition DSPATR(PC) on the position to field   *
     H*  07 - Used to condition DSPATR(PC) on the selection field     *
     H*  08 - Used to condition SFLNXTCHG                             *
     H*  09 - INQUIRY                                                 *
     H* N09 - UPDATE                                                  *
     H* N10 - Used to condition SFLEND (on message subfile)           *
     H*  11 - Used to indicate the list is subsetted                  *
     H*  12 - Used to condition SFLDROP/SFLFOLD                       *
     H*  13 - Used to condition SFLCLR (on confirm delete)            *
     H* N13 - Used to condition SFLDSPCTL (on confirm delete)         *
     H*  14 - Used to condition SFLDSP (on confirm delete)            *
     H*  15 - Used to condition SFLEND (on confirm delete)            *
     H*  21 - Number Order Search                                     *
     H*  30 - Called as a search                                      *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  93 - Used to display CPF6A7C; Option invalid                 *
     H*  94 - Used to display CPF5204; Function Key invalid           *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW7173F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWXAGP     IF   E             DISK    USROPN
     F                                     RENAME(PAG01:PFRCDF)
     FWXAGL1    IF   E           K DISK    USROPN
     F                                     INFDS(DBFDBA)
     F                                     RENAME(PAG01:DBRCDF)
      *  AGENT NAME
     FWMAAL5    IF   E           K DISK
     F                                     RENAME(PAA01:PAGL)
      *  User Parameters
     FSMUPL2    IF   E           K DISK
     F                                     RENAME(PUP01:PUP03)
     FWMFNL     IF   E           K DISK
     FWMEXL     IF   E           K DISK
      *  AGENT NAME
     FWMAAL1    IF   E           K DISK
     F                                     RENAME(PAA01:PAGL1)
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(2) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(12) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(12) ALT(OF)
     D BS              S              9  8 DIM(12)
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('User Spool Files')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W7174R')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D VIEWMX          C                   CONST(1)
     D INFOMX          C                   CONST(1)
     D*
     D DBFDBA          DS
     D  DBRRNA               397    400B 0

     C*
     C     ACDPL         PLIST
     C                   PARM                    ##MODE            1
     C                   PARM      SCAGRR        QQRRN             9 0
     C                   PARM      SCCO#         QQCO#             3 0
     C                   PARM      SCAGT#        QQAGT#            5 0
     C                   PARM      @@USER        QQUSER           10
     C                   PARM      SCNEWQ        QQOUTQ           10
     C                   PARM      *BLANKS       QQERR             1
     C                   PARM      *ZEROS        QQREC             9 0
      *
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  REPOSITION KEYS
     C*
     C     KEY1          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKHDR
     C*
     C     KEY1A         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    KKHDR
     C                   KFLD                    KKAGT#
     C*
     C     AGKY          KLIST
     C                   KFLD                    AGCO#
     C                   KFLD                    SCAGT#
     C*
     C     FNKY          KLIST
     C                   KFLD                    AGCO#
     C                   KFLD                    ##FND
     C*
     C     AAKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    ##FND
     C                   KFLD                    XXAGT#
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@USER           10
     C                   PARM                    @@OUTQ           10
      *
     C     *LIKE         DEFINE    AGAGT#        KKAGT#
     C     *LIKE         DEFINE    AGHDR         KKHDR
     C     *LIKE         DEFINE    AGAGT#        XXAGT#
     C                   MOVEL     *BLANKS       @@CALL            1
      *
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
      *
      *  Validates the entered Agent range
      *
      * Invalid agent 1
     C     SSAGT1        IFNE      *ZEROS
     C                   Z-ADD     SSAGT1        XXAGT#
     C     AAKY          CHAIN     WMAAL1                             99
     C   99              SETON                                        42  90
     C  N99              Z-ADD     AAAGT#        SSAGT1
     C                   ENDIF
      *
      * Invalid agent 2
     C     *IN90         IFEQ      *OFF
     C     SSAGT2        ANDNE     *ZEROS
     C                   Z-ADD     SSAGT2        XXAGT#
     C     AAKY          CHAIN     WMAAL1                             99
     C   99              SETON                                        43  90
     C  N99              Z-ADD     AAAGT#        SSAGT2
     C                   ENDIF
      *
      * Agent 1 is zero - Agent 2 not zero
     C     *IN90         IFEQ      *OFF
     C     SSAGT1        ANDEQ     *ZEROS
     C     SSAGT2        ANDNE     *ZEROS
     C                   SETON                                        42  90
     C                   ENDIF
      *
      * Agent 1 not zero- Agent 2 is zero
     C     *IN90         IFEQ      *OFF
     C     SSAGT1        ANDNE     *ZEROS
     C     SSAGT2        ANDEQ     *ZEROS
     C                   SETON                                        43  90
     C                   ENDIF
      *
      * Validates the status selected
      *
     C     SSSTAT        IFNE      *BLANKS
     C     SSSTAT        ANDNE     'M'
     C     SSSTAT        ANDNE     'P'
     C***  Begin Add  ***  08/24/06  **************************************
     C     SSSTAT        ANDNE     '*'
     C***  End   Add  ***  08/24/06  **************************************
     C                   SETON                                        44  90
     C                   ENDIF
     C*
     C                   ENDSR                                                  CHKSSM
     C*
     C*===============================================================
     C* CVTDT - SUBROUTINE TO CONVERT DATE FROM CYMD TO MDYY
     C*===============================================================
     C*
     C     CVTDT         BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*CYMD'       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT
     C*
     C*===============================================================
     C* ERRCK1 - SUBROUTINE TO CHECK SCREEN SELECTIONS
     C*===============================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   ENDSR                                                  ERRCK1
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C     ERRCK2        BEGSR
      *
     C                   ENDSR                                                  ERRCK2
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C     INZSST        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SSAGT1
     C                   Z-ADD     *ZEROS        SSAGT2
     C***  Begin Chg  ***  08/24/06  **************************************
     C**********           MOVEL*BLANKS   SSSTAT
     C                   MOVEL     '*'           SSSTAT
     C***  End   Chg  ***  08/24/06  **************************************
     C*
     C                   ENDSR                                                  INZSST
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C     KEYINZ        BEGSR
      *
     C                   MOVE      'Y'           KKHDR
     C                   MOVE      *LOVAL        KKAGT#
      *
     C                   ENDSR                                                  KEYINZ
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C     KEYSET        BEGSR
      *
     C     SCPST1        IFNE      *BLANKS
     C                   MOVE      SCPST1        KKAGT#
     C                   ELSE
     C                   MOVE      *LOVAL        KKAGT#
     C                   ENDIF
      *
     C                   ENDSR                                                  KEYSET
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C     KEYSFL        BEGSR
      *
     C                   Z-ADD     SCAGT#        KKAGT#
      *
     C                   ENDSR                                                  KEYSFL
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      *ON           *IN08
      *
     C                   Z-ADD     ##CO#         SCCO#
     C                   Z-ADD     AGAGT#        SCAGT#
     C                   MOVEL     *BLANKS       SCAGNM
     C                   Z-ADD     DBRRNA        SCAGRR
      *
     C     AGKY          CHAIN     WMAAL5                             60
     C     *IN60         IFEQ      *OFF
     C                   MOVEL     AANAME        SCAGNM
     C                   ENDIF
      *
     C                   MOVEL     AGSPLN        SCSPLN
     C                   MOVEL     AGSTAT        SCSTAT
     C                   MOVEL     AGOUTQ        SCOUTQ
      *
     C     FNKY          CHAIN     WMEXL                              60
     C     *IN60         IFEQ      *OFF
     C*                    MOVELEXAOUT    SCAOUT
     C                   ENDIF
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT20 - SUBROUTINE FOR OPTION 20
     C*===============================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
      *
     C     SCSTAT        IFNE      'P'
     C                   CLEAR                   SCNEWQ
      *
     C     *IN12         DOWEQ     *OFF
     C                   EXFMT     SCR04
     C                   SETOFF                                       44  90
      *
     C     *IN12         IFEQ      *ON
     C                   LEAVE
     C                   ENDIF
      *
      *  Validates the Outq entered by the user
      *
     C                   CALL      'S0003C'
     C                   PARM                    SCNEWQ
     C                   PARM      ' '           XXFOND            1
     C     XXFOND        IFEQ      '1'
     C                   SETON                                        44  90
     C                   ENDIF
      *
      *  No errors found, update database and leave loop
      *
     C     *IN90         IFEQ      *OFF
     C                   MOVE      'M'           ##MODE
     C                   CALL      ACDPGM        ACDPL                  99
     C                   MOVEL     'M'           SCSTAT
     C                   MOVEL     SCNEWQ        SCOUTQ
     C                   LEAVE
     C                   ENDIF
      *
     C                   ENDDO
      *
     C                   ELSE
     C                   SETON                                        41
     C                   ENDIF
     C*
     C                   ENDSR                                                  OPT20
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C                   Z-ADD     *ZEROS        SSAGT1
     C                   Z-ADD     *ZEROS        SSAGT2
     C                   MOVEL     *BLANKS       SSSTAT
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C     *INKG         CASEQ     *ON           KGDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C     OTHOPT        BEGSR
     C*
     C                   SETOFF                                       30
      *
     C     SCSEL         IFEQ      '20'
     C     SCSTAT        IFEQ      'P'
     C                   SETON                                        050793
     C                   ELSE
     C                   EXSR      OPT20
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* KGDO   - SUBROUTINE FOR F07
     C*===============================================================
     C*
     C     KGDO          BEGSR
      *
     C                   MOVE      'P'           ##MODE
     C                   Z-ADD     *ZEROS        QQRRN
     C                   Z-ADD     *ZEROS        QQCO#
     C                   Z-ADD     *ZEROS        QQAGT#
     C                   MOVEL     *BLANKS       QQOUTQ
     C     *IN60         DOUEQ     *ON
     C                   READC     SCR01S                                 60
     C     *IN60         IFEQ      *OFF
     C     SCSTAT        ANDEQ     'M'
     C                   MOVEL(P)  SCOUTQ        SCNEWQ
     C                   CALL      ACDPGM        ACDPL                  99
     C                   ENDIF
     C                   ENDDO
      *
     C     QQERR         IFEQ      *BLANKS
     C                   EXSR      KEDO
     C                   ENDIF
      *
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
      *
     C                   CLOSE     WXAGP
     C                   CLOSE     WXAGL1
      *
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     10            SF1PAG            5 0
      *
     C                   CALL      'RTVCLTID'
     C                   PARM                    XXCLT#            3
     C*
     C     @@USER        CHAIN     PUP03                              60
     C     *IN60         IFEQ      *OFF
     C                   Z-ADD     UPCO#         ##CO#
     C                   Z-ADD     UPFND         ##FND
     C                   ENDIF
     C*
     C*  GET GROUP INFORMATION
     C*
     C     FNKY          CHAIN     PFN01                              60
     C*                    Z-ADDFNFND     SCFND
     C*                    MOVELFNNAME    SCNAME
      *
     C*                    CALL 'W7172C'
     C*                    PARM @@USER    QQUSER 10
     C*
     C                   OPEN      WXAGP
     C                   OPEN      WXAGL1
     C*
     C                   ENDSR                                                  PGMSTR
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1          READE     DBRCDF                                 95
     C   22KEY1A         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1          READPE    DBRCDF                                 95
     C   22KEY1A         READPE    DBRCDF                                 95
      *
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1A         SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1A         SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C*                    Z-ADDSCDBRR    @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
      *
      * Don't show records for members or state, eventhough they are
      * in the file
      *
     C** **      AGRUN     IFNE 'A'
     C** **                MOVE *OFF      MEETSS
     C** **                ENDIF
      *
      * Only show "header" records
      *
     C********** AGHDR     IFEQ 'Y'
     C     AGHDR         IFNE      'Y'
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * Agent copies are not generated for Agent 1 in W7165R/7R
      *
     C     AGAGT#        IFEQ      1
     C                   MOVE      *OFF          MEETSS
     C                   ENDIF
      *
      * User select to view only certain status
      *
     C***  Begin Chg  ***  08/24/06  **************************************
     C********** SSSTAT    IFNE *BLANKS
     C********** AGSTAT    ANDNESSSTAT
     C**********           MOVE *OFF      MEETSS
     C**********           ENDIF
     C                   SELECT
     C     SSSTAT        WHENEQ    '*'
     C     AGSTAT        ANDEQ     'P'
     C                   MOVE      *OFF          MEETSS
     C***  Begin Add  ***  08/24/06  **************************************
     C     SSSTAT        WHENEQ    '*'
     C     AGSTAT        ANDNE     'P'
     C* * * *   N O  C H A N G E S
     C***  End   Add  ***  08/24/06  **************************************
     C     SSSTAT        WHENNE    *BLANKS
     C     AGSTAT        ANDNE     SSSTAT
     C                   MOVE      *OFF          MEETSS
     C                   ENDSL
     C***  End   Chg  ***  08/24/06  **************************************
      *
      * User wants only certain agents
      *
     C     SSAGT1        IFNE      *ZEROS
     C     SSAGT2        ANDNE     *ZEROS
      *
     C                   MOVE      *OFF          MEETSS
     C     AGAGT#        IFGE      SSAGT1
     C     AGAGT#        ANDLT     SSAGT2
     C                   MOVE      *ON           MEETSS
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDSR                                                  SUBSET
      *****************************************************************
** Headers - HD
Opt  Agent#  Agent Name                        Spool File  Status   OutQue
Opt   Bill Dep     Effective       Expires
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                NNN
12020=Change Status         YYY
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   NNN
207F7=Process Requests      YYY
211F11=Change View          NNN
212F12=Cancel               YYY
217F17=Subset               YYY
