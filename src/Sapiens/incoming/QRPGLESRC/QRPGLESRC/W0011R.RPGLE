     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0011R - WORK WITH GROUP YEARS
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL GROUP  *
     H*                YEARS FOR A SELECTED GROUP.  EACH YR CAN BE    *
     H*                ACCESSED FOR UPDATE, DISPLAY OR SELECTION      *
     H*                DEPENDING ON THE CALLING PROGRAM.              *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*  CALLS TO:                                                    *
     H*    W0011R02 - GROUP DETAIL INFORMATION (OPT 2/5)              *
     H*                                                               *
     H*  CREATED: 12/16/96  PROGRAMMER: C:33  PROJECT: WC577          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  03/12/98 C:85 PROJECT: WC577                                 *
     H*                CHANGED THE SLMSGI CONSTANT FROM PRJ9998       *
     H*                TO WC39998,AND THE SLMSGF CONSTANT FROM PJMESG *
     H*                TO WMESGG.                                     *
     H*                                                               *
     H*    NOTE:  PLEASE ADD NEW COMMENTS AT THE TOP OF REVISIONS.    *
     H*    -------------------------------------------------------    *
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
     H*  30 - Called as a search                                      *
     H* N30 - Called as Maintenance                                   *
     H*  40 - More Option Controller                                  *
     H*  41 - More Option Controller                                  *
     H*  71 - These indicators are to used as error indicators for    *
     H*       thru the various screens. They will set off after       *
     H*       displaying each input capible screen.                   *
     H*  90 - Used to general purpose error indicator                 *
     H*  91 - Used to display CPF5203; roll up/down past last record  *
     H*  95 - Used to condition SFLEND; also used to indicate EOF and *
     H*       BOF on the file being read.                             *
     H*****************************************************************
     FW0011F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWDF1P     IF   E             DISK
     F                                     RENAME(PF101:PFRCDF)
     FWDF1L2    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PF101:DBRCDF)
     FWMFNP     IF   E             DISK
     FWMUPL     IF   E           K DISK
      /COPY SCOPYWW01D
     D*
     D HD              S             74    DIM(1) CTDATA PERRCD(1)              HEADERS
     D OF              S              1    DIM(26) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(26) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Policy Years')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0011R02')
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

     I*
     I********FIDS01    E DSSZQ2P

     C*
     C     *LIKE         DEFINE    F1FYR         KKFYR
     C     *LIKE         DEFINE    FNNAME        U1PDSC
     C     *LIKE         DEFINE    FNNAME        U2PDSC
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@RRN         ##RRN2            9 0
     C*
     C     ACDPL1        PLIST
     C     *INKC         PARM                    ##KC
     C     *INKL         PARM                    ##KL
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CO#
     C                   PARM                    ##FYR             3 0
     C*
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
     C                   KFLD                    FNFND
     C                   KFLD                    KKFYR
     C*
     C     KEY1B         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNFND
     C                   KFLD                    KKFYR
     C*
     C     KEY1C         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    FNFND
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@YR              3 0
     C*
     C/COPY SCOPYWW01C
     C*
     C*===============================================================
     C* CHKSSM - SUBROUTINE FOR SUBSET MESSAGE
     C*===============================================================
     C*
     C     CHKSSM        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C                   MOVE      *ON           *IN11
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
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
     C*
     C*===============================================================
     C* ERRCK2 - SUBROUTINE TO CHECK SUBFILE SELECTIONS
     C*===============================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*===============================================================
     C* INZSST - SUBROUTINE TO INITIALIZE SUBSET SELECTIONS
     C*===============================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*===============================================================
     C* KEYINZ - SUBROUTINE TO INITIALIZE THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYINZ        BEGSR
     C                   MOVE      *HIVAL        KKFYR
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C  N20              MOVE      SCPST1        KKFYR
     C   20              MOVE      SCPST2        KKFYR
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C                   MOVE      SCFYR         KKFYR
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   Z-ADD     F1FYR         SCFYR
     C*
     C                   Z-ADD     F1STPO        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCSTPO
     C*
     C                   Z-ADD     F1ENPO        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCENPO
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*===============================================================
     C* OPT10 - SUBROUTINE FOR OPTION 10
     C*===============================================================
     C*
     C* OPTION 10=
     C*
     C     OPT10         BEGSR
     C*
     C*
     C                   ENDSR                                                  OPT10
     C*
     C*===============================================================
     C* OPT11 - SUBROUTINE FOR OPTION 11
     C*===============================================================
     C*
     C* OPTION 11=
     C*
     C     OPT11         BEGSR
     C*
     C*
     C                   ENDSR                                                  OPT11
     C*
     C*===============================================================
     C* OTHFKS - SUBROUTINE FOR OTHER FUNCTION KEYS
     C*===============================================================
     C*
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C     *INKX         CASEQ     *ON           KXDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHFKS
     C*
     C*===============================================================
     C* OTHOPT - SUBROUTINE FOR OTHER OPTIONS
     C*===============================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C                   MOVE      ##CO#         ##CO#
     C*
     C     SCSEL         CASEQ     '10'          OPT10
     C     SCSEL         CASEQ     '11'          OPT11
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C*
     C     RECSEL        IFEQ      *ON
     C   30              Z-ADD     *ZEROS        @@RRN
     C                   ENDIF                                                  RECSEL=*ON
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   Z-ADD     12            SF1PAG            5 0
     C                   MOVE      *OFF          RECSEL
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C*
     C     @@RRN         CHAIN     WMFNP                              60
     C  N60              Z-ADD     FNFND         SCFND
     C  N60              Z-ADD     FNFND         ##FND
     C  N60              MOVEL     FNNAME        SCNAME
     C*
     C     Q1USER        CHAIN     PUP01                              60
     C*
     C  N60UPFYR         IFNE      *ZERO
     C                   MOVE      UPFYR         KKFYR
     C                   ENDIF                                                  UPFYR<>*ZERO
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21KEY1C         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21KEY1C         READPE    DBRCDF                                 95
     C*
     C                   ENDSR                                                  READP
     C*
     C*===============================================================
     C* SETGT - SUBROUTINE TO SET THE FILE > THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETGT         BEGSR
     C*
     C   21KEY1          SETGT     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*===============================================================
     C* SETLL - SUBROUTINE TO SET THE FILE < THE SELECTED FIELD
     C*===============================================================
     C*
     C     SETLL         BEGSR
     C*
     C   21KEY1          SETLL     DBRCDF                             96
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*===============================================================
     C* SF1UPD - SUBROUTINE TO UPDATE THE SUBFILE AFTER A DELETE
     C*===============================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCFYR         @@YR
     C                   MOVE      SCDBRR        @@RRN
     C                   MOVE      *ON           RECSEL            1
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF YEARS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C*
B01  C     SSPDSC        IFNE      *BLANKS
     C*
     C     LO:UP         XLATE     FNNAME        U1PDSC
     C     LO:UP         XLATE     SSPDSC        U2PDSC
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
B02  C     SCNRES        IFEQ      0
     C                   MOVE      *OFF          MEETSS
E02  C                   ENDIF                                                  SCNRES=0
     C*
E01  C                   ENDIF                                                  SSPDSC<>*BLANKS
     C*
     C                   ENDSR                                                  SUBSET
** Headers - HD
  YR  Start Date   End Date
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1055=Display                YYY
1077=                       NNN
1088=                       NNN
1099=                       NNN
11010=                      NNN
11111=                      NNN
11212=                      NNN
11313=                      NNN
11414=                      NNN
11515=                      NNN
11616=                      NNN
201F1=                      NNN
202F2=                      NNN
203F3=Exit                  YYN
204F4=                      NNN
205F5=Refresh               YYY
206F6=Add                   YNN
207F7=                      NNN
208F8=                      NNN
209F9=Change Info           NNN
210F10=                     NNN
211F11=Change View          NNN
212F12=Cancel               YYY
