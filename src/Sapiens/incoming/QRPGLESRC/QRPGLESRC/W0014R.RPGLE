     Hcopyright('(c) 2018 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H/define CONVERT
     H*****************************************************************
     H/TITLE W0014R - STATISTICAL REPORTING/RATING MAINTENANCE
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL STATI- *
     H*                STICAL REPORTING/RATING FILES FOR A GROUP.     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0010R - WORK WITH GROUPS                                  *
     H*  CALLS TO:                                                    *
     H*    W0014R02 - STAT RPT/RATING DEFINITION DATA SCREENS         *
     H*                                                               *
     H*  CREATED: 05/20/97  PROGRAMMER: C:65  PROJECT: WC651          *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*
     H* 03/08/19 C:136 Project: SY406
     H*                Add Option 76 - Carrier Rate Defaults
     H*
     H*
     H* 11/27/18 C:33  Project: SY406
     H*                Add Option 75 = Template Associations
     H*                                                               *
     H*  04/19/13 C:180 SUPPORT WA146 82141                           *
     H*                 Recompiled only.                              *
     H*  01/28/11 C:190 PROJECT: WC1954                               *
     H*                 ADD OPTION 38.                                *
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
     H*  31 - Called as a search w/YR                                 *
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
     FW0014F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     FWMFNL     IF   E             DISK
     FWDRRL1    IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PRR01:DBRCDF)
     FWDRRP     IF   E             DISK
     F                                     RENAME(PRR01:PFRCDF)
     F***  BEG  ADD  ***  01/28/11  **************************************
     FSSYLF     IF   E           K DISK
     FWMLDL3    IF   E           K DISK    INFDS(DBFDS2)
      /COPY SCOPYWW01D
     D***** END ADD ***** 01/28/11 ***************************************
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              COLUMN HEADINGS
     D OF              S              1    DIM(26) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(26) ALT(OF)
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D OPTNM           C                   CONST('Statistics and Rati-
     D                                     ng')
     D DELETD          C                   CONST('*Deleted')
     D ACDPGM          C                   CONST('W0014R02')
     D LO              C                   CONST('abcdefghijklmnopqrs-
     D                                     tuvwxyz')
     D UP              C                   CONST('ABCDEFGHIJKLMNOPQRS-
     D                                     TUVWXYZ')
     D SLMSGI          C                   CONST('WC39998')
     D SLMSGF          C                   CONST('WMESGG')
     D COMAPI          C                   CONST('WA001R')
     D USPAPI          C                   CONST('W0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D***  Begin Add  ***  11/27/18  *************************************
      /copy WITMPLTD
      /copy sPrModule
     D***  End   Add  ***  11/27/18  *************************************
      *** beg add ***** 03/08/19 **************************
     dRateDefaults     pr                  extpgm('WITBLRDFT')
     d                                9  0
     d                                1
      *** end add ***** 03/08/19 **************************
     I*

     C*
     C     *LIKE         DEFINE    FNFND         KKFND
     C     *LIKE         DEFINE    FNNAME        KKNAME
     C     *LIKE         DEFINE    FNNAME        U1PDSC
     C     *LIKE         DEFINE    FNNAME        U2PDSC
     C     *LIKE         DEFINE    RRCO#         XXCO#
     C     *LIKE         DEFINE    RRST          XXST
     C     *LIKE         DEFINE    RREFDT        XXEFDT
     C     *LIKE         DEFINE    SCEFDT        XXEFD2
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    XXCO#
     C                   PARM                    SCFND
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
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C                   KFLD                    XXEFDT
     C*
     C     KEY1C         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C*
     C     KEY1S         KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXST
     C*** BEG ADD ***** 01/28/11 *************************************
     C*   KEY LIST FOR THE SYSTEM CONTROL FILE
     C*
     C     SYKY          KLIST
     C                   KFLD                    XXSYK             2
     C*
     C     LDKY          KLIST
     C                   KFLD                    XXCO#
     C                   KFLD                    SCFND
     C                   KFLD                    XXEMP#
     C                   KFLD                    XXDIV#
     C                   KFLD                    XXFYR
     C                   KFLD                    SCST
     C                   KFLD                    XXEFDT
     C*** END ADD ***** 01/28/11 *************************************
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@ST              2
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
     C* CVTDT2 - SUBROUTINE TO CONVERT DATE FROM MDY TO CYMD
     C*===============================================================
     C*
     C     CVTDT2        BEGSR
     C*
     C                   CALL      'SRCVTDAT'
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C                   ENDSR                                                  CVTDT2
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
     C  N32              MOVE      *LOVAL        XXST
     C                   MOVE      *HIVAL        XXEFDT
     C                   ENDSR                                                  KEYINZ
     C*
     C*===============================================================
     C* KEYSET - SUBROUTINE TO SET THE REPOSITION KEYS
     C*===============================================================
     C*
     C     KEYSET        BEGSR
     C  N32              MOVE      SCPST1        XXST
     C     SCPST2        IFEQ      *BLANKS
     C                   MOVE      *HIVAL        XXEFDT
     C                   ELSE
     C                   MOVE      SCPST2        XXEFD2
     C                   Z-ADD     XXEFD2        SRDTI
     C                   EXSR      CVTDT2
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDIF                                                  SCPST2 EQ 0
     C                   ENDSR                                                  KEYSET
     C*
     C*===============================================================
     C* KEYSFL - SUBROUTINE TO SET THE REPOSITION KEYS (USER DEFINED)
     C*===============================================================
     C*
     C     KEYSFL        BEGSR
     C  N32              MOVE      SCST          XXST
     C                   MOVE      SCEFDT        XXEFD2
     C                   Z-ADD     XXEFD2        SRDTI
     C                   EXSR      CVTDT2
     C                   MOVEL     SRDTO         XXEFDT
     C                   ENDSR                                                  KEYSFL
     C*
     C*===============================================================
     C* MVDBSC - SUBROUTINE TO LOAD SUBFILE FIELDS
     C*===============================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C                   MOVE      RRST          SCST
     C                   Z-ADD     RREFDT        SRDTI
     C                   EXSR      CVTDT
     C                   MOVEL     SRDTO         SCEFDT
     C                   MOVE      *BLANKS       SCDEL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*** BEG ADD ***** 01/28/11 *************************************
      *===============================================================
      * OPT38 - Large Deductible Entry
      *===============================================================
     C     OPT38         BEGSR
     C*
     C                   MOVE      *ZEROS        XXEMP#            9 0
     C                   MOVE      *ZEROS        XXDIV#            5 0
     C                   MOVE      *ZEROS        XXFYR             3 0
     C                   MOVE      SCEFDT        XXEFD2
     C                   Z-ADD     XXEFD2        SRDTI
     C                   EXSR      CVTDT2
     C                   MOVEL     SRDTO         XXEFDT
     C*
     C     LDKY          CHAIN     PLD01                              60
     C     *IN60         IFEQ      *OFF
     C                   MOVE      'C'           ##MODE
     C                   MOVE      DBRRN2        ##RRN
     C                   ELSE
     C                   MOVE      'A'           ##MODE
     C                   MOVE      *ZERO         ##RRN
     C                   ENDIF                                                  *IN60 = *OFF
     C*
     C   09              MOVE      'D'           ##MODE
      *
     C                   CALL      'W0051R38'                           99
     C     *INKC         PARM      *BLANK        ##KC              1
     C                   PARM      *BLANK        ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM                    ##RRN             9 0          WMLDP
     C                   PARM      SCDBRR        ##RRN1            9 0          WDRRP
     C                   PARM      'G'           #1MODE            1
      *
     C                   ENDSR                                                  OPT38
     C*** END ADD ***** 01/28/11 *************************************
     c***  Begin Add  ***  11/27/18  *************************************
    C*================================================================
    C* Opt75 - Process other function keys
    C*================================================================
      /Free
       Begsr Opt75;
        Select;
        When @@call = ' ';
           ##Mode = 'C';
        Other;
           ##Mode = 'D';
        Endsl;
        Reset dsWITMPLTparms;
        dsWITMPLTparms.co# = xxCo#;
        dsWITMPLTparms.fnd = scFnd;
        dsWITMPLTparms.state = scSt;
        TemplateAssociations(dsWITMPLTparms:'Group/State':##Mode);
       Endsr;
       //*** beg add ***** 03/08/19 ***********************
       //===================================================
       Begsr Opt76;    //Carrier Rate Defaults for GoConnect
       //===================================================
        Select;
        When @@call = ' ';
           ##Mode = 'C';
        Other;
           ##Mode = 'D';
        Endsl;
        RateDefaults(scDbRR:##Mode);
       Endsr;
       //*** end add ***** 03/08/19 ***********************
      /End-free
     c***  End   Add  ***  11/27/18  *************************************

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
     C*** BEG ADD ***** 01/28/11 *************************************
     C     SCSEL         CASEQ     '38'          OPT38                    90
     c***  Begin Add  ***  11/27/18  *************************************
     c     scsel         caseq     '75'          opt75
     c***  End   Add  ***  11/27/18  *************************************
     c***  beg add ***** 03/08/19 **************************
     c     scsel         caseq     '76'          opt76
     c***  End   Add  ***  11/27/18  ***********************
     C                   ENDCS
     C*** END ADD ***** 01/28/11 *************************************
     C*
     C                   ENDSR                                                  OTHOPT
      *
     C*
     C*===============================================================
     C* PGMEND - SUBROUTINE TO END THE PROGRAM
     C*===============================================================
     C*
     C     PGMEND        BEGSR
     C                   ENDSR
     C*
     C*===============================================================
     C* PGMSTR - SUBROUTINE TO START THE PROGRAM
     C*===============================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C                   MOVE      *ON           *IN20
     C                   Z-ADD     10            SF1PAG            5 0
     C*
     C     @@CALL        COMP      '1'                                    09    INQUIRY
     C     @@CALL        COMP      '2'                                    30    SEARCH
     C     @@CALL        COMP      '3'                                    31    SEARCH W/YR
     C*
     C   31              MOVEL     '2'           @@CALL
     C*
     C     @@ST          IFNE      *BLANKS
     C                   MOVE      *ON           *IN32
     C                   ELSE
     C                   MOVE      *OFF          *IN32
     C                   ENDIF                                                  @@ST IFNE
     C*
     C     @@RRN         CHAIN     PFN01                              60
     C                   MOVE      FNCO#         XXCO#
     C                   MOVE      FNCO#         ##CO#
     C                   MOVE      FNFND         SCFND
     C                   MOVE      FNNAME        SCFNAM
     C                   MOVE      *BLANKS       SCPST1
     C                   MOVE      *BLANKS       SCPST2
     C                   MOVE      @@ST          SCPSTS
     C                   MOVE      @@ST          XXST
     C                   Z-ADD     0             @@RRN
     C*** BEG ADD ***** 01/28/11 *************************************
     C                   MOVEL     'WC'          XXSYK
     C     SYKY          CHAIN     SWC01                              60
     C     *IN60         IFEQ      *OFF
     C     WCFL43        ANDEQ     'Y'
     C                   MOVE      '1'           XXOPFN
     C                   MOVE      '38'          XXNMBR
     C                   MOVE      'YYN'         XXMIS
     C                   EXSR      UPDOFA
     C                   ENDIF
     C*** END ADD ***** 01/28/11 *************************************
     c***  Begin Add  ***  11/27/18  *************************************
     c* Opt 75 is only allowed if module installed
      /Free
       If isModEnabled('Portal Module');
          xxOpFn = '1';
          xxNmbr = '75';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
        //*** beg add ***** 03/08/19 **********************
          xxOpFn = '1';
          xxNmbr = '76';
          xxMIS  = 'YYN';
          Exsr UpdOfA;
        //*** end add ***** 03/08/19 **********************
       Endif;
      /End-free
     c***  End   Add  ***  11/27/18  *************************************
     C*
     C                   ENDSR                                                  PGMSTR
     C*
     C*===============================================================
     C* READ - SUBROUTINE TO READ THE FILE
     C*===============================================================
     C*
     C     READ          BEGSR
     C*
     C   21
     CANN32KEY1C         READE     DBRCDF                                 95
     C   21
     CAN 32KEY1S         READE     DBRCDF                                 95
     C*
     C                   ENDSR                                                  READ
     C*
     C*===============================================================
     C* READP - SUBROUTINE TO READ THE FILE BACKWARDS
     C*===============================================================
     C*
     C     READP         BEGSR
     C*
     C   21
     CANN32KEY1C         READPE    DBRCDF                                 95
     C   21
     CAN 32KEY1S         READPE    DBRCDF                                 95
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
     C                   MOVEL     DELETD        SCDEL
     C                   ENDSR                                                  SF1UPD
     C*
     C*===============================================================
     C* SLCTRD - SUBROUTINE TO MOVE FILEDS FROM SELECTED RECORDS
     C*===============================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C                   MOVE      SCDBRR        @@RRN
     C*
     C                   ENDSR
     C*
     C*===============================================================
     C* SUBSET - SUBROUTINE TO SUBSET THE LIST OF GROUPS
     C*===============================================================
     C*
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*
** Column Headings
Opt
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1033=Copy                   YNN
1044=Delete                 YNN
1055=Display                YYY
13838=Large Deductible      NNN                            01/28/11
17575=Template Associations NNN                            11/27/18
17676=Carrier Rate Defaults NNN                            03/08/19
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
