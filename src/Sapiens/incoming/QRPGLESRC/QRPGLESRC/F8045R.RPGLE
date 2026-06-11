     hcopyright('(c) 2020 Sapiens, Inc. All rights reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*
     H*****************************************************************
     H*  F8045R - Work with Upload Bill Transfer Files                *
     H*                                                               *
     H*  Called from: Menu                                            *
     H*  Calls to   : F8045R01 - Upload Bill Transfer History upd/dsp *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/20/04 by C:62 for Project 51MFN                *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  05/11/20  C:212  Project: MC437                              *
     H*                   Redesign per CS standards                   *
     H*                   - Call to new CS standard program and return*
     H*                                                               *
     H*  01/19/12  C:251  Project: MC362                              *
     H*                   Change logical file to read in descending   *
     H*                   order                                       *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     FF8045F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FFAU9P     IF   E             DISK
     F                                     RENAME(PU901:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     F*** Beg Chg *** 01/19/12 ****************************************
     F**FAU9L1* IF  E           K        DISK
     FFAU9L3    IF   E           K DISK
     F*** End Chg *** 01/19/12 ****************************************
     F                                     INFDS(DBFDS)
     F                                     RENAME(PU901:DBRCDF)
     D***** Beg Add *** 05/11/20 **************************************
     D PBXfrHist       pr                  EXTPGM('MTPBULTRHS')
     D ppCall                         1
     D ppMode                         1
     D ppTabInd                       5
     D***** End Add *** 05/11/20 **************************************
      /COPY SCOPYWW01D
     D*
     D HD              S             79    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(10) CTDATA PERRCD(1)             Options, Function
     D OFD             S             35    DIM(10) ALT(OF)
     D***** Beg Add *** 05/11/20 **************************************
     d ppCall          s              1    inz
     d ppMode          s              1    inz
     d ppTabInd        s              5    inz
     D***** End Add *** 05/11/20 **************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D OPTNM           C                   CONST('Bill Transfer Histor-
     D                                     y')
     D SLMSGI          C                   CONST('MC49998')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F8045R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(11)

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C     *ENTRY        PLIST
     C                   PARM                    @@CALL            1
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
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY1          KLIST
     C                   KFLD                    KKNAME
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C     *LIKE         DEFINE    U9NAME        KKNAME
     C***** Beg Add *** 05/11/20 **************************************
     C/free
        If q1prms >= 1;
           ppCall = @@call;
        Else;
           ppCall = ' ';
        EndIf;
        ppMode = ' ';
        ppTabInd = '0';
        Callp PBXfrHist(ppCall:ppMode:ppTabInd);
        *inlr = *on;
        Return;
      /end-free
     C***** End Add *** 05/11/20 **************************************
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
     C*** Beg Chg *** 01/19/12 *****************************************
     C*******              MOVEL*BLANKS   KKNAME    P
     C                   MOVEL     *HIVAL        KKNAME
     C*** End Chg *** 01/19/12 *****************************************
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C     KEYSET        BEGSR
     C*
     C                   MOVEL(P)  SCPST1        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C     KEYSFL        BEGSR
     C*
     C                   MOVEL(P)  SCNAME        KKNAME
     C*
     C                   ENDSR
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C     MVDBSC        BEGSR
     C*
     C                   MOVEL(P)  U9NAME        SCNAME
     C                   MOVEL(P)  U9MBR#        SCMBR#
     C*
     C                   Z-ADD     U9ULDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCULDT
     C*
     C                   Z-ADD     U9ULTM        SCULTM
     C*
     C                   Z-ADD     U9PRDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCPRDT
     C*
     C                   Z-ADD     U9PRTM        SCPRTM
     C                   Z-ADD     U9TCNT        SCTCNT
     C*
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
     C                   ENDSR
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C     READ          BEGSR
     C   21              READ      DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C     READP         BEGSR
     C   21              READP     DBRCDF                                 95
     C                   ENDSR
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C     SETGT         BEGSR
     C   21KEY1          SETGT     DBRCDF                             96
     C                   ENDSR
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C     SETLL         BEGSR
     C   21KEY1          SETLL     DBRCDF                             96
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
Opt Batch name  Member #     Date       Time      Date       Time         Count
** Description--------------MISN
1011=Select                 NNY
1022=Change                 NNN
1044=Delete                 NNN
1055=Display                YYY
203F3=Exit                  YYN
205F5=Refresh               NNN
206F6=Add                   NNN
212F12=Cancel               NNY
217F17=Subset               NNN
3
