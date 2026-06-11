     HCopyright('(c) 2020 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H***** Beg Add *** 06/09/20 *******************************************
     H Bnddir('STBNDDIR')
     H***** End Add *** 06/09/20 *******************************************
     H*****************************************************************
     H*  F2550R - WORK WITH BILLS WITH ERROR STATUS                   *
     H*                                                               *
     H*  CALLED FROM: F8047R                                          *
     H*  CALLS TO:    F2550R01                                        *
     H*                                                               *
     H*****************************************************************
     H*  Created on 02/21/2012 by c:280 for project 133MCC            *
     H*****************************************************************
     H*                                                               *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  06/09/20  C:212 Project : SY420                              *
     H*                  Mask SSN                                     *
     H*                                                               *
     H*  11/14/14  C:79  Project:  FSMNT                              *
     H*                  Change name                                  *
     H*                                                               *
     H*  07/11/13  C:65  Project:  Redmine #3349                      *
     H*                  Fix the return position of INKC and INKL.    *
     H*                                                               *
     H*                                                               *
     H*****************************************************************
     H*  INDICATORS USED:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     FF2550F    CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     FFAU6P     IF   E             DISK
     F                                     RENAME(PU601:PFRCDF)
     F*
     F*  BILL UPLOAD ERROR STATUS BY TRID/STDT/STTM
     F*
     FFAU6L     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PU601:DBRCDF)
     F*
     F*  BILL UPLOAD ERROR STATUS BY TRID/STDT/STTM
     F*
     FFAU1P     IF   E             DISK
     F*
     F*  PAYMENT FILE - HEADER  BY TRID
     F*
      ***** Beg Add *** 06/09/20 *******************************************
      /copy sPrMask
      /copy sPrSecure
      ***** End Add *** 06/09/20 *******************************************
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D*
     D                 DS
     D  XXDATA                 1     74
     D  XXMDY                  1     10
     D  XXHMS                 13     20
     D  XXSTAT                26     26
     D  XXREAS                31     74
     D*
     D                 DS
     D  XXSTDT                 1      8  0
     D  XMTH                   1      2
     D  XDAY                   3      4
     D  XYR                    5      8
     D*
     D                 DS
     D  XXSTTM                 1      6  0
     D  XHR                    1      2
     D  XMM                    3      4
     D  XSS                    5      6
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D OF              S              1    DIM(8) CTDATA PERRCD(1)              Options, Function
     D OFD             S             35    DIM(8) ALT(OF)
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*** Beg Chg *** 11/14/14 **************************************
     D*******
     D*OPTNM*          C                   CONST('Bills with Error St-
     D*******                              atus')
     D*******
     D*OPTNMT          C                   CONST('Bills with Error St-
     D*******                              atus')
     D*******
     D*OPTNMD          C                   CONST('Bills with Error St-
     D*******                              atus')
     D*
     D OPTNM           C                   CONST('Error Status')
     D*
     D OPTNMT          C                   CONST('Error Status')
     D*
     D OPTNMD          C                   CONST('Error Status')
     D*** End Chg *** 11/14/14 **************************************
     D*
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('FSMESG')
     D ACDPGM          C                   CONST('F2550R01')
     D COMAPI          C                   CONST('FA007R')
     D USPAPI          C                   CONST('F0006R')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(8)
      *
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C*** begin chg ***  07/11/13  ***************************************************
     C******INKC         PARM                    @@KC              1
     C******INKL         PARM                    @@KL              1
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C*** begin chg ***  07/11/13  ***************************************************
     C                   PARM                    @@MODE            1
     C                   PARM                    @@RRN             9 0
     C                   PARM                    @@CALL            1
     C                   PARM                    @@RTCD            1
     C                   PARM                    @@CO#             3
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM                    ##CALL            1
     C                   PARM                    ##TRID           15
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C*  CONVERTS DATE FROM MDYY TO CYMD
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTMDY2        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      '*YYMD'       SRFFMT            8
     C                   PARM      '*MDYY'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY           KLIST
     C                   KFLD                    KKTRID
     C*                    KFLD           KKSTDT
     C*                    KFLD           KKSTTM
     C*                    KFLD           KKESTS
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    U6TRID        KKTRID
     C     *LIKE         DEFINE    U6STDT        KKSTDT
     C     *LIKE         DEFINE    U6STTM        KKSTTM
     C     *LIKE         DEFINE    U6ESTS        KKESTS
     C     *LIKE         DEFINE    @@CALL        SVCALL
     C*
     C*          *LIKE     DEFN KKCO#     CMCO#
     C*          *LIKE     DEFN KKSS#     CMSS#
     C*          *LIKE     DEFN KKACDT    CMACDT
     C*          *LIKE     DEFN KKCASE    CMCASE
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C*
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* ERRCKP - Error checking for the prompt screen
     C*================================================================
     C*
     C     ERRCKP        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
      /free

       KKTRID = *blanks;
       KKSTDT = *ZEROS;
       KKSTTM = *ZEROS;
       KKESTS = *BLANKS;

       CHAIN @@RRN PU101;

       KKTRID = U1TRID;
       SCTRID = U1TRID;
       ##TRID = U1TRID;

      /end-free
      *
     C                   ENDSR
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C* Convert date
     C                   MOVE      SCPST1        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         KKSTDT
      /free

       KKSTTM = *HIVAL;

      /end-free
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
      /free

       KKTRID = SCTRID;
       KKSTDT = SCSTDT;
       KKSTTM = SCSTTM;
       KKESTS = SCESTS;

      /end-free
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
      /free

       IF U6STDT <> *ZEROS;

      /end-free
     C                   Z-ADD     U6STDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTMDYY
     C                   MOVEL     SRDTO         SCACDT
      /free

       XXSTDT = SCACDT;

       XXMDY = XMTH + '/' + XDAY + '/' + XYR;

       XXSTTM = U6STTM;

       ELSE;

       XXSTDT = *ZEROS;
       XXSTTM = *ZEROS;

       ENDIF;

       XXSTTM = U6STTM;

       XXHMS = XHR + ':' + XMM + ':' + XSS;

       XXSTAT = U6ESTS;

       XXREAS = U6REAS;

       SCDATA = XXDATA;

      /end-free
     C                   ENDSR
     C*
     C*================================================================
     C* OPT2 - WORK WITH ERRORS - CHANGE
     C*================================================================
     C*
     C     OPT           BEGSR
     C*
     C                   CALL      acdpgm        acdpl
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C     OTHFKS        BEGSR
     C*
     C     *INKC         CASEQ     *ON           KCDO
     C                   CAS                     ENTRDO
     C                   ENDCS
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         IFEQ      '2'
     C     SCSEL         OREQ      '4'
     C     SCSEL         OREQ      '5'
     C*
     C                   EXSR      OPT
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   MOVE      *ON           *INLR
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
     C*                    CALL 'SRCENTER'
     C*                    PARM           ##DATA 80
     C*                    PARM 40        ##LENG  40
     C                   MOVE      OPTNMT        SCPRDS
     C*
     C* GET PAYMENT HEADER
     C*
     C     *IN99         IFEQ      *OFF
     C*
     C                   MOVEL(P)  KKTRID        SCTRID
     C*
     C                   CALL      'SRNAME'
     C                   PARM      U1CLSN        ##LAST           56
     C                   PARM      U1CFSN        ##FRST           15
     C                   PARM      '1'           ##TERM            1
     C*
     C                   MOVEL(P)  ##LAST        SCCNAM
      ***** Beg Chg *** 06/09/20 *******************************************
     C***                MOVEL(P)  U1SS#         SCSS#
      /Free
       scSS# = DisplaySS#(MaskData('SSN':'U1SS#':U1SS#));
      /End-free
      ***** End Chg *** 06/09/20 *******************************************
     C                   Z-ADD     U1DOI         SRDTI
     C                   CALL      'SRCVTDAT'    DTMDY2
     C                   MOVEL     SRDTO         SCACDT
     C*
     C                   MOVEL     U1CLM#        SCCLM#
     C*
     C                   ENDIF
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C     KEY           READE     DBRCDF                                 95
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C     KEY           READPE    DBRCDF                                 95
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C     KEY           SETGT     DBRCDF                             96
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C     KEY           SETLL     DBRCDF                             96
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C*
     C*
     C                   ENDSR
     C*
     C*================================================================
** Column headings
Opt    Date       Time    Status  Reason
** Description--------------MISN
1022=Change                 YYY
1044=Delete                 YYY
1055=Display                YYY
203F3=Exit                  YYN
206F6=Add                   YNN
212F12=Cancel               YNY
3
