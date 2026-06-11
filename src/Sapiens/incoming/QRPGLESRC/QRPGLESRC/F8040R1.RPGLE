     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H/define CONVERT
     H*****************************************************************
     H*  F8040R1 - Automated Bill Upload                              *
     H*****************************************************************
     H*                                                               *
     H*  Called from: F8040C1 - Upload Bills - Controller             *
     H*  Calls to   : F8041C  - Upload Bills - Override               *
     H*                                                               *
     H*****************************************************************
     H*  Created on 05/06/2013 by c:282 for project 114WJC            *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*  05/03/19 C:062 Project 152MAP                                *
     H*                 Change call to F8041R so as to create eobs.   *
     H*                                                               *
     H*  11/01/13 C:274 Support 83764                                 *
     H*        Only call UploadBills only id Process Date (U9PRDT)    *
     H*        is equal to ZERO.                                      *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     fFxu0l     if   e           k disk
     f                                     Rename(QWHFDML:dbrcdf)
     fFau9l1    if a e           k disk    Rename(pu901:pu900)
     f                                     Infds(dbfds1)
     fFau9l2    if   e           k disk
     f
      *
     d charNAME        s             10
     d charMBR#        s             10
     d charCmpy        s              1
     d charRRN         s              9
     d intRRN          s              9  0
      *
     d                 ds
     d  DSdttm                 1     14  0
     d  DStime                 1      6  0
     d  DSdate                 7     14  0
      *
     d                 ds
     d  KKMBR#                 1     10
     d  YYJUL                  2      8  0
     d  B                      1      1
     d  YYYEAR                 2      5  0
     d  YYDAYS                 6      8  0
     d  Seq#                   9     10  0
      *
     d                 ds
     d  U9mbr#                 1     10
     d  YYJUL2                 2      8  0
     d  xxSeq#                 9     10  0
      *
     d psds          esds                  Extname(szq1p)
      *
     d dbfds1          ds
     d  rrnFAU9L1            397    400b 0

      * Upload Bills
     d UploadBills     pr                  extpgm('F8041C')
     d   charRRN                      9
     d   charNAME                    10
     d   charMBR#                    10
     d   charCMPY                     1
      *================================================================
      *  PLIST definitions
      *================================================================
     c     *ENTRY        Plist
     c                   Parm                    @@Call            1
     c                   Parm                    @@RtCd            1
     c                   Parm                    @@CMPY            1
      *
     c     DTCYMD        Plist
     c                   Parm                    SRDTI             8 0
     c                   Parm      *BLANKS       SRFFMT            8
     c                   Parm      '*CYMD'       SRTFMT            8
     c                   Parm                    SRDTO            10
     c                   Parm                    SRERR             1
      *
     c     DTYYMD        Plist
     c                   Parm                    SRDTI             8 0
     c                   Parm      *BLANKS       SRFFMT            8
     c                   Parm      '*YYMD'       SRTFMT            8
     c                   Parm                    SRDTO            10
     c                   Parm                    SRERR             1
      *
     c     DTJUL         Plist
     c                   Parm                    SRDTI             8 0
     c                   Parm      '*MDYY'       SRFFMT            8
     c                   Parm      '*JUL'        SRTFMT            8
     c                   Parm                    SRDTO            10
     c                   Parm                    SRERR             1
      *================================================================
      *  KLIST definitions
      *================================================================
     c     keyFAU9L1     Klist
     c                   Kfld                    MLname
      *
     c     keyFAU9L2     Klist
     c                   Kfld                    kkMbr#
      *================================================================
      *  MAIN PROCESSING
      *================================================================
      /Free
       Exsr Main;
       *InLr = *On;
      /End-Free
      *================================================================
      * MAIN - Main processing
      *================================================================
      /Free
       BegSr Main;

       Read Dbrcdf;
       Dow Not %EOF(Fxu0l);
         if %subst(mlname:1:1) = 'O';
          Exsr GetWrt;
          intRRN  =  rrnFAU9L1;
          charRRN =  %Editc(intRRN:'X');
          charNAME = U9name;
          charMBR# = U9mbr#;
          //*** Begin Chg *** 05/03/19 ******************************
          //*** charCMPY = @@CMPY;
          charCMPY = ' ';
          //*** End   Chg *** 05/03/19 ******************************
         //** Beg Add *** 11/01/13 **********************************
          if   U9PRDT=0;
         //** End Add *** 11/01/13 **********************************
          UploadBills(charRRN:charNAME:charMBR#:charCMPY);
         //** Beg Add *** 11/01/13 **********************************
          endif;
         //** End Add *** 11/01/13 **********************************
         endif;
          Read Dbrcdf;
       EndDo;

       EndSr;
      /End-Free
      *================================================================
      * GETWRT - Get or Write the transaction record ds
      *================================================================
     c     GETWRT        BEGSR
      *
     c                   TIME                    DSDTTM
      *
     c     keyFAU9L1     CHAIN     PU900                              99
     c     *IN99         IFEQ      *ON
      *
     c                   CLEAR                   PU900
      *
     c                   EXSR      BLDMBR
      *
     c                   MOVEL(P)  MLname        U9name
     c                   MOVEL(P)  KKMBR#        U9mbr#
      *
     c                   Z-ADD     DSDATE        SRDTI
     c                   CALL      'SRCVTDAT'    DTCYMD
     c                   MOVEL     SRDTO         U9uldt
      *
     c                   Z-ADD     DSTIME        U9ultm
     c                   Z-ADD     *ZEROS        U9cvdt
     c                   Z-ADD     *ZEROS        U9cvtm
     c                   Z-ADD     *ZEROS        U9prdt
     c                   Z-ADD     *ZEROS        U9prtm
     c                   MOVE      'A'           U9stat
     c                   Z-ADD     *ZEROS        U9tcnt
     c                   Z-ADD     *ZEROS        U9dcnt
     c                   Z-ADD     *ZEROS        U9ccnt
     c                   Z-ADD     *ZEROS        U9rcnt
     c                   Z-ADD     *ZEROS        U9ucnt
      *
     c                   MOVEL     Q1PGM         U9pgm
     c                   MOVEL     Q1USER        U9user
     c                   Z-ADD     U9ULDT        U9dt
     c                   Z-ADD     DSTIME        U9time
      *
     c                   MOVEL     Q1PGM         U9ipgm
     c                   MOVEL     Q1USER        U9iusr
     c                   Z-ADD     U9ULDT        U9idt
     c                   Z-ADD     DSTIME        U9itim
      *
     c                   WRITE     PU900
     c                   ENDIF
      *
     c                   ENDSR
      *================================================================
      * BLDMBR - Build Member Number
      *================================================================
     c     BLDMBR        BEGSR
      *
     c                   Z-ADD     *ZEROS        YYJUL2
     c                   Z-ADD     *ZEROS        xxSeq#
     c                   MOVEL(P)  'B'           B
      *
     c                   TIME                    DSDTTM
      *
     c                   Z-ADD     DSDATE        SRDTI
     c                   CALL      'SRCVTDAT'    DTJUL
     c                   MOVEL     SRDTO         YYJULI            5 0
      *
     c                   Z-ADD     DSDATE        YYYEAR
     c                   Z-ADD     YYJULI        YYDAYS
     c                   Z-ADD     99            Seq#
      *
     c     keyFAU9L2     SETGT     PU901
     c                   READP     PU901                                  99
      *
     c     *IN99         IFEQ      *OFF
     c     YYJUL         ANDEQ     YYJUL2
     c     xxSeq#        IFEQ      99
     c                   Z-ADD     0             Seq#
     c                   ELSE
     c     xxSeq#        ADD       1             Seq#
     c                   ENDIF
     c                   ELSE
     c                   Z-ADD     0             Seq#
     c                   ENDIF
      *
     c                   ENDSR
