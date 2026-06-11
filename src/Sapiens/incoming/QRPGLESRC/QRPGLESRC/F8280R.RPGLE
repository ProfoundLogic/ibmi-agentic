    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE F8280R
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************
    H*  F8280R - First Health EOR Financial Process - Selection
    H*
    H*  Called from: F8280C - First Health EOR Financial Process - List Docs in
    H*  Calls to   : F8281C - First Health EOR Financial Process - Copy Doc to
    H*
     H**************************************************************************
    H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
    H*  Based on F8260R from Client 135
     H**************************************************************************
    H*  Revisions:
    H*
     H**************************************************************************
     Ffxx0l1    if   e           k disk
     F                                     rename(docdtl:dbrcdf)
     Ffax9l1    if a e           k disk
     F                                     infds(dbfds)
     Ffax9l2    if   e           k disk
     F                                     rename(px901:px902)
    F*
     D dbfds           ds
     D dbrrn                 397    400b 0
    D*
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    D*
     D                 ds
     D kkmbr#                        10
     D  b                             1    overlay(kkmbr#)
     D  yyjul                         7  0 overlay(kkmbr#:2)
     D   yyyear                       4  0 overlay(yyjul)
     D   yydays                       3  0 overlay(yyjul:5)
     D  seq#                          2  0 overlay(kkmbr#:9)
    D*
     D                 ds
     D x9mbr#                        10
     D  yyjul2                        7  0 overlay(x9mbr#:2)
     D  xxseq#                        2  0 overlay(x9mbr#:9)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##copy          s              1
     D ##dcid          s             12
     D ##fold          s             60
     D ##mbr#          s             10
     D ##rrn           s              9  0
     D @@co#           s              3
     D @@fold          s             60
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D yyjuli          s              5  0
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@fold
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtyymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*YYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtjul         plist
     C                   parm                    srdti
     C                   parm      '*MDYY'       srffmt
     C                   parm      '*JUL'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     x9ky1         klist
     C                   kfld                    docnam
    C*
     C     x9ky2         klist
     C                   kfld                    kkmbr#
    C*================================================================
    C*  MAIN PROCESSING
    C*================================================================
     C                   exsr      mainsr
    C*
     C                   eval      *inlr = *on
    C*================================================================
    C* BLDMBR - Build Member Number
    C*================================================================
     C     bldmbr        begsr
    C*
     C                   eval      yyjul2 = *zeros
     C                   eval      xxseq# = *zeros
    C*
     C                   movel(p)  'B'           b
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtjul
     C                   movel     srdto         yyjuli
    C*
     C                   z-add     dsdate        yyyear
     C                   z-add     yyjuli        yydays
    C*
     C                   eval      seq# = 99
    C*
     C     x9ky2         setgt     px902
     C                   readp     px902                                  99
    C*
     C                   if        *in99 = *off
     C                             and yyjul = yyjul2
     C                   if        xxseq# = 99
     C                   eval      seq# = 0
     C                   else
     C                   eval      seq# = xxseq# + 1
     C                   endif
     C                   else
     C                   eval      seq# = 0
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* GETWRT - Get or Write the transaction record
    C*================================================================
     C     getwrt        begsr
    C*
     C                   time                    dsdttm
    C*
     C     x9ky1         chain     px901                              99
     C                   if        *in99 = *on
    C*
     C                   exsr      bldmbr
    C*
     C                   clear                   px901
    C*
     C                   movel(p)  docnam        x9dcid
    C*
     C                   movel(p)  kkmbr#        x9mbr#
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         x9uldt
     C                   eval      x9ultm = dstime
     C                   eval      x9cvdt = *zeros
     C                   eval      x9cvtm = *zeros
     C                   eval      x9prdt = *zeros
     C                   eval      x9prtm = *zeros
     C                   move      'A'           x9stat
    C*
     C                   movel     q1pgm         x9pgm
     C                   movel     q1user        x9user
     C                   eval      x9dt = x9uldt
     C                   eval      x9time = dstime
    C*
     C                   movel     q1pgm         x9ipgm
     C                   movel     q1user        x9iusr
     C                   eval      x9idt = x9uldt
     C                   eval      x9itim = dstime
    C*
     C                   write     px901
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* MAINSR - Main processing
    C*================================================================
     C     mainsr        begsr
    C*
     C                   read      dbrcdf                                 99
     C                   dow       *in99 = *off
    C*
     C                   if        %subst(docnam:1:2) = 'ER'
    C*
     C                   exsr      getwrt
    C*
     C                   if        x9prdt = *zeros
     C                   if        x9cvdt = *zeros
     C                   movel     'Y'           ##copy
     C                   else
     C                   movel     'N'           ##copy
     C                   endif
    C*
     C                   call      'F8281C'
     C                   parm      dbrrn         ##rrn
     C                   parm      @@fold        ##fold
     C                   parm      x9dcid        ##dcid
     C                   parm      x9mbr#        ##mbr#
     C                   parm                    ##copy
    C*
     C                   else
    C*
     C                   call      'F8282C'
     C                   parm                    @@co#
     C                   parm      x9dcid        ##dcid
    C*
     C                   endif
     C                   endif
    C*
     C                   read      dbrcdf                                 99
     C                   enddo
    C*
     C                   endsr
