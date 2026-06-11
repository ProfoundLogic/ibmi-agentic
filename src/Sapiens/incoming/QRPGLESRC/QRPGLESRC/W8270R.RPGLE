    H*  (C) Copyright Fiserv Insurance Solutions, Inc., 2006
     H/TITLE W8270R
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H**************************************************************************
    H*  W8270R     : First Health Claims Ack - Upload Selection
    H*
     H*  Description:
     H*
    H*  Called from: W8270C - First Health Claims Ack - Upload Controller
    H*  Calls to   : W8271C - First Health Claims Ack - Copy Doc to WZC0P Mbr
    H*               W8272C - First Health Claims Ack - Delete Document
    H*
     H**************************************************************************
     H*  Created on 05/24/2006 by C:163 for Project 139MCV from client 139
     H**************************************************************************
    H*  Revisions:
    H*
    H*  12/05/05  C: 79  SUPPORT: 50173
    H*                   CALL F8262C WITH CO#
    H*
     H**************************************************************************
     Fwzcxl     if   e           k disk
     F                                     rename(docdtl:dbrcdf)
     Fwzc9l1    if a e           k disk
     F                                     infds(dbfds)
     Fwzc9l2    if   e           k disk
     F                                     rename(pc901:pc902)
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
     D  a                             1    overlay(kkmbr#)
     D  yyjul                         7  0 overlay(kkmbr#:2)
     D   yyyear                       4  0 overlay(yyjul)
     D   yydays                       3  0 overlay(yyjul:5)
     D  seq#                          2  0 overlay(kkmbr#:9)
    D*
     D                 ds
     D c9mbr#                        10
     D  yyjul2                        7  0 overlay(c9mbr#:2)
     D  xxseq#                        2  0 overlay(c9mbr#:9)
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
     C     c9ky1         klist
     C                   kfld                    docnam
    C*
     C     c9ky2         klist
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
     C                   movel(p)  'A'           a
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtjul
     C                   movel     srdto         yyjuli
    C*
     C                   move      dsdate        yyyear
     C                   move      yyjuli        yydays
    C*
     C                   eval      seq# = 99
    C*
     C     c9ky2         setgt     pc902
     C                   readp     pc902                                  99
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
     C     c9ky1         chain     pc901                              99
     C                   if        *in99 = *on
    C*
     C                   exsr      bldmbr
    C*
     C                   clear                   pc901
    C*
     C                   movel(p)  docnam        c9dcid
    C*
     C                   movel(p)  kkmbr#        c9mbr#
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         c9uldt
     C                   eval      c9ultm = dstime
     C                   eval      c9cvdt = *zeros
     C                   eval      c9cvtm = *zeros
     C                   eval      c9prdt = *zeros
     C                   eval      c9prtm = *zeros
     C                   move      'A'           c9stat
    C*
     C                   movel     q1pgm         c9pgm
     C                   movel     q1user        c9user
     C                   eval      c9dt = q1dt
     C                   eval      c9time = q1time
    C*
     C                   movel     q1pgm         c9ipgm
     C                   movel     q1user        c9iusr
     C                   eval      c9idt = q1dt
     C                   eval      c9itim = q1time
    C*
     C                   write     pc901
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
     C                   if        %subst(docnam:1:2) = 'CA'
    C*
     C                   exsr      getwrt
    C*
     C                   if        c9prdt = *zeros
     C                   if        c9cvdt = *zeros
     C                   movel     'Y'           ##copy
     C                   else
     C                   movel     'N'           ##copy
     C                   endif
    C*
     C                   call      'W8271C'
     C                   parm      dbrrn         ##rrn
     C                   parm      @@fold        ##fold
     C                   parm      c9dcid        ##dcid
     C                   parm      c9mbr#        ##mbr#
     C                   parm                    ##copy
    C*
     C                   else
    C*
     C                   call      'W8272C'
     C                   parm                    @@co#
     C                   parm      c9dcid        ##dcid
    C*
     C                   endif
     C                   endif
    C*
     C                   read      dbrcdf                                 99
     C                   enddo
    C*
     C                   endsr
