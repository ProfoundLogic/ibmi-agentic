     *  (C) Copyright Fiserv Insurance Solutions, Inc., 2008
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
      **********************************************************************
     *  F8680R - Selection
     *
     *  Called by: F8680C - Load Document names from Import folder to FIY2P
     *  Calls to : F8681C - Copy Document names to process into FIY9P.
     *
      **********************************************************************
      *  CREATED  :  5/19/2008   PROGRAMMER: 47    PROJECT: 129MPN
     *  Based on F8260R from Client 135
      **********************************************************************
     *  Revisions:
     *
      **********************************************************************
     Ffiy2l1    if   e           k disk
     F                                     rename(docdtl:dbrcdf)
     Ffiy9l1    if a e           k disk
     F                                     infds(dbfds)
     Ffiy9l2    if   e           k disk
     F                                     rename(py901:py902)
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
     D y9mbr#                        10
     D  yyjul2                        7  0 overlay(y9mbr#:2)
     D  xxseq#                        2  0 overlay(y9mbr#:9)
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
     C     y9ky1         klist
     C                   kfld                    docnam
    C*
     C     y9ky2         klist
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
     C     y9ky2         setgt     py902
     C                   readp     py902                                  99
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
     C     y9ky1         chain     py901                              99
     C                   if        *in99 = *on
    C*
     C                   exsr      bldmbr
    C*
     C                   clear                   py901
    C*
     C                   movel(p)  docnam        y9dcid
    C*
     C                   movel(p)  kkmbr#        y9mbr#
    C*
     C                   time                    dsdttm
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         y9uldt
     C                   eval      y9ultm = dstime
     C                   eval      y9cvdt = *zeros
     C                   eval      y9cvtm = *zeros
     C                   eval      y9prdt = *zeros
     C                   eval      y9prtm = *zeros
     C                   move      'A'           y9stat
    C*
     C                   movel     q1pgm         y9pgm
     C                   movel     q1user        y9user
     C                   eval      y9dt = y9uldt
     C                   eval      y9time = dstime
    C*
     C                   movel     q1pgm         y9ipgm
     C                   movel     q1user        y9iusr
     C                   eval      y9idt = y9uldt
     C                   eval      y9itim = dstime
    C*
     C                   write     py901
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
     C                             or %subst(docnam:1:2) = 'EV'
    C*
     C                   exsr      getwrt
    C*
     C                   if        y9prdt = *zeros
     C                   if        y9cvdt = *zeros
     C                   movel     'Y'           ##copy
     C                   else
     C                   movel     'N'           ##copy
     C                   endif
    C*
     C                   call      'F8681C'
     C                   parm      dbrrn         ##rrn
     C                   parm      @@fold        ##fold
     C                   parm      y9dcid        ##dcid
     C                   parm      y9mbr#        ##mbr#
     C                   parm                    ##copy
    C*
     C                   else
    C*
     C                   call      'F8682C'
     C                   parm                    @@co#
     C                   parm      y9dcid        ##dcid
    C*
     C                   endif
     C                   endif
    C*
     C                   read      dbrcdf                                 99
     C                   enddo
    C*
     C                   endsr
