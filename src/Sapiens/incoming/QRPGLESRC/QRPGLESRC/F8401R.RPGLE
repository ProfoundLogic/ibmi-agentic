     Hcopyright('(c) 2010 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*
     H/TITLE F8401R - Procura Return File Build Member Name
     H************************************************************************
    H*                                                                      *
    H*  F8401R - Procura Return File Build Member Name                      *
    H*                                                                      *
    H*  Called by: F8401C - Load Document names from Procura fldr to FAZ6P  *
    H*  Calls to : F8402C - Copy Document names to process into FAZ9P.      *
    H*                                                                      *
     H************************************************************************
     H*  Created: 12/18/10  C:251  for Project: 146MCV                       *
    H************************************************************************
    H*  Revisions:                                                          *
    H*                                                                      *
     H************************************************************************
     Ffaz6l1    if   e           k disk
     F                                     rename(docdtl:dbrcdf)
     Ffaz9l1    if a e           k disk
     F                                     infds(dbfds)
     Ffaz9l2    if   e           k disk
     F                                     rename(pz901:pz902)
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
     D z9mbr#                        10
     D  yyjul2                        7  0 overlay(z9mbr#:2)
     D  xxseq#                        2  0 overlay(z9mbr#:9)
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
     C     z9ky1         klist
     C                   kfld                    docnam
    C*
     C     z9ky2         klist
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
     C     z9ky2         setgt     pz902
     C                   readp     pz902                                  99
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
     C     z9ky1         chain     pz901                              99
     C                   if        *in99 = *on
    C*
     C                   exsr      bldmbr
    C*
     C                   clear                   pz901
     C                   movel(p)  docnam        z9dcid
     C                   movel(p)  kkmbr#        z9mbr#
     C                   time                    dsdttm
     C                   eval      z9prdt = *zeros
     C                   eval      z9prtm = *zeros
     C                   move      'A'           z9stat
     C                   movel     q1pgm         z9pgm
     C                   movel     q1user        z9user
    C*
     C                   eval      srdti = dsdate
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         z9dt
     C                   eval      z9time = dstime
    C*
     C                   movel     q1pgm         z9ipgm
     C                   movel     q1user        z9iusr
     C                   eval      z9idt = z9dt
     C                   eval      z9itim = dstime
    C*
     C                   write     pz901
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
     C                   if        %subst(docnam:1:2) = 'PR'
    C*
     C                   exsr      getwrt
    C*
     C                   if        z9prdt = *zeros
    C*
     C                   call      'F8402C'
     C                   parm      dbrrn         ##rrn
     C                   parm      @@fold        ##fold
     C                   parm      z9dcid        ##dcid
     C                   parm      z9mbr#        ##mbr#
     C                   parm      'Y'           ##copy
    C*
     C                   else
    C*
     C                   call      'F8403C'
     C                   parm                    @@co#
     C                   parm      z9dcid        ##dcid
    C*
     C                   endif
     C                   endif
    C*
     C                   read      dbrcdf                                 99
     C                   enddo
    C*
     C                   endsr
