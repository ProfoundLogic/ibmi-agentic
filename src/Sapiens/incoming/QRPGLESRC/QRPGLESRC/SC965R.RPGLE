    H*===============================================================*
     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*===============================================================*
    H* TITLE SC965R - Print Single Comment                           *
    H*===============================================================*
    H* Created on 03/14/2005 by c:62 for Project 129IJS              *
    H* Revisions:                                                    *
    H*                                                               *
    H*===============================================================*
     Fsdcmla    if   e           k disk
     Fsmcol     if   e           k disk
     Fsmupl     if   e           k disk
     Fqsysprt   o    f  132        printer infds(prtds)

     *===============================================================*
      * Definition Specifications
     *===============================================================*
     D                 ds
     D docsuf                        24a
     D  type                          2a   overlay(docsuf:6)
     D                 ds
     D comment                      275a
     D  comment1                     92a   overlay(comment:1)
     D  comment2                     92a   overlay(comment:93)
     D  comment3                     91a   overlay(comment:185)

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D prtds           ds
     D fill1                        200
     D fill2                        166
     D xxline                         4b 0
     D xxpage                         9b 0

     D psds          esds                  extname(szq1p)

     D ##data          s             80
     D ##leng          s              3  0
    
     D @@key           s             40
     D @@seq#          s              5
     D @@path          s            256
     D @@docsuf        s             24

     D companyName     s             60
     D reportName      s             60
     D remarkDate      s               d   datfmt(*usa)

     D kkkey           s                   like(cmkey)
     D kkseq#          s                   like(cmseq#)

     D reportNameC     c                   'Single Darts Comment Print'
     *===============================================================*
      * KLIST Definitions
     *===============================================================*
    C*
    C*  DARTS REMARKS FILE
     C     cmky          klist
     C                   kfld                    kkkey
     C                   kfld                    kkseq#
     *===============================================================*
      * PLIST Definitions
     *===============================================================*
    C*
     C     *entry        plist
     C                   parm                    @@key
     C                   parm                    @@seq#
     C                   parm                    @@docsuf
     *===============================================================*
      * Main
     *===============================================================*
     C                   time                    dsdttm
    C*
    C*  User Parms
    C*
     C     q1user        chain     pup01
    C*
    C*  Center Company Name
    C*
     C     upco#         chain     pco01
     C                   movel(p)  coname        ##data
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel(p)  ##data        companyName
    C*
    C*  Center Report Name
    C*
     C                   movel(p)  reportNameC   ##data
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel(p)  ##data        reportName
    C*
     C                   except    head
    C*
     C                   eval      kkkey = @@key
     C                   move      @@seq#        kkseq#
     C     cmky          chain     pcm01
     C                   if        %found
     C     *cymd         move      cmcdat        remarkDate
     C                   eval      comment = cmcomm
     C                   except    body
     C                   endif
    C*
     C                   eval      docsuf = @@docsuf
     C                   eval      type = '-C'
     C                   eval      @@docsuf = docsuf
    C*
     C                   move      *on           *inlr
     Oqsysprt   e            head           1 02
     O                       dsdate              10 '  /  /    '
     O                       companyName       +006
     O                       dstime            +006 '  :  :  '
    
     O          e            head           2
     O                       q1pgm               10
     O                       reportName        +006
     O                                         +006 'Page:'
     O                       page              +001
    
     O          e            body           1
     O                                            4 'Key:'
     O                       cmkey             +001
     O                                         +002 'Sequence # :'
     O                       cmseq#        4   +001
    
     O          e            body           2
     O                                           11 'Diary user:'
     O                       cmuser            +001
     O                                         +002 'Remark code:'
     O                       cmrmcd            +001
     O                                         +005 'Remark date:'
     O                       remarkDate        +001
    
     O          e            body           1
     O                                            8 'Comment:'
    
     O          e            body        1  1
     O                       comment1            92
    
     O          e            body           1
     O                       comment2            92
    
     O          e            body           1
     O                       comment3            91
