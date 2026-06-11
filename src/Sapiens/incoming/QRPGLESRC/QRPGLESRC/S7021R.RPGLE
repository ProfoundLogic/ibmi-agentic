     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H*====================================================================
     H/TITLE F7021R - ICD-9-CM Diagnosis Code File Listing
     H*====================================================================
     H*  Created: 05/20/09   C:33   Project: WC1770
     H*====================================================================
     H*  Revisions:
     H*
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H*====================================================================
     Fsmorl     if   e           k disk    rename(por01:por00)
     Fsmcol     if   e           k disk    rename(pco01:pco00)
     Fsmupl     if   e           k disk    rename(pup01:pup00)
     Fsmdil     if   e           k disk    rename(pdi01:pdi00)
     F*
     Fqsysprt   o    f  132        printer oflind(*inoa)
     F*
     D title           c                   'ICD-9-CM Diagnosis Code File-
     D                                      Listing'
     D*
     D @@Co#           s              3
     D @@From          s              5
     D @@To            s              5
     D @@Que           s             10
     D @@Mode          s              1
     D*
     D sysDt           s               d   datfmt(*USA)
     D sysTm           s               t   timfmt(*USA)
     D xxCo#           s                   like(COCO#)
     D xxFrom          s                   like(@@From)
     D xxTo            s                   like(@@To)
     D xxQue           s                   like(@@Que)
     D xxICD9          s                   like(DIICD9)
     D xxPrt           s                   like(ORFIL#)
     D xxRcds          s              7  0
     D printFrom       s              5
     D printTo         s              5
     D*
     D/copy sPrDate
     D/copy sPrString
     D*
     D psds          esds                  extname(SZQ1P)
     C*
     C     diKey         klist
     C                   kfld                    XXICD9
     C*
     C     orKey         klist
     C                   kfld                    q1pgm
     C                   kfld                    xxPrt
     C*
     C*
     C     *entry        plist
     C                   parm                    @@Co#
     C                   parm                    @@From
     C                   parm                    @@To
     C                   parm                    @@Que
     C                   parm                    @@Mode
     C*
     C                   movel     @@From        xxFrom
     C                   movel     @@To          xxTo
     C                   move      @@Co#         xxCo#
     C                   z-add     0             xxRcds
     C*
     C* FIRST CYCLE PROCESSING
     C*
     C                   eval      sysDt = SystemDate
     C                   eval      sysTm = SystemTime
     C*
     C*   SET-UP AND INITIALIZE
     C*
     C                   eval      *in09 = @@Mode = 'D'
     C                   eval      *in08 = @@Mode <> 'D'
     C*
     C*   DETERMINE LOOP CONDITIONS
     C*
     C                   if        xxTo = *blanks
     C                   eval      xxFrom = *loval
     C                   eval      xxTo   = *hival
     C                   eval      printFrom = '* ALL'
     C                   eval      printTo   = '* ALL'
     C                   endif
     C*
     C*   GET THE USER PARAMETERS FILE.
     C*
     C                   clear                   UPUSNM
     C     q1user        chain     pup00                              60
     C*
     C     xxCo#         chain     pco00                              60
     C*
     C* Get the distribution information
     C*
     C                   move      '0'           xxPrt             1
     C     orKey         chain     por00                              60
     C*
     C* Print the distribution information
     C*
     C                   if        %found
     C                   eval      ORTITL = LeftJstify(ORTITL)
      ***** Beg Del ***** 08/26/14 ************************************
     C***********        except    route
      ***** End Del ***** 08/26/14 ************************************
     C                   endif
     C*
     C                   if        ORTITL = *blank
     C                   eval      ORTITL = title
     C                   endif
     C*
     C                   eval      ORTITL = CenterFld(ORTITL)
     C                   eval      CONAME = CenterFld(CONAME)
     C*
     C                   except    HEAD
     C*
     C* Get starting record and start loop
     C*
     C                   movel     xxFrom        XXICD9
     C     diKey         setll     pdi00
     C                   read      pdi00                                  60
     C*
     C                   dow       not *in60 and DIICD9 <= xxTo
     C*
     C* Print the detail
     C*
     C                   if        *inoa
     C                   except    HEAD
     C                   move      '0'           *INOA
     C                   endif
     C                   except    DET
     C*
     C                   ADD       1             xxRcds
     C*
     C                   read      pdi00                                  60
     C                   enddo
     C*
     C*   END PROGRAM
     C*
     C                   move      '1'           *INLR
     C*
     C                   if        *inoa
     C                   except    HEAD
     C                   move      '0'           *INOA
     C                   endif
     C                   except    TOT
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR00;
       If %found;
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C*
     O/copy sCopyRout
     O          E            ROUTE          1
     O                                           23 'REPORT SELECTION:'
     O          E            ROUTE          1
     O                                           23 'FROM CODE:'
     O                       printFrom           29
     O          E            ROUTE          1
     O                                           23 'TO CODE:'
     O                       printTo             29
     O          E            ROUTE          1 59
     O                                         +001 '(c)'
     O                       *YEAR             +001
     O                                         +001 'StoneRiver, Inc.'
     O                                         +001 'All rights reserved.'
     O          E            ROUTE          1
     O                                         +001 'StoneRiver is a'
     O                                         +001 'trademark of'
     O                                         +001 'StoneRiver, Inc.'
     O          E            HEAD           1 06
     O                       sysTm               08
     O                       CONAME              85
     O                       sysDt              122
     O          E            HEAD           2
     O                       Q1PGM               10
     O                       ORTITL              96
     O                                          116 'PAGE:'
     O                       PAGE          Z    122
     O          E            HEAD           2
     O                                           58 'RANGE:'
     O                       printFrom           67
     O                                           70 'TO'
     O                       printTo             76
     O          E            HEAD           1
     O                                           08 'CODE'
     O                                           34 'GENERAL CATEGORY'
     O                                          129 'STATUS'
     O          E            HEAD           2
     O                                           40 'SPECIFIC CATEGORY'
     O          E            DET            1
     O                       DIICD9              09
     O                       DIDSC1              68
     O                       DISTAT             126
     O          E            DET            3
     O                       DIDSC2             123
     O          E            TOT         2  1
     O                                           69 '# OF RECORDS LISTED:'
     O                       xxRcds        Z     78
