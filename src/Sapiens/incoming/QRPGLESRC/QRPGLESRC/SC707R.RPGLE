    H*===============================================================*
     Hcopyright('(c) 2015 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
    H*===============================================================*
    H* TITLE SC707R - Remarks Print - Print Program                  *
    H*===============================================================*
    H* Revisions:                                                    *
    H*                                                               *
     H* 08/05/15 C:307 Project: SY354                                 *
     H*                -Adding 'Route To:'                            *
     H*                                                               *
     H* 08/26/14 C:298 Project: SY354                                 *
     H*                -Implement Routing Page Changes.               *
     H*                                                               *
     H* 01/28/10  C:249  Project: SY215                               *
     H*           C:189  Retrieve brand Name                          *
     H*                                                               *
     H* 12/15/08 C: 220 SUPPORT: 63991                                *
     H*                -THE DARTS PRINT REPORT INS NOT PRINTINNG R/H  *
     H*                 MARGIN ON REPORT.                             *
     H*                                                               *
    H*===============================================================*
     Fsdcml1    if   e           k disk
     Fsmarl     if   e           k disk
     Fsmsll     if   e           k disk
     Fsmorl     if   e           k disk
     Fsmupl     if   e           k disk
      ******* BEG CHG ******* 12/15/08 ********************************
     F*qsysprt   o    f   95        printer infds(prtds)
     Fqsysprt   o    f  108        printer infds(prtds)
      ******* END CHG ******* 12/15/08 ********************************

     *===============================================================*
      * Definition Specifications
     *===============================================================*
     D                 ds
     D dummy                         40
     D  dum40                         1    Overlay(dummy:40)

     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)

     D prtds           ds
     D fill1                        200
     D fill2                        166
     D xxline                         4b 0
     D xxpage                         9b 0

     D                 ds
     D subfolder                     12
     D  dfdat                         6    Overlay(subfolder:1)
     D  dtdat                         6    Overlay(subfolder:7)

     D                 ds
     D document                      27
     D  claim#                       16    Overlay(document:1)
     D  report                       10    Overlay(document:18)
     D                                     inz('Report.PDF')

     D psds          esds                  extname(szq1p)
    
     D $$user          s                   like(q1user)
     D ##data          s             80
     D ##leng          s              3  0
     D hlasq#          s                   like(arasq#)
     D hlkey           s                   like(arkey)
     D hlseq#          s                   like(arseq#)
     D n               s              3  0
     D qqkey           s             40
     D qqmode          s              1
     D qqque           s             10
     D qqfdat          s              7
     D qqtdat          s              7
     D qqpath          s            256
     D folder          s             16
     D rskey           s             80
     D rskydf          s             80
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D svkey           s                   like(cmkey)
     D x               s              5  0
     D x#key           s             79
     D x#name          s             79
     D x#pnam          s             10
     D x#rtcd          s              1
     D xxcdat          s              8  0
     D xxcm1           s             75
     D xxcm2           s             75
     D xxcm3           s             75
     D xxcm4           s             50
     D xxcm75          s                   like(arcm75)
     D xxdata          s             60
     D xxkey           s             79
     D xxone           s              1
     D xxprt           s              1
     D x1comm          s            150
     D x2comm          s            125
     D***  Begin Add  ***  01/28/10  **************************************
     D#brnd1           s             80    inz
     D#brnd2           s             80    inz
     D*
     D/copy SPrRtvBrnd
     D***  End   Add  ***  01/28/10  **************************************

     D rootFolder      c                   const('/CLAIMS OUTPUT')
    
     *===============================================================*
      * KLIST Definitions
     *===============================================================*
    C*
    C*  ADDITIONAL REMARKS FILE
     C     arky          klist
     C                   kfld                    hlkey
     C                   kfld                    hlseq#
     C                   kfld                    hlasq#
    C*
     C     arky2         klist
     C                   kfld                    hlkey
     C                   kfld                    hlseq#
    C*
    C*   SECURITY FILE
     C     slky          klist
     C                   kfld                    $$user
    C*
    C*   KEY LIST FOR THE OVERRIDE FILE.
    C*
     C     orkey         klist
     C                   kfld                    q1pgm
     C                   kfld                    xxprt
     *===============================================================*
      * PLIST Definitions
     *===============================================================*
    C*

     C     *entry        plist
     C                   parm                    qqkey
     C                   parm                    qqque
     C                   parm                    qqmode
     C                   parm                    qqfdat
     C                   parm                    qqtdat
     C                   parm                    qqpath
    C*
     C     datchk        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdyy        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy         plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDY'        srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
    C*
     C     dtmdy1        plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      '*MDYY'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr

     *===============================================================*
      * Main
     *===============================================================*
     C                   clear                   xxkey
    
      ** Check last char of parm > 30 for special dummy char.
     C                   move      qqkey         dummy
     C                   if        dum40 = '¢'
     C                   move      ' '           qqkey
     C                   endif
    C*
    C* Establish Routing Sheet Information
    C*
     C                   clear                   rskey
     C                   clear                   rskydf
     C                   movel     qqkey         rskey
    C*
    C*  INITIALIZE WORK FIELDS
    C*
     C                   time                    dsdttm
    C*
     C                   if        qqmode = 'D'
     C                   eval      *in09 = *on
     C                   else
     C                   eval      *in08 = *on
     C                   endif
    C*
     C                   read      pcm01                                  50
    C*
    C*** Build Key Definition here
     C                   movel     cmpnam        x#pnam
     C                   movel     cmkey         x#key
     C                   clear                   x#name
     C                   move      ' '           x#rtcd
     C                   call      'SRKCVT1'
     C                   parm                    x#pnam
     C                   parm                    x#key
     C                   parm                    x#name
     C                   parm                    x#rtcd
    C*
     C                   if        x#rtcd <> 'N'
     C                   movel     x#key         xxkey
     C                   movel     x#name        rskydf
     C                   else
     C                   movel     cmkey         xxkey
     C                   endif

     C                   clear                   svkey
     C                   movel     cmkey         svkey
    C*
    C*   GET THE DISTRIBUTION INFORMATION.
    C*
     C                   move      '0'           xxprt
     C     orkey         chain     por01                              60
    C*
    C*   PRINT THE DISTRIBUTION INFORMATION, IF A VALID RECORD IS FOUND.
    C*
     c*** Beg Add ***** 01/28/10 **************************************
     C                   callp     RtvBrnd('@':#Brnd1:#brnd2)
     c*** End add ***** 01/28/10 **************************************
     c*
     C                   if        *in60 = *off
     C                             and orstat = 'A'
     C     q1user        chain     pup01                              60
    C*
    C*  Left Justify Title in Field
    C*
     C                   move(p)   ortitl        xxdata
    C*
     C     ' '           check     xxdata        n
    C*
     C                   if        n <= 60
     C                   subst(p)  xxdata:n      ortitl
     C                   endif
    C*
      ***** Beg Del ***** 08/26/14 ************************************
     C***********        except    route
      ***** End Del ***** 08/26/14 ************************************
    C*
    C*  Center Text in Field
    C*
     C                   movel     ortitl        ##data
    C*
     C                   call      'SRCENTER'                           99
     C                   parm                    ##data
     C                   parm      60            ##leng
    C*
     C                   movel     ##data        ortitl
    C*
     C                   endif
     C                   except    head
    C*
     C                   if        *in50 = *on
     C                   except    errmsg
     C                   goto      term
     C                   endif
    C*
    C*
     C                   dow       *in50 = *off
    C* ================================================================
    C* Only print remarks that this user has authority to
     C                   movel     q1user        $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   clear                   $$user
     C     slky          chain     psl01                              60
     C                   if        *in60 = *on
     C                   except    errms2
     C                   goto      term
     C                   endif
     C                   endif
    C*
     C                   if        cmsecl <= slsecl
    C* ================================================================
    C* Security passed - continue with remark
    C*
    C*** Build Key Definition here
     C                   movel     cmpnam        x#pnam
     C                   movel     cmkey         x#key
     C                   clear                   x#name
     C                   move      ' '           x#rtcd

     C                   call      'SRKCVT1'
     C                   parm                    x#pnam
     C                   parm                    x#key
     C                   parm                    x#name
     C                   parm                    x#rtcd
    C*
     C                   clear                   xxkey
    C*
     C                   if        x#rtcd <> 'N'
     C                   movel     x#key         xxkey
     C                   else
     C                   movel     cmkey         xxkey
     C                   endif
    C*
     C                   eval      srdti = cmcdat
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         xxcdat
    C*
     C                   clear                   x1comm
     C                   clear                   x2comm
     C                   clear                   xxcm1
     C                   clear                   xxcm2
     C                   clear                   xxcm3
     C                   clear                   xxcm4
    C*
     C                   movel     cmcomm        x1comm
     C                   move      cmcomm        x2comm
    C*
     C                   movel     x1comm        xxcm1
     C                   move      x1comm        xxcm2
    C*
     C                   movel     x2comm        xxcm3
     C                   move      x2comm        xxcm4
    C*
     C                   if        cmkey <> svkey
     C                             and *inof <> *on
     C                   if        xxline <= 55
     C                   except    nkey
     C                   else
     C                   move      *on           *inof
     C                   endif
     C                   endif
    C*
     C                   clear                   svkey
     C                   movel     cmkey         svkey
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   except    det1
     C                   except    det2
     C                   move      *off          *inof
     C                   else
     C                   if        xxline <= 59
     C                   except    det1
     C                   except    det2
     C                   else
     C                   move      *on           *inof
     C                   except    head
     C                   except    det1
     C                   except    det2
     C                   move      *off          *inof
     C                   endif
     C                   endif
    C*
     C                   if        *inof = *on
     C                             and xxcm2 <> *blanks
     C                   except    head
     C                   except    det1
     C                   move      *off          *inof
     C                   endif
     C                   if        xxcm2 <> *blanks
     C                   except    det3
     C                   endif
    C*
     C                   if        *inof = *on
     C                             and xxcm3 <> *blanks
     C                   except    head
     C                   except    det1
     C                   move      *off          *inof
     C                   endif
     C                   if        xxcm3 <> *blanks
     C                   except    det4
     C                   endif
    C*
     C                   if        *inof = *on
     C                             and xxcm4 <> *blanks
     C                   except    head
     C                   except    det1
     C                   move      *off          *inof
     C                   endif
     C                   if        xxcm4 <> *blanks
     C                   except    det5
     C                   endif
    C*
    C* Get Additional Remarks and Print them
    C*
     C                   move      *off          xxone
     C                   clear                   hlkey
     C                   clear                   hlseq#
     C                   clear                   hlasq#
     C                   movel     cmkey         hlkey
     C                   move      cmseq#        hlseq#
     C     arky          setll     par01
     C     arky2         reade     par01                                  61
     C                   dow       *in61 = *off
    C*
     C                   if        xxone = *off
     C                             and arcm75 <> *blanks
     C                   move      *on           xxone
     C                   movel     arcm75        xxcm75
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   except    det1
     C                   move      *off          *inof
     C                   endif
    C*
     C                   except    det6
     C                   else
    C*
     C                   if        arcm75 <> *blanks
     C                   movel     arcm75        xxcm75
    C*
     C                   if        *inof = *on
     C                   except    head
     C                   except    det1
     C                   except    det6
     C                   move      *off          *inof
     C                   else
     C                   except    det7
     C                   endif
    C*
     C                   endif
     C                   endif
    C*
     C     arky2         reade     par01                                  61
     C                   enddo
    C*
     C                   endif
    C*
     C                   read      pcm01                                  50
     C                   enddo
    C*
     C* Let's generate the path statement
     C*

     C     term          tag

    C*
     C* Let's generate the path statement
     C*
     C                   clear                   qqpath
     C                   movel(p)  qqkey         folder
     C                   move      qqfdat        dfdat
     C                   move      qqtdat        dtdat
     C                   eval      claim# = folder

     C     rootFolder    cat(p)    '/':0         qqpath
     C                   cat       folder:0      qqpath
     C                   cat       '/':0         qqpath
     C                   cat       subfolder:0   qqpath
     C                   cat       '/':0         qqpath
     C                   cat       document:0    qqpath
    C*
      /Free
       //***** Beg Add ***** 08/26/14 ***********************************
       xxPrt = '0';
       Chain orKey POR01;
       If %found and orStat = 'A';
          Except Route;
       Endif;
       //***** End Add ***** 08/26/14 ***********************************
      /End-free
     C                   move      *on           *inlr
     Oqsysprt   e            route          1 06
      ***** Beg Del ***** 08/26/14 ************************************
     O**********                                  8 'XXXXXXXX'
     O**********                                 20 'XXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXX     XX'
     O**********                                 77 'XXXXXX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 34 'XX      XX'
     O**********                                 45 'XXXXXXXXXX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XXXX    XX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XXXX    XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                 10 'XX      XX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  9 'XXXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX XX   XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  8 'XXXXXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 71 'XX'
     O**********e            route          1
     O**********                                  4 'XXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX  XX  XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  5 'XXXXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX    XXXX'
     O**********e            route          1
     O**********                                  6 'XX XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX   XX XX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  7 'XX  XXX'
     O**********                                 22 'XX      XX'
     O**********                                 34 'XX      XX'
     O**********                                 41 'XX'
     O**********                                 52 'XX'
     O**********                                 67 'XX    XXXX'
     O**********                                 79 'XX      XX'
     O**********e            route          1
     O**********                                  8 'XX   XXX'
     O**********                                 21 'XXXXXXXX'
     O**********                                 33 'XXXXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX    XXXX'
     O**********                                 78 'XXXXXXXX'
     O**********e            route          3
     O**********                                  9 'XX    XXX'
     O**********                                 20 'XXXXXX'
     O**********                                 32 'XXXXXX'
     O**********                                 41 'XX'
     O**********                                 56 'XXXXXXXXXX'
     O**********                                 67 'XX     XXX'
     O**********                                 77 'XXXXXX'
     O**********e            route          2
     O**********                                 15 'ROUTE TO:'
     O**********             upusnm              67
      ***** End Del ***** 08/26/14 ************************************
      ***** Beg Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           15 'ROUTE TO:'
     O                       upusnm              67
      ***** End Add ***** 08/05/15 ************************************
     O          e            route          2
     O                                           19 'REPORT TITLE:'
     O                       ortitl              80
      ***** Beg Del ***** 08/26/14 ************************************
     O**********e            route          2
     O**********                                 19 'PROGRAM NAME:'
     O**********             orpnam              30
     O**********                                 36 'FILE:'
     O**********             orfil#              38
     O**********                                 46 'COPIES:'
     O**********             orcopy        z     50
     O**********                                 56 'HOLD:'
     O**********             orhold              61
     O**********e            route          2
     O**********                                 19 'OUTPUT QUEUE:'
     O**********             qqque               30
     O**********                                 37 'FORMS:'
     O**********             orform              48
     O**********                                 56 'SAVE:'
     O**********             orsave              61
      ***** End Del ***** 08/26/14 ************************************

     O          e            route          1
     O                                           24 'REPORT SELECTIONS:'

     O          e            route          1
     O                                           19 'Key . . . :'
     O                       qqkey               60

     O          e            route          2
    
     o*** Beg Chg *****  01/28/10  ************************************
     O****      e            route            60
     O***                                        23 '(c) Copyright Progressi'
     O***                                           've Data Solutions, Inc.,'
     O          e            route          1 59
     O                       #brnd1
     O          e            route          1
     O                       #brnd2
     o*** End Chg *****  01/28/10  ************************************
    
     Oqsysprt   e            head           1 06
     O                       dsdate              15 '  /  /    '
     O                                           56 'DATABASE AND REMARKS'
     O                                           72 'TRACKING SYSTEM'
     O                       dstime              95 '  :  :  '
    
     O          e            head           2
     O                       q1pgm               15
     O                                           61 'REMARKS LISTING'
     O                                           90 'PAGE:'
     O                       page                95
    
     O          e            head           1
     O                       xxkey               84
    
     O          e            head           1
     O                       rskydf              85
    
     O          e            head           1
     O                                           11 'REMARK'
     O                                           22 'REMARK'
     O                                           55 'REMARK'
    
     O          e            head           1
     O                                           10 'DATE'
     O                                           21 'TYPE'
    
     O          e            nkey        2  1
     O                       xxkey               84
    
     O          e            nkey           1
    
     O          e            det1        1  1
     O                       xxcdat              15 '  /  /    '
     O                       cmrmcd              21
    
     O          e            det2           1
     O                       xxcm1               95
    
     O          e            det3           1
     O                       xxcm2               95
    
     O          e            det4           1
     O                       xxcm3               95
    
     O          e            det5           1
     O                       xxcm4               75
    
     O          e            det6           1
     O                       xxcm75              95
    
     O          e            det7           1
     O                       xxcm75              95
    
     O          e            errmsg      3
     O                                           24 '*** NO RECORDS MEET'
     O                                           46 'SELECTED CRITERIA ***'
    
     O          e            errms2      3  1
     O                                           28 '** YOU ARE NOT ENROLLED'
     O                                           52 'IN DARTS SECURITY FILE.'
    
     O          e            errms2         1
     O                                           32 'SEE SYSTEM ADMINISTRATOR'
     O                                           57 'FOR FURTHER INFORMATION.'
