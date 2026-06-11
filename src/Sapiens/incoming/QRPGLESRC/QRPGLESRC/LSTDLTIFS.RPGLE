     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*
     H***********************************************************************
     H* List of Deleted Images in a Queue
     H***********************************************************************
     H* Create onb 08/21/2014 by c:62 for Support 86551
     H***********************************************************************
     H* Revisions:
     H*
     H***********************************************************************
     Fimiel1    uf   e           k Disk
     Fimdol1    if   e           k Disk
     Fqsysprt   o    f  132        Printer Oflind(*inob)
     F*
     d ChkIfsObj       pr                  ExtPgm('CHKIFSOBJ')
     d   inPath                     256
     d   outResult                     n

     d psds          esds                  extname(SZQ1P)

     d                 ds
     d  DSdttm                 1     14  0
     d  DStime                 1      6  0
     d  DSdate                 7     14  0

     D @@qid           s                   like(IEqid)
     d inPath          s            256
     d outResult       s               n
     d count           s              5s 0 inz(0)
     d minDate         s              7s 0 inz(9999999)
     d maxDate         s              7s 0 inz(0)
     d dateMaint       s               d   datfmt(*usa)
     d docPath         s             50
     d group           s              3
     d year            s              3
     d case            s              7

     C     *entry        plist
     C                   parm                    @@qid

     C                   Time                    DSdttm
      /free

                         Except Head;
                         Chain @@qid pie01;
                         If %Found(IMIEL1);
                            DoU %Eof(IMIEL1);
                               Chain IEdcid pdo01;
                               If %Found(IMDOL1);
                                  If DOosys=' ';
                                     inPath=DOuncn;
                                     outResult=*Off;
                                     ChkIfsObj(inPath:outResult);
                                     If outResult=*Off;
                                        count=count+1;
                                        If IEiDt<minDate;
                                           minDate=IEiDt;
                                        EndIf;
                                        If IEiDt>maxDate;
                                           maxDate=IEiDt;
                                        EndIf;
                                        dateMaint=%Date(IEiDt:*ymd);
                                        docPath=%Subst(DOuncn:1:100);
                                        group=%Subst(DOdkey:4:3);
                                        year=%Subst(DOdkey:7:3);
                                        case=%Subst(DOdkey:10:7);
                                        Except Detail;
                                     EndIf;
                                  EndIf;
                               EndIf;
                               ReadE @@qid pie01;
                            EndDo;
                         EndIf;

                         If count=0;
                            docPath='No missing images.';
                            Except Detail;
                         EndIf;

                         Except Total;
      /end-free
     C                   seton                                        lr
     OQSYSPRT   E            Head           1 03
     O                       DSTIME               8 '  :  :  '
     O                                           45 ' Image File Listing  '
     O                       DSdate             123 '  /  /    '
     O          E            Head           2
     O                       Q1PGM               10
     O                                           47 '    Missing Images     '
     O                       PAGE          Z    123
     O                                          118 'Page'
     O          E            Head           2
     O                                            9 'Queue ID:'
     O                       @@qid             +001
     O          E            Head           2
     O                                            8 'Document'
     O                                           65 'Creation Date'
     O                                           72 'Group'
     O                                           78 'Year'
     O                                           87 '   Case'

     O          E            Detail         1
     O                       docPath             50
     O                       dateMaint           62
     O                       group               72
     O                       year                78
     O                       case                87

     O          E            Total       1  2
     O                                            6 'Count:'
     O                       count         4   +002
     O                                         +002 'Min Date'
     O                       minDate           +002
     O                                         +002 'Max Date'
     O                       maxDate           +002
