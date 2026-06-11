     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H*****************************************************************
     H/Title W7019CW - Check Print Wrapper
     H*****************************************************************
     H* Created: 11/07/12  C:062   Project: WC2116                    *
     H*****************************************************************
     H* Revisions:                                                    *
     H*                                                               *
     H*****************************************************************
     F* Files Used
     fwdpap     if   e             Disk    ReName(ppa01:ppa0p)
     Fwmbil2    if   e           k disk    ReName(pbi01:pbi02)

    D*================================================================
    D*  Procedure Definitations
    D*================================================================
     D W7019C          pr                  extPgm('W7019C')
     D company                        3
     D group                          3
     D year                           3
     D case                           7
     D tranDate                       7
     D sequence                       5
     D formType                      10
     D copyQueue                     10

     D company         s              3
     D group           s              3
     D year            s              3
     D case            s              7
     D tranDate        s              7
     D sequence        s              5
     D formType        s             10
     D copyQueue       s             10
     D @@Rrn           s              9
     D xxRrn           s              9  0
     D ##appl          s              2    Inz('WC')
    C*================================================================
    C*  *Entry
    C*================================================================
     C     *entry        plist
     C                   parm                    @@rrn
      *
     C     key1WMBIL2    klist
     C                   kfld                    PAco#
     C                   kfld                    ##appl
     C                   kfld                    PAfnd
     C                   kfld                    PAcktp
     C                   kfld                    PAtrdt
      *
     C     key2WMBIL2    klist
     C                   kfld                    PAco#
     C                   kfld                    ##appl
     C                   kfld                    PAfnd
     C                   kfld                    PAcktp

      /free

       If @@rrn <> ' ';
          xxRrn = %int(@@rrn);
       Endif;

       // get record from WDPAP file
       Chain xxRrn ppa0p;
       If %found(WDPAP);

          Setll key1WMBIL2 pbi02;
          Reade key2WMBIL2 pbi02;
          Dow not %eof(WMBIL2);
             If BIstat <> 'I';
                Leave;
             EndIf;
             Reade key2WMBIL2 pbi02;
          Enddo;

          company = %EditC(PAco#:'X');
          group   = %EditC(PAfnd:'X');
          year    = %EditC(PAfYr:'X');
          case    = %EditC(PAcase:'X');
          sequence  = %editc(PAseq#:'X');
          trandate = %editc(PAtrdt:'X');

       Endif;

       formType  = '*STD';
       copyQueue = 'QPRINT';

       If not %eof(WMBIL2);
          If BIfTyp<>*Blanks;
             formType = BIftyp;
          EndIf;
          If BIcpyQ<>*Blanks;
             copyQueue = BIcpyq;
          EndIf;
       Endif;

       // Call program for reprint check
       W7019C(company:group:year:case:tranDate:sequence:
              formType:copyQueue);

       Return;

      /end-free
