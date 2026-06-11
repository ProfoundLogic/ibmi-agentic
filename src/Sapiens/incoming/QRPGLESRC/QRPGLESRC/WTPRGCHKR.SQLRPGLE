     h Copyright('(c) 2017 StoneRiver, Inc. All rights reserved. -
     h StoneRiver is a trademark of StoneRiver, Inc.')
     h**********************************************************************
     h/title WTPRGCHKR - Purging Process
     h option(*nodebugio: *srcstmt)
     h dftactgrp(*no) actgrp(*caller)
     h/define   PROFOUNDUI
     h bnddir('WTBNDDIR')
     h**********************************************************************
    *====================================================================*
    *  Created    : 05/05/17                                             *
    *  Programmer : C:306                                                *
    *  Project    : WC2408                                               *
    *  Description: Check Purging process.                               *
    *                                                                    *
    *====================================================================*
    *  Revisions:                                                        *
     *                                                                    *
    *                                                                    *
    * ------------------------------------------------------------------ *
    *  NOTE: Please add comments to the top of Revisions.                *
    * ------------------------------------------------------------------ *
    *====================================================================*

     fwlcksprg  uf a e             disk    rename(pck01:pck0p)

      * data structure
     d psds          esDs                  extname(szq1p)
     d dbfds           ds
     d  dbrrn                397    400b 0


     d intPurgeCnt     s              8  0

     d strGrpChk       s              1a
     d intCompany      s              3  0
     d intStartDate    s              7  0
     d intEndDate      s              7  0
     d intGroup        s              3  0
     d

     d/copy sprDate

      * Entry parameter
     d wtprgchkrp      pr                  Extpgm('WTPRGCHKR')
     d  inGroupChk                    1
     d  inCompany                     3
     d  inStartDate                   7
     d  inEndDate                     7
     d  inGroup                       3

     d wtprgchkrp      pi
     d  inGroupChk                    1
     d  inCompany                     3
     d  inStartDate                   7
     d  inEndDate                     7
     d  inGroup                       3

      /free

          strGrpChk    = inGroupChk;
          intCompany   = %dec(inCompany:3:0);
          intStartDate = %dec(inStartDate:7:0);
          intEndDate   = %dec(inEndDate:7:0);

          If inGroup <> *blanks;
             intGroup = %dec(inGroup:3:0);
          Else;
             intGroup = *zeros;
          EndIf;

         // If Group check schedule.
          If StrGrpChk = 'Y';
             Select;
             When intStartDate <> *zeros and intEndDate <> *zeros
              and intGroup = *zeros;

               exec sql
               select count(*) into :intPurgeCnt from WAC3P
                where CPCO# = :intCompany and CPPRDT between
                :intStartDate and :intEndDate;
               Exsr $WriteLogRec;

               exec sql
               Delete from WAC3P where CPCO# = :intCompany and
                CPPRDT between :intStartDate and :intEndDate;

             When intStartDate <> *zeros and intEndDate <> *zeros
              and intGroup <> *zeros;

               exec sql
               select count(*) into :intPurgeCnt from WAC3P
                where CPCO# = :intCompany and CPFND = :intGroup
                and CPPRDT between :intStartDate and :intEndDate;
               Exsr $WriteLogRec;

               exec sql
               Delete from WAC3P where CPCO# = :intCompany and CPFND = :intGroup
                and CPPRDT between :intStartDate and :intEndDate;

             EndSl;

         // If Non Group check schedule.
          Else;
             Select;
             When intStartDate <> *zeros and intEndDate <> *zeros
              and intGroup = *zeros;

               exec sql
               select count(*) into :intPurgeCnt from WACPP
                where CPCO# = :intCompany and CPPRDT between
                :intStartDate and :intEndDate;
               Exsr $WriteLogRec;

               exec sql
               Delete from WACPP where CPCO# = :intCompany and
                CPPRDT between :intStartDate and :intEndDate;

             When intStartDate <> *zeros and intEndDate <> *zeros
              and intGroup <> *zeros;

               exec sql
               select count(*) into :intPurgeCnt from WACPP
                where CPCO# = :intCompany and CPFND = :intGroup
                and CPPRDT between :intStartDate and :intEndDate;
               Exsr $WriteLogRec;

               exec sql
               Delete from WACPP where CPCO# = :intCompany and CPFND = :intGroup
                and CPPRDT between :intStartDate and :intEndDate;

             EndSl;

          EndIf; //StrGrpChk = 'Y';

          *inLr = *on;


      // Writing a Purge history log record Subrotuine.
       BegSr $WriteLogRec;
          CKiUsr  = q1user;
          CKiDt   = Dateto7(SystemDate());
          CKiTm   = Timeto6(SystemTime());
          CKiPgm  = q1pgm;
          CKprgCnt = intPurgeCnt;
          CKfrDt   = intStartDate;
          CKtoDt   = intEndDate;
          CKgrp = intGroup;

          If strGrpChk = 'Y';
             CKgrpChk = 'Y';
          Else;
             CKgrpChk = 'N';
          EndIf;

          Write pck0p;
       EndSr;

      /End-free
