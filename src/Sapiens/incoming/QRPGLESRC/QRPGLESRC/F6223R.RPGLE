     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H/TITLE F6223R - 824 Application Advice Processing               *
     H*****************************************************************
     H*  CREATED BY:                                                  *
     H*                                                               *
     H*  07/16/14  C:079 Project: MC376                               *
     H*  Program reads all the 824 file data and write error records  *
     H*  on a report and updates the log and transmission files       *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FFE824P    if   e             disk
     FFE824ISAP if   e             disk
     F                                     RENAME(P82401:P824ISA)
     FFE824GSP  if   e             disk
     F                                     RENAME(P82401:P824GS)
     FFE824STP  if   e             disk
     F                                     RENAME(P82401:P824ST)
     FFE824BGNP if   e             disk
     F                                     RENAME(P82401:P824BGN)
     FFE824N1P  if   e             disk
     F                                     RENAME(P82401:P824N1)
     FFE824OTIP if   e             disk
     F                                     RENAME(P82401:P824OTI)
     FFE824REFP if   e             disk
     F                                     RENAME(P82401:P824REF)
     FFE824DTMP if   e             disk
     F                                     RENAME(P82401:P824DTM)
     FFE824LMP  if   e             disk
     F                                     RENAME(P82401:P824LM)
     FFE824LQP  if   e             disk
     F                                     RENAME(P82401:P824LQ)
     FFE824REDP if   e             disk
     F                                     RENAME(P82401:P824RED)
     FFE824SEP  if   e             disk
     F                                     RENAME(P82401:P824SE)
     FFE824GEP  if   e             disk
     F                                     RENAME(P82401:P824GE)
     FFE824IEAP if   e             disk
     F                                     RENAME(P82401:P824IEA)
     FFEMFL2    uf a e           k disk
     F*
     F* Define printer file -
     F*
     FQSYSPRT   O    F  160        PRINTER OFLIND(*inof)
     D*
     D @@Co#           s              3A
     D @@JSt           s              2A
     D @@TSet          s              3A
     D*
     D psds          esds                  extname(szq1p)
     Dintcntto         S              9  0 inz(0)
     DintcntAc         S              9  0 inz(0)
     DintcntRe         S              9  0 inz(0)
     DflgStatus        S              1A   inz('0')
     Dflgoti           S              3  0 inz(0)
     D*
     D xxLINE          S              3  0 inz(0)
     D xxCO#           S              3  0
     D xxJST           S              2A
     D xxTSET          S              3A
     D xxBCH#          S             15A
     D prCode          S             50A
     D prLineNo        S             10A
     D prEob#          S             20A
     D prValSnt        S             10A
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D*
     D srdti           s              8  0
     D srdto           s             10
     D srerr           s              1
     D srffmt          s              8
     D srtfmt          s              8
     D*
     DCallPgm          Pr                  ExtPgm('SRCVTDAT')
     D srdti                          8  0
     D srffmt                         8
     D srtfmt                         8
     D srdto                         10
     D srerr                          1
     D*
     *
     C*
     C     *entry        plist
     C                   parm                    @@CO#
     C                   parm                    @@Jst
     C                   parm                    @@TSet
     C*
     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     keyFEMFL2     Klist
     C                   kfld                    xxCO#
     C                   kfld                    xxJST
     C                   kfld                    xxTSET
     C                   kfld                    xxBCH#
     C*
     C     *LIKE         DEFINE    PAGE          XXPAGE
     C     *LIKE         DEFINE    LQ02          prELERNO
     C     *LIKE         DEFINE    RED06         prElNo
     C***********************************************************
     C* Main program starts here -
     C***********************************************************
      /Free
         Exsr MainSr;
         //Print detail line record-
            if xxLine > 52 ;
               *inof = *on ;
               except PAGLN1 ;
               except head2 ;
               except dtl2 ;
               xxLINE = xxLINE + 2 ;
               *inof = *off ;
            else;
               except head2 ;
               except dtl2 ;
               xxLINE = xxLINE + 1 ;
            endif ;
         *inlr = *on ;

       Begsr MainSr ;

          xxCo#  = %Dec(@@Co#:3:0);
          xxJSt  = @@Jst;
          xxTSet = @@TSet;

          Exsr ClrDtlRec ;
          xxLINE = 1 ;
          Except PAGLN1 ;
          Except Head1 ;

          Read P82401 ;
          Dow not %eof(FE824P) ;

          If %subst(data824:1:3) = *blanks;
             Read P82401 ;
             Iter;
          Endif;

          Select ;

          When %subst(data824:1:3) = 'ISA' ;

            //Do nothing

          When %subst(data824:1:2) = 'GS' ;

            //Do nothing

          When %subst(data824:1:2) = 'ST' ;

            //Do nothing

          When %subst(data824:1:3) = 'BGN' ;

             read P824BGN ;

             flgOti = 0;
             mfBch# = bgn06 ;
             xxBCH# = bgn06 ;
             intcntto += 1;
             // Chain out to FEMFL2
             Chain keyFEMFL2 FEMFL2;
                if not %found(FEMFL2) ;

                //If record not present for transaction id,
                //write it to the report
                   flgStatus = '1' ;

                   //Write Report
                   Exsr ClrDtlRec ;
                   prEob#  = OTI03;
                   prCode = 'Batch control number does not exist' ;

                   if xxLine > 52 ;
                   *inof = *on ;
                   except head1 ;
                   xxLine = xxLine + 2 ;
                   except pagln1 ;
                   *inof = *off ;
                   endif ;

                   except dtl1 ;
                   xxLine = xxLine + 1 ;

                endif ;

          When %subst(data824:1:2) = 'N1' ;

            //Do nothing

          When %subst(data824:1:3) = 'OTI' ;

            Exsr SrOti;

          When %subst(data824:1:3) = 'REF' ;

            If otI03 = 'IX';
               Exsr SrRef;
            Endif;

          When %subst(data824:1:3) = 'DTM' ;

            //Do something

          When %subst(data824:1:2) = 'LM' ;

            //Do something

          When %subst(data824:1:2) = 'LQ' ;

            Exsr SrLq;

          When %subst(data824:1:2) = 'RED' ;

            Exsr SrRed;

          When %subst(data824:1:2) = 'SE' ;

            //Do something

          When %subst(data824:1:2) = 'GE' ;

            //Do nothing

          When %subst(data824:1:3) = 'IEA' ;

            //Do nothing

          endsl ;

            Read P82401 ;

          enddo ;
       Endsr;


       // Subroutine to clear report-detail line variables.
       Begsr ClrDtlRec;
         clear prEob#;
         clear prLineNo;
         clear prElNo;
         clear prELERNO;
         clear prValSnt;
         clear prCode;
       EndSr;

       // Subroutine to execute OTI file loop.
       Begsr SrOti;
         Read FE824OTIP;
         Select;

         When otI01 = 'TA';
           //update, set LGSTS = 'IA' where FELGP/LGBCH# = BGN06.
           Exec SQL
             Update  FELGP set LGSTS = 'IA', LGUSER = :Q1USER,
             LGPGM = :Q1PGM , LGTIME = :Q1TIME  , LGDT = :Q1DT
             Where LGBCH# = :BGN06;

         When OTI01 = 'TR';
           //Update FEMFP with MFRJTF = Y and write report.
           Exec SQL
             Update FEMFP set
             MFRJTF = 'Y', MFUSER = :Q1USER, MFPGM = :Q1PGM,
             MFTIME = :Q1TIME, MFDT = :Q1DT
             where MFBCH# = :BGN06;
               //Write Report
                   Exsr ClrDtlRec ;
                   prEob# = OTI03;
                   prCode = 'TR-Transaction set rejected';

                   if xxLine > 52;
                   *inof = *on;
                   except head1;
                   xxLINE = xxLINE + 2;
                   except PAGLN1;
                   *inof = *off;
                   endif;

                   except dtl1 ;
                   xxLINE = xxLINE + 1 ;

         When OTI01 = 'IA';
           //update, set LGSTS = 'IA' where FELGP/LGBCH# = BGN06
           Exec SQL
             Update  FELGP set LGSTS = 'IA', LGUSER = :Q1USER,
             LGPGM = :Q1PGM , LGTIME = :Q1TIME  , LGDT = :Q1DT
             where LGBCH# = :BGN06;

             intcntAc += 1;

               // Write Report
                   Exsr ClrDtlRec ;
                   prEob# = OTI03;
                   prCode = 'IA-Item was accepted';

                   if xxLine > 52 ;
                   *inof = *on ;
                   except head1 ;
                   xxLINE = xxLINE + 2 ;
                   except PAGLN1 ;
                   *inof = *off ;
                   endif ;

                   except dtl1 ;
                   xxLINE = xxLINE + 1 ;

         When OTI01 = 'IR';
           //update, set LGSTS = 'IR' where FELGP/LGBCH# = BGN06
           Exec SQL
             Update  FELGP set LGSTS = 'IR', LGUSER = :Q1USER,
             LGPGM = :Q1PGM , LGTIME = :Q1TIME  , LGDT = :Q1DT
             Where LGBCH# = :BGN06;

             intcntRe += 1;

               // Write Report
                   Exsr ClrDtlRec ;
                   prEob# = OTI03;
                   prCode = 'IR-Item was rejected';

                   if xxLine > 52 ;
                   *inof = *on ;
                   except head1 ;
                   xxLINE = xxLINE + 2 ;
                   except PAGLN1 ;
                   *inof = *off ;
                   endif ;

                   except dtl1 ;
                   xxLINE = xxLINE + 1 ;

         When OTI01 = 'IE';
           //update, set LGSTS = 'IE' where FELGP/LGBCH# = BGN06
           Exec SQL
             Update  FELGP set LGSTS = 'IE', LGUSER = :Q1USER,
             LGPGM = :Q1PGM , LGTIME = :Q1TIME  , LGDT = :Q1DT
             Where LGBCH# = :BGN06;

               // Write Report
                   Exsr ClrDtlRec ;
                   prEob# = OTI03;
                   prCode = 'IE-Item was accepted with error';

                   if xxLine > 52 ;
                   *inof = *on ;
                   except head1 ;
                   xxLINE = xxLINE + 2 ;
                   except PAGLN1 ;
                   *inof = *off ;
                   endif ;

                   except dtl1 ;
                   xxLINE = xxLINE + 1 ;

         Other;
           flgoti = 1 ;

         EndSl;
       EndSr;

       Begsr SrRef;

          //Write Report
          Exsr ClrDtlRec ;
          prEob#   = OTI03;
          prLineNo= REF02;

          if xxLine > 52 ;
          *inof = *on ;
          except head1 ;
          xxLINE = xxLINE + 2 ;
          except PAGLN1 ;
          *inof = *off ;
          endif ;

          except dtl1 ;
          xxLINE = xxLINE + 1 ;

       Endsr;

       Begsr SrLq;

          //Write Report
          Exsr ClrDtlRec ;
          prEob#   = OTI03;
          prELERNO  = LQ02;

          if xxLine > 52 ;
          *inof = *on ;
          except head1 ;
          xxLINE = xxLINE + 2 ;
          except PAGLN1 ;
          *inof = *off ;
          endif ;

          except dtl1 ;
          xxLINE = xxLINE + 1 ;

       Endsr;

       Begsr SrRed;

          //Write Report
          Exsr ClrDtlRec ;
          prEob#   = OTI03;
          prElNo  = RED06;

            if xxLine > 52 ;
            *inof = *on ;
            except head1 ;
            xxLINE = xxLINE + 2 ;
            except PAGLN1 ;
            *inof = *off ;
            endif ;

          except dtl1 ;
          xxLINE = xxLINE + 1 ;

       Endsr;
      /End-Free
     C
     OQSYSPRT   E            ROUTE          1 06
     O                                           19 'REPORT TITLE:'
     O*                      ORTITL              80
     O                                           35 'EDI 824 APPLICATION'
     O                                           58 'ADVICE REPORT'
     O          E            ROUTE          1
     O                                           19 'PROGRAM NAME:'
     O*                      ORPNAM              30
     O                                           26 'F6223R'
     O*                      ORPNAM              30
     O                                           36 'FILE:'
     O*                      ORFIL#              38
     O*                                          46 'COPIES:'
     O*                      ORCOPY        Z     50
     O*                                          56 'HOLD:'
     O*                      ORHOLD              61
     O*         E            ROUTE          2
     O*                                          19 'OUTPUT QUEUE:'
     O*              08      OROUTQ              30
     O*                 09   QQQUE               30
     O*                                          37 'FORMS:'
     O*                      ORFORM              48
     O*                                          56 'SAVE:'
     O*                      ORSAVE              61
     O**   Headers -
     O          E            Head1          1
     O                                           20 'EOMB Number'
     O                                           33 'Line Number'
     O                                           48 'Element Number'
     O                                           61 'Error Number'
     O                                           72 'Value Sent'
     O                                           80 'Code'
     O**   Headers for displaying total values-
     O          E            Head2          1  1
     O                                           28 'Total EOBs Processed : '
     O          E            Head2          1
     O                                           28 'Total EOBs Accepted : '
     O          E            Head2          1
     O                                           28 'Total EOBs Rejected : '
     O**   Detail Records -
     O          E            DTL1           1
     O                       prEob#              20
     O                       prLineNo            33
     O                       prElNo              48
     O                       prELERNO            61
     O                       prValSnt            72
     O                       prCode              85
     O*
     O**   Detail Records fpr displaying total count-
     O          E            DTL2           1
     O                       intcntto            37
     O                       intcntAc            37
     O                       intcntRe            37
     O          E            PAGLN1           63
     O                       PAGE          Z     78
     O                                           75 'PAGE:'
