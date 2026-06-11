     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H*****************************************************************
     H/TITLE F6217R - 997 Functional Ack Processing                   *
     H*****************************************************************
     H*  CREATED BY                                                   *
     H*                                                               *
     H*  03/25/14  C:269 Project: MC376                               *
     H*  Program reads all the Arc 997 files and write error records  *
     H*  in 997 Ack Error File FEAE997P.                              *
     H*                                                               *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H*****************************************************************
     FFE997P    if   e             disk
     FFE997ISAP if   e             disk
     F                                     RENAME(P99701:P99ISA)
     FFE997GSP  if   e             disk
     F                                     RENAME(P99701:P99GS)
     FFE997STP  if   e             disk
     F                                     RENAME(P99701:P99ST)
     FFE997AK1P if   e             disk
     F                                     RENAME(P99701:P99AK1)
     FFE997AK2P if   e             disk
     F                                     RENAME(P99701:P99AK2)
     FFE997AK3P if   e             disk
     F                                     RENAME(P99701:P99AK3)
     FFE997AK4P if   e             disk
     F                                     RENAME(P99701:P99AK4)
     FFE997AK5P if   e             disk
     F                                     RENAME(P99701:P99AK5)
     FFE997AK9P if   e             disk
     F                                     RENAME(P99701:P99AK9)
     FFE997SEP  if   e             disk
     F                                     RENAME(P99701:P99SE)
     FFE997GEP  if   e             disk
     F                                     RENAME(P99701:P99GE)
     FFE997IEAP if   e             disk
     F                                     RENAME(P99701:P99IEA)
     FFEAE997P  uf a e             disk
     F                                     RENAME(P99701:P99EAE)
     FFTER997P  if a e             disk
     F                                     RENAME(P99701:P99TER)
     FFTER997L1 if a e             disk
     F                                     RENAME(P99701:P99TEL1)
     FFEMFL2    uf a e           k disk
     F*
     F* Define printer file -
     FQSYSPRT   O    F  150        PRINTER OFLIND(*INOF)
     D*
     D psds          esds                  extname(szq1p)
     D*
     D xxLine          S              3  0 inz(0)
     D xxCo#           S              3  0
     D xxJSt           S              2A
     D xxTSet          S              3A
     D xxBch#          S             15A
     D*
     D LoopCnt         S              9  0
     D flgStatus       S              1A   inz('0')
     D @@Co#           S              3
     D*
     D*Report variables
     D prAckCd         S              1A
     D prRejDec        S             50A
     D*
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
     *
     C*
     C     *entry        plist
     C                   parm                    @@CO#
     C                   parm                    xxJSt
     C                   parm                    xxTSet

     C     dtcymd        plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     C
     C     keyFEMFL2     Klist
     C                   kfld                    xxCo#
     C                   kfld                    xxJSt
     C                   kfld                    xxTSet
     C                   kfld                    xxBch#
     C*
     C     *LIKE         DEFINE    PAGE          xxpage
     C     *LIKE         DEFINE    AK201         prTxId                         Tran. Set ID
     C     *LIKE         DEFINE    AE997tsTid    prTxCnNo                       Tran. Set Cnt. No.
     C     *LIKE         DEFINE    AE997sgId     prSegId                        Seg. ID
     C     *LIKE         DEFINE    AE997dt       prTxTrDt                       Tran. Set Trss. Dt.
     C     *LIKE         DEFINE    AE997dt       prTxRvDt                       Tran. Set Recv. Dt.
     C***** COPY BOOK *******************************************
     C*****/COPY SCOPYWW01C
     C***********************************************************
     C* Main program starts here -
     C***********************************************************
      /Free
       Exsr MainSr;
       *inlr = *on ;

       Begsr MainSr ;

          //Clear report-detail line variables.
          Exsr ClrDtlRec;

          xxCo# = %dec(@@CO#:3:0);
          LoopCnt = 0;
          xxLine  = 1;
            Except Route;
            Except Head1;

          // Read flat file and process each record.
          Read P99701 ;
          Dow not %eof(FE997P) ;

          Select ;

          When %subst(data997:1:3) = 'ISA' ;

            Read P99ISA;
            //Do nothing

          When %subst(data997:1:2) = 'GS' ;

            Read p99gs;
            //Do nothing

          When %subst(data997:1:2) = 'ST' ;

            Read P99ST;
            //Do nothing

          When %subst(data997:1:3) = 'AK1' ;

            Read P99AK1;
            //Do nothing

          When %subst(data997:1:3) = 'AK2' ;

             dow (%subst(data997:1:3) = 'AK2') ;
             //for LoopCnt = 1 to 999999 ;

             read P99AK2 ;
             //Get Co# from company API

             xxBch# = ak202 ;

             //Chain out to femfl2
             Chain keyfemfl2 pmf01;
                if not %found(femfl2);

                //If record not present for transaction id,
                //write it in error file saying Batch control
                //number does not exist.

                   //Status flag is set to rejected.
                   flgStatus = '1';

                   //Move fields and write;
                   evalr AE997tsTid  = AK202;
                   AE997idCd   = AK201;
                   AE997sgId   = AK2SEG;
                   AE997pgm    = q1pgm;
                   AE997user   = q1user;

                   AE997dt     = %Dec(%Date():*usa);
                   AE997time   = %Dec(%Time():*hms);

                   AE997iPgm   = q1pgm;
                   AE997iUsr   = q1user;
                   AE997iDt    = %Dec(%Date():*usa);
                   AE997iTim   = %Dec(%Time():*hms);

                   Write P99EAE;                                     // Write error file FEAE997P.

                   //Writing record in detail summary report.
                   Exsr ClrDtlRec;
                   prTxId   = AK201;
                   prTxCnNo = AK202;
                   prSegId  = AK2SEG;
                   prTxTrDt = %Dec(%Date():*usa);
                   prTxRvDt = %Dec(%Date():*usa);
                   prAckCd  = 'R';
                   prRejDec = 'Batch control number does not exist';

                     if xxLine > 52;
                     *INOF = *on;
                     xxLine = xxLine + 2;
                     except Route;
                     //except PAGLN1;
                     except head1;
                     *INOF = *off;
                     endif;

                   except dtl1;
                   xxLine = xxLine + 1;

                else;                                            //if found in FEMFL2.

                read P99701 ;                                    //read next record.
                dow (%subst(data997:1:3) = 'AK3' or
                %subst(data997:1:3) = 'AK4');
                //Read FE997AK3P and FE997AK4P
                //Write error record.

                If %subst(data997:1:3) = 'AK3';
                Read P99AK3;
                   //Move fields and write ;
                   evalr AE997tsTid  = AK202 ;
                   AE997sgId   = AK3SEG ;
                   AE997idCd   = AK301 ;
                   AE997ERCD   = AK304 ;
                   AE997pgm    = q1pgm ;
                   AE997user   = q1user ;

                   AE997dt     = %Dec(%Date():*usa);
                   AE997time   = %Dec(%Time():*hms);

                   AE997iPgm   = q1pgm ;
                   AE997iUsr   = q1user ;
                   AE997iDt    = %Dec(%Date():*usa);
                   AE997iTim   = %Dec(%Time():*hms);
                   Write P99EAE ;                                        // Write error file.

                elseif %subst(data997:1:3) = 'AK4' ;
                Read P99AK4 ;
                   // Move fields and write ;
                   evalr AE997tsTid  = AK202 ;
                   AE997sgId   = AK4SEG ;
                   AE997idCd   = AK301 ;
                   AE997ERCD   = AK403 ;
                   AE997POS    = AK401 ;
                   AE997REF    = AK402 ;
                   AE997DATA   = AK404 ;
                   AE997pgm    = q1pgm ;
                   AE997user   = q1user ;

                   AE997dt     = %Dec(%Date():*usa);
                   AE997time   = %Dec(%Time():*hms);

                   AE997iPgm   = q1pgm ;
                   AE997iUsr   = q1user ;
                   AE997iDt    = %Dec(%Date():*usa);
                   AE997iTim   = %Dec(%Time():*hms);
                   Write P99EAE ;                            // Write error record in file FEAE997P.

                endif ;
                Read P99701 ;
                enddo ;
                endif ;


             if %subst(data997:1:3) = 'AK5' ;
             if flgStatus = '0' ;

                MF997p = 'A' ;
                MF997ar = %Dec(%Date():*usa);

                update PMF01 ;

             else ;
                //Update this rejected record in file FEMFL2.
                MF997p = 'R' ;
                MF997ar = %Dec(%Date():*usa);
                MFtSet  = AK201 ;
                MFbch#  = AK202 ;
                MFltDt  = %Dec(%Date():*usa);
                MFstat  = 'R' ;

                Write PMF01 ;

                //Update this rejected record in file FEAE997P.
                // Move fields and write ;
                evalr AE997tsTid  = AK5SEG ;
                AE997sgId   = AK4SEG ;
                AE997idCd   = AK301 ;
                AE997ERCD   = AK403 ;
                AE997pgm    = q1pgm ;
                AE997user   = q1user ;

                AE997dt     = %Dec(%Date():*usa);
                AE997time   = %Dec(%Time():*hms);

                AE997iPgm   = q1pgm ;
                AE997iUsr   = q1user ;
                AE997iDt    = %Dec(%Date():*usa);
                AE997iTim   = %Dec(%Time():*hms);

                Write P99EAE ;                               // Write error record in file FEAE997P.

                //Writing record in detail summary report.
                Exsr ClrDtlRec ;
                prTxId   = AK201 ;
                prTxCnNo = AK202 ;
                prSegId  = AK5SEG ;
                prTxTrDt =  %Dec(%Date():*usa);
                prTxRvDt = %Dec(%Date():*usa);
                prAckCd  = 'A' ;
                prRejDec = ER997DSC ;

                if xxLine > 52 ;
                *INof = *on ;
                xxLine = xxLine + 2 ;
                except head1 ;
                except Route;
                *inOf = *off ;
                endif ;

                except dtl1 ;
                xxLine = xxLine + 1 ;

             endif ;
             endif ;
             //Read P99701 ;

             enddo ;                                                //end for loop AK2


          When %subst(data997:1:3) = 'AK9' ;
             read P99AK9 ;

             //write on report summary on the basis
             //of accepted and rejected.
             if flgStatus = '0' ;
                Exsr ClrDtlRec ;
                  prTxId   = ST01;
                  prTxCnNo = ST02;
                  prSegId  = AK9SEG ;
                  prTxTrDt = %Dec(%Date():*usa);
                  prTxRvDt = %Dec(%Date():*usa);
                  prAckCd  = 'A' ;
                  //prRejDec = ER997DSC ;

                  if xxLine > 52 ;
                  *INOF = *on ;
                  xxLine = xxLine + 2 ;
                  except Route;
                  except Head1 ;
                  *inOf = *off ;
                  endif ;

                except dtl1 ;
                xxLine = xxLine + 1 ;

             else ;

                Exsr ClrDtlRec ;
                prTxId   = ST01;
                prTxCnNo = ST02;
                prSegId  = AK9SEG ;
                prTxTrDt = %Dec(%Date():*usa);
                prTxRvDt = %Dec(%Date():*usa);
                prAckCd  = 'R' ;
                prRejDec = ER997DSC ;

                if xxLine > 52 ;
                *inOf = *on ;
                xxLine = xxLine + 2 ;
                //except PAGLN1 ;
                except Route;
                except head1 ;
                *InOf = *off ;
                endif ;

                except dtl1 ;
                xxLine = xxLine + 1 ;

             endif ;

          When %subst(data997:1:2) = 'SE' ;
             read P99SE;
             //write on report summary on the basis
             //of accepted and rejected.

          When %subst(data997:1:2) = 'GE' ;
             read P99GE;

          When %subst(data997:1:3) = 'IEA' ;
             read P99IEA;

          Other;

          endsl ;

          Read P99701 ;
          enddo ;
       Endsr ;

       //Subroutine to clear report-detail line variables.
       Begsr ClrDtlRec;
       clear prTxId ;
       clear prTxCnNo;
       clear prSegId;
       clear prTxTrDt;
       clear prTxRvDt;
       clear prAckCd;
       clear prRejDec;
       EndSr ;

      /End-Free
     C
     OQSYSPRT   E            ROUTE          1 06
     O                                           19 'REPORT TITLE:'
     O*                      ORTITL              80
     O                                           35 'EDI A49 EXTRACT'
     O                                           58 'ERROR REJECTION REPORT'
     O                       PAGE          Z     78
     O                                           75 'PAGE:'
     O          E            ROUTE          1
     O                                           19 'PROGRAM NAME:'
     O*                      ORPNAM              30
     O                                           26 'F6217R'
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
     O                                           21 'Transaction Set'
     O                                           39 'Transaction Set'
     O                                           51 'Segment ID'
     O                                           69 'Transaction set'
     O                                           89 'Transaction set'
     O                                           96 'Ack.'
     O                                         +003 'Rejection'
     O                                         +001 'Description'
     O          E            Head1          1
     O                                           16 'Identifier'
     O                                           38 'Control Number'
     O                                           71 'Transmission date'
     O                                           87 'Received Date'
     O                                           96 'Code'
     O**   Detail Records -
     O          E            DTL1           1
     O                       prTxId              14
     O                       prTxCnNo            39
     O                       prSegId             51
     O                       prTxTrDt            69 '  /  /    '
     O                       prTxRvDt            89 '  /  /    '
     O                       prAckCd             95
     O                       prRejDec           145
     O*         E            PAGLN1           63
