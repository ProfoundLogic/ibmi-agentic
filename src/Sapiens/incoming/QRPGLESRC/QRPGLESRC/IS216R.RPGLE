     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*Caller)
     H/TITLE IS216R  "REMOVE" DOC FROM QUEUE
      //==================================================================
      // This program soft-deletes records from IMIEP table, after
      // a "Are you sure ?" confirmation.
      //
      // All programs deleting entries from IMIEP should always
      // call this program.
      //
      //  REVISIONS:
      //  07/27/15   C:302  Support item 36260
      //                    Incorrect variable used for silent mode.
      //  10/08/14   C:302  Support item 84408
      //                    Not all places in the application where
      //                    we delete entries from IMIEP, are writting
      //                    the corresponding note.
      //                    Cleaned-up program, and add the note writting.
      //
      //  07/16/2014 c:62   Support IM114 86202
      //                    Prevent record lock
      //
      //     3/22/99 -      Always ask before removing the doc from the users
      //                     queue.
      //==================================================================

     FIS216F    CF   E             WORKSTN Handler('PROFOUNDUI(HANDLER)')
     F                                     INFDS(FIDS01)  USROPN
      // Index entries   Stat = 'A' or ' ', and Dsts = 'RL' - by Qid,DcId,Date,Time
     FIMIELF    UF   E           K DISK
      // Index entries - no select omit - by DcId
     FIMIEL5    IF   E           K DISK    RENAME(PIE01:PIE02)
      // Document transaction file - by DcId
     FIMDOL1    UF   E           K DISK

     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)

     D I1              s              5  0
     D ##RtCd          s              1
     D Held            s              1
     D noRjct          s              1
     D ##Cnt           S              5P 0
     D $$Date          S              8  0
     D $$Time          S              6  0
     D dn              s             75    dim(5)

     D RejectDoc       pr                  extPgm('IS157R1')
     D ##DcId                        12    const
     D ##Status                       2    const
     D ##Pgm                         10    const
     D ##RtCd                         1

     DWriteNotes       pr                  extPgm('ISDOCNR')
     D ##Comm75                      75    const
     D ##DcId                        12    const
     D ##CDat                         8    const
     D ##PgmName                     10    const
     D ##PrFl                         1    const
     D ##RtCd                         1

     D IS216R          PR
     D  #$Sdt                         8A
     D  #$Stm                         6A
     D  qqDcId                       12A
     D  qqQid                        10A
     D  qqPgm                        10A
     D  qqSilent                      1A
     D  qqRtcd                        1A

     D IS216R          PI
     D  #$Sdt                         8A
     D  #$Stm                         6A
     D  qqDcId                       12A
     D  qqQid                        10A
     D  qqPgm                        10A
     D  qqSilent                      1A
     D  qqRtcd                        1A
      *

      /Free

       // Return i for ie rcd, or d for both updated.... L is error (locked)
       qqRtcd = *Blanks;

       // Translate the incoming date and time to decimal
       $$Date = %Dec(%Char(%Date(#$Sdt:*usa0):*iso0):8:0);
       $$Time = %Dec(#$Stm:6:0);

       //  If it was not call in silent mode, then usr display file.
       If qqSilent <> 'Y';
          open  IS216F;
       Endif;

       //  Determine if other ie's exist besides the one we are working
       //  with

       ##Cnt = *zeros ;

       Setll qqDcId IMIEL5;

       Held = *off;
       Reade qqDcId IMIEL5;
       Dow Not %EOF And ##Cnt < 3;
          If IEdsts <> 'RA' And IEdsts <> 'DL';
             If IEdsts = 'HD';
                Held = *On;
             Endif;
             ##Cnt = ##Cnt + 1;
          Endif;
          Reade qqDcId IMIEL5;
       Enddo;

        //  If  no more than one ie record exists, we need to display
        //  a screen to "are you sure?" for the removal of the record,
        //  because, we will also "dl" the do record in this case.
        //  else, we will just do the remove...

       Chain(N) (qqQID:qqDCID:$$DATE:$$TIME) IMIELF;

       *in80 = *On;
       Dow *in80;

          If %Found;
             SCdcid = IEdcid;

             // If "held" exists (orig scan rcd) and ##cnt <=2 then we must
             // change the dsts on the hd rcd to be "rj" all

             //  *** Beg chg  *** 07/27/15 //
             // If ##Cnt <= 2 And Held = *On;
             If ##Cnt <= 2 And Held = *On and qqSilent <> 'Y';
             //  *** End chg  *** 07/27/15 //
                Exsr NODLT;

                If norjct = *On;
                   qqRtcd = *blanks;
                   Leave;
                Else;
                   RejectDoc(IEdcid:'RJ':qqPgm:qqRtcd);
                   //** Call is157r to process reject.....

                   If qqRtcd = 'A' Or qqRtcd = 'B';
                      qqRtcd = *blanks;
                      Leave;
                   Endif;

                Endif;

             Endif;

             // now valid return from reject, finish the remove *****
             // go ahead and remove the current rcd.
             //  *** Beg chg  *** 07/27/15 //
             // If (##Cnt <= 2 And Held = *On) or (Silent = 'Y');
             If (##Cnt <= 2 And Held = *On) or (qqSilent = 'Y');
             //  *** End chg  *** 07/27/15 //

             Else;
                Exsr SURE;

                If SUREFL = 'N';
                   qqRtcd = *Blanks;
                   Leave;
                Endif;
             EndIf;

             Chain (QQQID:QQDCID:$$DATE:$$TIME) IMIELF;
             If %Found;
                IEdsts = 'DL';
                IEldt = %dec(%Char(%Date():*ymd0):6:0);
                IEltim = %dec(%char(%Time():*HMS0):6:0);
                IElusr = q1User;
                IElpgm = qqPgm;
                Update PIE01;
             EndIf;

             qqRtcd = 'I';

             If ##Cnt <= 1;
                Chain qqdcid IMDOL1;
                If %Found;
                   DOdsts = 'DL';
                   DOldt = %dec(%Char(%Date():*ymd0):6:0);
                   DOltim = %dec(%char(%Time():*HMS0):6:0);
                   DOlusr = q1User;
                   DOlpgm = qqPgm;
                   Update PDO01;
                   qqRtcd = 'D';
                Endif;

             Endif;

             // Write notes for tracking
             Exsr UpdtNoteRmv;

          Else;

             qqRtcd = 'L';

          Endif;

       *In80 = *off;

       EndDo;
       *inLr = *On;

       //================================================================
       //   Sure subroutine
       //================================================================
       Begsr SURE;

          // Display the screen (window) that says "are you sure" you wish
          // to remove the document, no other copies of this document exist
          // on the system, and this document will no longer be accessible.
          //
          // Change the surefl to by y if the user selects y or n if they
          // select, n, and if f12, we do no updates, change surefl to n,
          // (will exit the program).

          //  *** Beg Add  *** 07/27/15 //
          If Not %open(IS216F);
             Open IS216F;
          Endif;
          //  *** End Add  *** 07/27/15 //
          Exfmt WDW01;
          If *InKl = *On;
             sureFl = 'N';
          Endif;

       Endsr;

       //================================================================
       //    Sure subroutine
       //================================================================
       Begsr NODLT;

          noRjct = *Off;
          //  *** Beg Add  *** 07/27/15 //
          If Not %open(IS216F);
             Open IS216F;
          Endif;
          //  *** End Add  *** 07/27/15 //
          Exfmt WDW02;
          If *inkl = *on;
             noRjct = *On;
          Endif;

       Endsr;

       //====================================================================
       // UpdtNoteRmv -  Update document notes with removal information
       //====================================================================
       Begsr UpdtNoteRmv;

          dn(1) = 'Date: ' + %Char(%Date():*mdy/) + ' '
                + 'Time: ' + %char(%Time():*usa) + ' '
                + 'REMOVED';
          dn(2) = 'Document ' + %trim(scDcId) + ' '
                + 'has been removed from Queue: ' + %trim(qqQid);
          dn(3) = '   and marked DELETED from the system.';
          dn(4) = 'By user: ' + %trim(Q1User) + '.';
          dn(5) = *blanks;
          For i1 = 1 to 5;
            Select;
            When i1 = 4 and qqRtCd <> 'D';
              ##RtCd = '1';
            When i1 = 5 and qqRtCd = 'D';
              ##RtCd = '1';
            When i1 = 3 and qqRtCd <> 'D';
              iter;
            Other;
              ##RtCd = *blank;
            Endsl;
            WriteNotes(dn(i1):qqDcId:%char(%Date():*iso0):qqPgm
                       :'H':##RtCd);
          Endfor;

       EndSr;

      /End-free
