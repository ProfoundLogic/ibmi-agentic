     HCopyright('(c) 2020 Sapiens International. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller) bnddir('WTBNDDIR')
     H/TITLE DTTFRRCPRC - iDarts Tranfer by Remark Code - Processor
     H DFTNAME(DTTFRRCPRC)
     H/DEFINE   PROFOUNDUI
      *---------------------------------------------------------------*
      *  Created   : 01/15/20                                         *
      *  Programmer: C:180                                            *
      *  Project   : 129WVT - Darts Remark Extract                    *
      *---------------------------------------------------------------*
      *  Description:  Darts Remark Extract Process.
      *
      *
      *---------------------------------------------------------------*
      * Revisions:
      *
     FWXGSL     IF   E           K DISK
     FSDCML#A   UF   E           K DISK
     FSMARL1    UF   E           K DISK
      * *Entry
      *
     D DTTFRRCPRC      PR
     D inFromRmk                      5A   Const
     D inToRmk                        5A   Const

     D DTTFRRCPRC      PI
     D inFromRmk                      5A   Const
     D inToRmk                        5A   Const

     D strCo#          S              3A
     D strFnd          S              3A

       Read PGS01;
       Dow not %Eof(WXGSL);
          If GSiefl = 'I';
             strCo# = %editc(GSCo#:'X');
             strFnd = %editc(GSfnd:'X');
             Setll (strCo#:strFnd:inFromRmk) PCM01;

             If %equal(SDCML#A);
                Reade (strCo#:strFnd:inFromRmk) PCM01;

                Dow not %eof(SDCML#A);
                   Exsr UpdSMARP;
                   CMRMCD = inToRmk;
                   Update PCM01;
                   Reade (strCo#:strFnd:inFromRmk) PCM01;
                Enddo;

             Endif;

          Endif;
          Read PGS01;
       Enddo;

       *inlr = *on;
      *------------------------------------------------------------------
      * Update File SMARP
      *------------------------------------------------------------------
       Begsr UpdSMARP;
       Setll (CMkey:CMseq#) PAR01;
       If %equal(SMARL1);
          Reade (CMkey:CMseq#) PAR01;
          Dow not %eof(SMARL1);
             If ARrmcd = inFromRmk;
                ARrmcd = inToRmk;
                Update PAR01;
             Endif;
             Reade (CMkey:CMseq#) PAR01;
          Enddo;
       Endif;
       Endsr;
