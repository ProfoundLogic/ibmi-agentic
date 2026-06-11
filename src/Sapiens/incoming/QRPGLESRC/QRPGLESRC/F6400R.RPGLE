     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     F*
     F/TITLE F6400R - Import Medicare Zip9 Prompt
     F*******************************************************************
     F*                                                                 *
     F*  Called From:                                                   *
     F*  Calls To   :  F6400C                                           *
     F*                                                                 *
     F*******************************************************************
     F* Created on 09/18/12  by C:284 for Project TX079                 *
     F*******************************************************************
      * Revisions:                                                      *
      *                                                                 *
      *    01/29/14 C:291 Redmine: 3807                                 *
      *                   Add profound handler.                         *
      *                                                                 *
     F*******************************************************************
     FF6400F    CF   E             WORKSTN
     F********** begin Add **** 01/29/14 ****************
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F********** End Add **** 01/29/14 ****************
     F                                     INFDS(FIDS01)
     F*
     D F6400C          PR                  ExtPgm('F6400C')
     D ##co#                          3  0
      *
     D FA007R          PR                  ExtPgm('FA007R')
     D*
     D ##co#                          3  0
     D ##fnd                          3  0
     D ##conm                        40
     D ##fnam                        40
     D*
     D F6400R          PR                  ExtPgm('F6400R')
     D F6400R          PI
      *
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D FIDS01        E DS                  EXTNAME(SZQ2P)
      *
     D ##co#           S              3  0 Inz(*zeros)                          *Company Number
     D ##fnd           S              3  0 Inz(*zeros)                          *Group Number
     D ##conm          S             40    Inz(*blanks)                         *Company Name
     D ##fnam          S             40    Inz(*blanks)                         *Fund Name

      /Free

       Exsr Pgmstr;

          Exfmt Wdw01;

          If *Inkc = *On;
             Exsr PgmEnd;
          EndIf;

          If *In11 = *Off;
             F6400C (##co#);
          EndIf;
       Exsr Pgmend;

      /End-Free
     C*****************************************************************
     C* ERRCK - Error Check Routine
     C*****************************************************************
      /Free
       Begsr Errck;

       EndSr;
      /End-Free
     C*****************************************************************
     C* PGMEND - Program End Routine
     C*****************************************************************
      /Free
       BegSr PgmEnd;

       *Inlr = *On;
       return;

       EndSr;
      /End-Free
     C*****************************************************************
     C* PGMSTR - Program Initialization
     C*****************************************************************
      /Free
       BegSr PgmStr;

          Scpgm  = Q1pgm;
          Scuser = Q1user;
          Fa007r (##co# : ##fnd : ##conm : ##fnam);
          Scconm = ##conm;

       EndSr;
      /End-Free
