     Hcopyright('(c) 2012 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H bnddir('WTBNDDIR')
    H*********************************************************************
    H/title Agent Activity Report - Quote Types
    H*********************************************************************
    H*                                                                   *
    H*  CALLED FROM: W7870R1                                             *
    H*                                                                   *
    H*  CALLS TO: None                                                   *
    H*                                                                   *
    H*  Created:   10/20/03      C:33      Project: WC1017               *
    H*********************************************************************
    H*  REVSIONS:                                                        *
     H*                                                                   *
     H* 07/19/13  C:65  Project: Redmine #3414 (Luba)                     *
     H*                 Fix the loop over workfile for exclusions so it   *
     H*                 properly processes.                               *
     H*                                                                   *
     H* 11/06/12  C:33  Project: WC2115                                   *
     H*                 -Implement renewal and new types.                 *
    H*                                                                   *
    H*  04/22/08  C:33  Seq#: WA071/63055                                *
    H*                 -Close files after changing status.               *
    H*                 -Implement 'Change all status'.                   *
    H*********************************************************************
     Fw7870f02  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwxq2p     uf a e             disk    rename(xq201:dbRcdF)
     F***  Begin Add  ***  11/06/12  **************************************
     Fwxq2l     uf   e           k disk    rename(xq201:xq200)
     F***  End   Add  ***  11/06/12  **************************************
     Fwxq2l2    uf a e           k disk    rename(xq201:xq202)

     D ChkPQB          pr                  like(*in90)
     D Exclusions      pr                  like(*in90)

     D/copy sPrString

     D***  Begin Add  ***  11/06/12  **************************************
     DUpdRenewSts      pr             1n
     D inType                              like(Q2qTyp) const
     D inStatus                            like(Q2stat) const
     D***  End   Add  ***  11/06/12  **************************************

    D*====================================================================
    D*  /COPY required constants
    D*====================================================================
     D optnm           c                   'Agent Activity Report'
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@Mode          s              1
     D @@PQB           s              1
     D @@rrn           s              9  0
     D flgQteTypes     s              1
     D***  Begin Add  ***  11/06/12  **************************************
     D***  End   Add  ***  11/06/12  **************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*====================================================================
    C*  PLIST definitions
    C*====================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@PQB
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*
    C*====================================================================
    C*  KLIST definitions
    C*====================================================================
    C*
     C*
    C*================================================================
    C* CHGSTS - Update file with new status
    C*================================================================
     C     Chgsts        Begsr
     C*
     C     @@rrn         Chain     dbRcdF
     C*
     C                   if        %found
     C                   select
     C                   when      Q2STAT = 'I'
     C                   eval      Q2STAT = 'E'
     C                   when      Q2STAT = 'E'
     C                   eval      Q2STAT = 'I'
     C                   endsl
     C*
     C                   update    dbRcdF
     C*
     C                   endif
     C*
     C                   endsr
     C*
    C*====================================================================
    C* CHGSTSALL - Update file with new status for all records
    C*====================================================================
     C*
     C     ChgStsAll     begsr
     C*
     C     1             setll     dbrcdf
     C                   read      dbrcdf                                 99
     C*
     C     *in99         doweq     *off
     C*
     C                   select
     C                   when      Q2STAT = 'I'
     C                   eval      Q2STAT = 'E'
     C                   when      Q2STAT = 'E'
     C                   eval      Q2STAT = 'I'
     C                   endsl
     C*
     C                   update    dbrcdf
     C*
     C                   read      dbrcdf                                 99
     C*
     C                   enddo                                                  *in99=*off
     C*
     C                   endsr                                                  chgsts
    C*
    C*====================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*====================================================================
    C*
     C     dltpre        begsr
     C                   endsr
    C*
    C*====================================================================
    C* DSPLYR - Processing after displaying information
    C*====================================================================
    C*
     C     dsplyr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*====================================================================
    C*
     C     errck1        begsr
    C*
     C  n90              eval      *in90 = ChkPQB
    C*
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*====================================================================
    C*
     C     errck2        begsr
     C                   endsr
    C*
    C*====================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*====================================================================
    C*
     C     errck3        begsr
     C                   endsr
    C*
    C*====================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*====================================================================
    C*
     C     mvdbsc        begsr
     C*
     C                   eval      scPQB = 'N'
     C*
     C                   endsr
    C*
    C*====================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*====================================================================
    C*
     C     mvscdb        begsr
     C*
     C                   eval      @@PQB = scPQB
     C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHERR - Processing for other modes
    C*====================================================================
    C*
     C     otherr        begsr
    C*
     C                   select
     C                   when      @@Mode = 'X'
     C                   exsr      mvScDb
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* OTHFKS - Processing for other functions keys
    C*====================================================================
    C*
     C     othfks        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PGMEND - End of program processing
    C*====================================================================
    C*
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*
    C*====================================================================
    C* PGMSTR - Start of program processing
    C*====================================================================
    C*
     C     pgmstr        begsr
     C                   endsr
    C*
    C*====================================================================
    C* PREOTH - Pre-processing for other modes
    C*====================================================================
    C*
     C     preoth        begsr
    C*
     C                   select
     C                   when      @@Mode = '*'
     C                   exsr      ChgStsAll
     C                   eval      *inlr = *on
     C                   return
      /Free
       When @@mode = '#';
          flgQteTypes = @@pqb;
          Exsr UpdSts;
          *inlr = *on;
          Return;
      /End-free
     C                   when      @@Mode = 'S'
     C                   exsr      chgSts
     C                   eval      *inlr = *on
     C                   return
     C                   when      @@Mode = 'X'
     C                   if        Exclusions = *on
     C                   eval      @@PQB = 'N'
     C                   eval      *inlr = *on
     C                   return
     C                   endif
     C                   exsr      mvDbSc
     C                   endsl
    C*
     C                   endsr
    C*
    C*====================================================================
    C* UPDATR - Update data base record and after processing
    C*====================================================================
    C*
     C     updatr        begsr
     C                   endsr
      /Free

      //==================================================================
      // UpdSts - Update file with status for records based on type
      //==================================================================
       Begsr UpdSts;

       Setll 1 dbRcdF;
       Dou %eof(wxq2p);
          Read dbRcdF;
          If not %eof(wxq2p) and Q2qTyp = Upper(Q2qTyp);
             Select;
             When flgQteTypes = 'A';
                Unlock wxq2p;
                UpdRenewSts(Q2qTyp:Q2stat);
             When flgQteTypes = 'R';
                If UpdRenewSts(Q2qTyp:Q2stat);
                   Q2stat = 'E';
                   Update dbRcdF;
                Endif;
             When flgQteTypes = 'N';
                Unlock wxq2p;
                UpdRenewSts(Q2qTyp:'E');
             Other;
                Unlock wxq2p;
             Endsl;
          Endif;
       Enddo;

       Endsr;

      /End-free
    C*====================================================================
    C* WRITER - Write data base record and after processing
    C*====================================================================
    C*
     C     writer        begsr
     C                   endsr
    C*____________________________________________________________________
    C*





    C*====================================================================
    C* ChkPQB - Check the pending, quote, bound flag (scPQB)
    C*====================================================================
     PChkPQB           b

     D ChkPQB          pi                  like(*in90)

     C                   select
     C                   when          scPQB <> 'Y'
     C                             and scPQB <> 'N'
     C                   eval      *in79 = *on
     C                   return    *on
     C                   endsl

     C                   return    *off

     PChkPQB           e





    C*====================================================================
    C* Exclusions - Determine if any quote types have been excluded
    C*====================================================================
     PExclusions       b

     D Exclusions      pi                  like(*in90)

      ***  Begin Chg  ***  11/06/12  **************************************
     C******loval        setll     xq202
     C**********         read      xq202
     C**********         return    %found
     D dsWXQ2L         ds                  LikeRec(xq200)

      /Free
       Setll *loval xq202;
       Dou %eof(wxq2l2);
          Read(n) xq202;
       //*** begin chg ***  07/19/13  ******************************************
       //*****  If %eof(wxq2l2);
          If not %eof(wxq2l2);
       //*** begin chg ***  07/19/13  ******************************************
             Select;
             When Q2qTyp = Lower(Q2qTyp);
                Chain(n) (Upper(Q2qTyp)) xq200 dsWXQ2L;
             When Q2qTyp = Upper(Q2qTyp);
                Chain(n) (Lower(Q2qTyp)) xq200 dsWXQ2L;
             Endsl;
             Select;
             When %found and dsWXQ2L.Q2stat = 'E';
                Return *on;
             When not %found;
                Return *on;
             Endsl;
          Endif;
       Enddo;
       Return not %eof(wxq2l2);
      /End-Free
      ***  End   Chg  ***  11/06/12  **************************************

     PExclusions       e

     P***  Begin Add  ***  11/06/12  **************************************
      //==================================================================
      // Update the renewal code status
      //==================================================================
     PUpdRenewSts      b
     DUpdRenewSts      pi             1n
     D inType                              like(Q2qTyp) const
     D inStatus                            like(Q2stat) const

     D dsWXQ2L         ds                  LikeRec(xq200)

      /Free
       Chain (Lower(inType)) xq200 dsWXQ2L;
       If %Found;
          dsWXQ2L.Q2stat = inStatus;
          Update xq200 dsWXQ2L;
       Endif;

       Return %Found;
      /End-free

     PUpdRenewSts      e
     P***  End   Add  ***  11/06/12  **************************************
