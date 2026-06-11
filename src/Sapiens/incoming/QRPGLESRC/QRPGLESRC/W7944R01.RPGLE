     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
    H*********************************************************************
     H*/TITLE W7944R01 - Group Select - Populate Work File
    H*********************************************************************
    H*  Description: Populates WXGSP workfile for Renewal tracking.      *
    H*                                                                   *
    H*  Called from: W7944C                                              *
    H*                                                                   *
    H*  Calls to: WA001R                                                 *
    H*                                                                   *
    H*  Created: C:53   Project: 129WJW/WC1277  03/16/05                 *
    H**************************************************************************************
    H*  Revisions:                                                                        *
     H*
     H*  04/16/21  C:079  Support:99/95541
     H*                   During testing found issue in Group Select default
     H*                   that should only be for DARTS extract.
     h*                                                                                    *
     h*  12/23/19  C:180  Project:133IRJ                                                    *
     h*                   Set to include if flag WDF2P/F2DXTR is set to Y.                 *
     h*                                                                                    *
/*  1H*  12/19/19  C:079  Project: 150WEM                                                  *
     H*                   Default to Include in Policy Export Group Select
     h*                                                                                    *
/*  1H*  08/19/19  C:079  Project: WC2604                                                  *
/*   H*                   Add NCCI Group Carrier Code                                      *
     h*                                                                                    *
/*  1H*  04/04/18  C:062  Ticket: 70103(129)                                               *
/*   H*                   Fix group select conditions.                                     *
     h*                                                                                    *
/*  1H*  12/14/15  C:295  Project: WC2370                                                  *
/*   H*                   Single Group Call on Group Select Programs                       *
/*   H*                                                                                    *
     H*  09/11/15  C:295 PROJECT WC2362                                                    *
     H*                  1099 Process include Manual Entries                               *
     H*                  To populate Trasaction Id                                         *
     H*                                                                                    *
     H* 06/23/15 C:33  35519
     H*               -Add Carrier#
     H*                                                                                    *
     H*  04/11/13  C:282 PROJECT WC2185                                                    *
     H*                  Group Restrictions by User to Control Group                       *
     H*                  Selection for Reports and Processes                               *
     H*                                                                                    *
     H*  10/06/10 C:198 Support: 74432                                                     *
     H*                 Corrected chain on WTGSP.                                          *
     H*                                                                                    *
     H*  08/16/10 C:33  Project: SY190  Priss #77                                          *
     H*                 When the default Inc/Exc flag is blank, set it to include.         *
     H*                                                                                    *
     H*  07/26/10 C:136 Project: WC1694                                                    *
     H*                 Enhance pgm to be table driven as opposed to hardcoding exceptions *
     H*                 !!!!!! Rerun LODWTGSP prior to implementing this enhancement !!!!!!*
     H*                                                                                    *
     H*  05/03/10 C:136 Project: WC1694 PRISS                                              *
     H*                 Include groups in WXGSP that have the Policy Export set up         *
     H*                                                                                    *
     H*  06/12/09  C:65  Project: WC1819                                                   *
     H*                  Populate GSRRE# from the group auxiliary file for medicare.       *
    H*                                                                                    *
    H**************************************************************************************
    F*
    F*  Physical file definition (for refreshing subfile record)
     Fwxgsp     uf a e             disk
    F*
    F*  Fund master file
     Fwmfnl     if   e           k disk
     Fwdf2l     if   e           k disk
     fwtgsp     if   e           k disk    rename(pgs01 : pgs0t)
     fwmpel     if   e           k disk
     d sys_date        s               d   datfmt(*usa)
     d sys_date7       s              7  0
     d charUser        s             10
     d charAppl        s              1
     d charRtcdUsr     s              1
     d charRtcdFnd     s              1
     d charAuth        s              1
     d charGrp         s              3  0
     d psds          esds                  extname(szq1p)
     d                 ds
     d dsdttm                        14  0
     d  dstime                        6  0 overlay(dsdttm)
     d  dsdate                        8  0 overlay(dsdttm:7)
     d****** Beg Add ***** 12/14/15 ****************************************
     d W7944SNG        s              6    DTAARA(W7944SNG)
     d W7944SDA        ds             6    dtaara(W7944SNG)
     d FromFund                1      3
     d ToFund                  4      6
     d isSingle        s              1    inz(*off)
     d intFromFund     s                   Like(FNfnd)
     d intToFund       s                   Like(FNfnd)

     d outName         s             10    inz
     d outType         s              7    inz
     d outRtcd         s              1    inz

      * Check Object
     dGetObject        pr                  extpgm('SRCHKOBJ')
     d outName                             like(outName)
     d outType                             like(outType)
     d outRtcd                             like(outRtcd)

     d****** End Add ***** 12/14/15 ****************************************
     F*
     D @@pgm           s             10
     D kkpgm           s             10
     c*
     d RtvUsrAuth      pr                  Extpgm('SA008R')
     d ##co#                          3  0 const
     d charUser                      10    const
     d charAppl                       1    const
     d charRtcdUsr                    1
      *
     d RtvGrpAuth      pr                  Extpgm('SA008R')
     d ##co#                          3  0 const
     d charUser                      10    const
     d charAppl                       1    const
     d charRtcdFnd                    1
     d charAuth                       1          options(*nopass)
     d charGrp                        3  0 const options(*nopass)
     c* Key list for Group Auxiliary
     c*
     c     f2ky          klist
     c                   kfld                    fnco#
     c                   kfld                    fnfnd

     c     gsky          klist
     c                   kfld                    kkpgm
     c*
     c* Entry key list
     c*
     C     *entry        plist
     C                   parm                    @@pgm
     C*
     C                   call      'WA001R'
     C                   parm                    ##co#             3 0
     C                   parm                    ##fnd             3 0
     C                   parm                    ##conm           40
     C                   parm                    ##fnam           40

     c                   time                    dsdttm
     c     *usa          move      dsdate        sys_date
     c     *cymd         move      sys_date      sys_date7
      /Free
        Exsr RtvUsrAuthority;
      /End-Free
      /Free
       //***** Beg Add ***** 12/14/15 **********************************
          outName = 'W7944SNG';
          outType = '*DTAARA';
          outRtcd = ' ';
          GetObject(outName:outType:outRtcd);
          If outRtcd <> '1';
             Exsr RtvGroup;
          Endif;
       //***** End Add ***** 12/14/15 **********************************
      /End-Free
     C*
     C     ##co#         chain     pfn01                              99
     C                   dow       *in99=*off
     C
     C                   eval      gsco# = fnco#
     C                   eval      gsfnd = fnfnd
     C                   eval      gsname = fnname

     c                   eval      kkpgm = @@pgm
      /Free
       //***** Beg Add ***** 12/14/15 **********************************
         If isSingle =  *on;
            Select;
            When ToFund <> '000';
               //*** Begin Chg *** 04/04/18 ****************************
               //*** Select;
               //*** When FNfnd >= intFromFund;
               //*** GSiEFl = 'I';
               //*** When FNfnd <= intToFund;
               //*** GSiEFl = 'I';
               //*** Other;
               //*** GSiEFl = 'E';
               //*** Endsl;
               If FNfnd >= intFromFund And FNfnd <= intToFund;
                  GSiEFl = 'I';
               Else;
                  GSiEFl = 'E';
               EndIf;
               //*** End   Chg *** 04/04/18 ****************************
            When FromFund <> '000';
               GSiEFl = 'E';
               If FNfnd = intFromFund;
                  GSiEFl = 'I';
               Endif;
            Other;
               GSiEFl = GSdIEF;
            Endsl;
         Else;
       //***** End Add ***** 12/14/15 **********************************
      /End-Free
     c     kkpgm         chain     pgs0t
     c                   if        %found
     c                   eval      GSIEFL = GSDIEF
     C                   if        GSIEFL = *blank
     C                   eval      GSIEFL = 'I'
     C                   endif
     c                   if        GSCOPE = 'Y'
     c     f2ky          chain     ppe01
     c                   select
     c                   when      %found and
     c                             (peefdt = 0 or peefdt <= sys_date7) and
     c                             (peexdt = 0 or peexdt >= sys_date7)
       //Beg Add *** 12/19/19 **********************************************
      /Free
                         If PEdftGrpSl <> *blank;
                            GSiEfl = PEdftGrpSl;
                         Endif;
      /End-Free
       //End Add *** 12/19/19 **********************************************
     c                   other
     C     ##co#         reade     pfn01                                  99
     c                   iter
     c                   endsl
     c                   endif                                                  GSCOPE = 'Y'
     c***  Begin Del  ***  06/23/15  **************************************
     c**********         if        GSCRRE = 'Y'
     c***  End   Del  ***  06/23/15  **************************************
     c***  Begin Add  ***  06/23/15  **************************************
     C                   eval      GSncci = '00000'
     c***  End   Add  ***  06/23/15  **************************************
     c***  Begin Add  ***  08/19/19  **************************************
     C                   eval      GSnCarGpCd = '00000'
     c***  End   Add  ***  08/19/19  **************************************
     c     f2ky          chain     pf201
     c                   if        %found(WDF2L)
     c                   eval      gsrre# = f2rre#
     c***  Begin Add  ***  06/23/15  **************************************
     C                   eval      GSncci = %EditC(F2car#:'X')
     c***  End   Add  ***  06/23/15  **************************************
     c***  Begin Add  ***  08/19/19  **************************************
     C                   eval      GSnCarGpCd = %EditC(F2nCarGrp:'X')
     c***  End   Add  ***  08/19/19  **************************************
     c***  Begin Add  ***  12/23/19  **************************************
      /Free
       //*** Beg Chg *** 10/28/20 *****************************************
       If @@PGM='SCEXTR' and F2DxTr = 'Y';
       //***If F2DxTr = 'Y';
       //*** End Chg *** 10/28/20 *****************************************
          GSIEFL = 'I';
       Endif;
      /End-Free
     c***  End   Add  ***  12/23/19  **************************************
     c                   endif                                                  %found(WDF2L)
     c***  Begin Del  ***  06/23/15  **************************************
     c**********         endif                                                  GSCRRE = 'Y'
     c***  End   Del  ***  06/23/15  **************************************
     c                   else
     c                   eval      GSIEFL = 'I'
     c                   endif                                                  WTGSP = %found
     C
      ********* Beg Add ********** 09/11/15 *****************************
      /Free
        If GSreqTcc = 'Y';
           Chain (fnco#:fnfnd) pf201;
           If %found(Wdf2l);
              GSTcc = F2Tcc;
              If GSreqTcc = 'Y' and GSTcc = *blanks;
              GSieFl = 'E';
              Endif;
           Endif;
        EndIf;
      /End-Free
      ********* End Add ********** 09/11/15 *****************************

      //**** Beg Add **** 12/14/15 ***********************************************
      /Free
          EndIf;
      /End-Free
      //**** End Add **** 12/14/15 ***********************************************

      /Free
          charRtcdFnd = *Off;
          If charRtcdUsr = *On;
          ExSr RtvGrpAuthority;
          EndIf;
          If charRtcdFnd = *Off;
      /End-Free

     C                   write     pgs01
      /Free
          EndIf;
      /End-Free
     C*
     C     ##co#         reade     pfn01                                  99
     C                   enddo
     C*
     C                   eval      *inlr=*on
      *================================================================
      * RtvUsrAuthority - Retrieve User Authority
      *================================================================
      /Free

        BegSr RtvUsrAuthority;
        charUser = Q1user;
        charAppl = 'W';
        charRtcdUsr = *Off;
        RtvUSrAuth(##co#:charUser:charAppl:charRtcdUsr);

        EndSr;
      /End-Free
      *================================================================
      * RtvGrpAuthority - Retrieve Group Authority
      *================================================================
      /Free

        BegSr RtvGrpAuthority;
        charUser = Q1user;
        charAppl = 'W';
        charRtcdFnd = *Off;
        charGrp  = FNfnd;
        RtvGrpAuth(##co#:charUser:charAppl:charRtcdFnd:charAuth:charGrp);

        EndSr;
      /End-Free
      ******** Beg Add ***** 12/14/15 **********************************
      *================================================================
      * RtvGroup - Retrieve single/range group(s) from data area
      *================================================================
      /Free

          BegSr RtvGroup;
         //Read from W7944SNG data area
             In *lock W7944SNG;

             W7944SDA  = W7944SNG;
             If FromFund  <> '000' or FromFund <> *Blanks;
               isSingle = *on;
                intFromFund = %int(FromFund);
             Endif;
             If ToFund  <> '000' or ToFund <> *Blanks;
                intToFund = %int(ToFund);
             Endif;
             unlock W7944SNG;
          EndSr;
      /End-Free
      ******** End Add ***** 12/14/15 **********************************
