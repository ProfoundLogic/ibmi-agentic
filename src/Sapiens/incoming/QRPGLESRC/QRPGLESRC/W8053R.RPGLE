      *** Begin add *** 02/20/17 **************************************
     H BNDDIR('WTBNDDIR')
      *** End   add *** 02/20/17 **************************************
      ***************************************************************
      * (C) Copyright Progressive Data Solutions, Inc., 2005       **
      *                                                            **
      * Program:     W8053R                                        **
      * Description: Create EDI A49 Annual Report                  **
      *                                                            **
      * Created By:  C:167  Proj:WC1405   Date:  12/30/05          **
      *****************************************************************
      * REVISIONS:                                                    *
      *                                                               *
      * 02/20/17 C:306 Project WC2454                                 *
      *                Added code  for 'TN' Tennessee state to select *
      *                claims to have an 'AN' log record              *
      *                                                               *
      * 06/26/06 C:15  Project 132WEC                                 *
      *                Added code to allow this program to be used to *
      *                create 'UR' transactions.                      *
      *****************************************************************
      * INDICATOR USAGE:                                           **
      *                                                            **
      ***************************************************************
      *
     Fwmcml13   if   e           k disk
     Fwdpal5    if   e           k disk
     Fwmlgl     uf a e           k disk
      *
     D  @@co#          s              3
     D  @@state        s              2
     D  @@year         s              4
     D  @@dudt         s              8
     D  @@mntcde       s              2
     D  srdti          s              8  0
     D  srdto          s             10
     D  srerr          s              1
     D  srffmt         s              8
     D  srtfmt         s              8
     D  xxco#          s              3  0
     D  xxdate         s              7  0
     D  xxdudt         s              8  0
     D  xxdue          s              7  0
     D  xxyear         s              4  0
      *
      *** Begin add *** 02/20/17 **************************************
     D  strdate        s              8a   inz
     D  intdate7       s              7  0 inz
     D  intdate8       s              8  0 inz
     D  intpastDt      s              8  0 inz
     D  intnumdays     s              5  0 inz
      *** End   add *** 02/20/17 **************************************
     D                 ds
     D  dsdttm                 1     14  0
     D  dstime                 1      6  0
     D  dsdate                 7     14  0
     D padate          ds
     D  padtmm                 1      2  0
     D  padtdd                 3      4  0
     D  padtyr                 5      8  0
      *
     D FIDS01        E DS                  EXTNAME(SZQ2P)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
      *** Begin add *** 02/20/17 **************************************
     d/copy sPrDate
      *** End add *** 02/20/17 **************************************
      *
      *  entry parm list
      *
     C     *entry        plist
     C                   parm                    @@co#
     C                   parm                    @@state
     C                   parm                    @@year
     C                   parm                    @@dudt
     C                   parm                    @@mntcde
      *
      *** Begin Chg *** 02/20/17 **************************************
     C***  dtcymd        plist
     C***                parm                    srdti
     C***                parm      *blanks       srffmt
     C***                parm      '*CYMD'       srtfmt
     C***                parm                    srdto
     C***                parm                    srerr
      *
     C***  dtmdyy        plist
     C***                parm                    srdti
     C***                parm      '*CYMD'       srffmt
     C***                parm      *blanks       srtfmt
     C***                parm                    srdto
     C***                parm                    srerr
     c*
     C     dtcymd2       plist
     C                   parm                    srdti
     C                   parm      *blanks       srffmt
     C                   parm      '*CYMD'       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     c
     C     dtmdyy1       plist
     C                   parm                    srdti
     C                   parm      '*CYMD'       srffmt
     C                   parm      *blanks       srtfmt
     C                   parm                    srdto
     C                   parm                    srerr
     c
      *** End   Chg *** 02/20/17 **************************************
      *
      *  KEY LIST FOR THE CLAIM FILE
     C     cmky          klist
     C                   kfld                    xxco#
     C                   kfld                    @@state
      *
      *  KEY LIST FOR THE PAYMENT HISTORY FILE
     C     paky          klist
     C                   kfld                    cmco#
     C                   kfld                    cmfnd
     C                   kfld                    cmfyr
     C                   kfld                    cmcase
      *
      *  DEFINE WORK FIELDS
      *
     C                   move      @@co#         xxco#
     C                   move      @@year        xxyear
      *
     C                   time                    dsdttm
     C                   z-add     dsdate        srdti
      *** Begin Chg *** 02/20/17 **************************************
     C***                call      'SRCVTDAT'    dtcymd
     C                   call      'SRCVTDAT'    dtcymd2
      *** End   Chg *** 02/20/17 **************************************
     C                   movel     srdto         xxdate
      *
     C                   move      @@dudt        xxdudt
     C                   z-add     xxdudt        srdti
      *** Begin Chg *** 02/20/17 **************************************
     C***                call      'SRCVTDAT'    dtcymd
     C                   call      'SRCVTDAT'    dtcymd2
      *** End   Chg *** 02/20/17 **************************************
     C                   movel     srdto         xxdue
      *
      *  MAIN LOOP
      *
      * read claims
     C     cmky          setll     pcm01
     C                   dow       *in91 = *off
     C     cmky          reade     pcm01                                  91
     C                   if        *in91 = *off
      *
      *** Begin add *** 02/20/17 **************************************
      /Free
      //** If State is 'TN' Tennessee, create an 'AN' in log file.
       Select;
         When @@state = 'TN';
             If @@mntcde = 'AN';
                strdate    = '0630' + @@year;
                intdate8   = %dec(strdate:8:0);
                intnumdays = 365;
                intpastDt  = dtPast(intdate8:intnumdays);
                intdate7   = dtCYMD(intpastDt);

                If CMacDt < intdate7 and CMstat ='O';
                   Exsr $createWMLGP;
                EndIf;
             EndIf;
        //*** For other states. Process as it is.
         Other;
      /End-Free
      *** End   add *** 02/20/17 **************************************
     C                   eval      *in92 = *off
     C     paky          setll     ppa01
     C                   dow       *in92 = *off
     C     paky          reade     ppa01                                  92
     C                   if        *in92 = *off
      *
      *** begin add *** 06/26/06 **************************************
     C                   if        @@mntcde = 'AN'
      *** end add  **** 06/26/06 **************************************
      *
     C                   z-add     patrdt        srdti
      *** begin chg *** 02/20/17 **************************************
     C****               call      'SRCVTDAT'    dtmdyy
     C                   call      'SRCVTDAT'    dtmdyy1
      *** End   chg *** 02/20/17 **************************************
     C                   movel     srdto         padate
     C                   if        padtyr = xxyear
     C                   exsr      $createWMLGP
     C                   leave
     C                   endif
      *
      *** begin add *** 06/26/06 **************************************
     C                   endif
      *
     C                   if        @@mntcde = 'UR'
     C                   exsr      $createWMLGP
     C                   leave
     C                   endif
      *** end add  **** 06/26/06 **************************************
      *
     C                   endif
     C                   enddo
      *
      *** Begin add *** 02/20/17 **************************************
      /Free
       EndSl;
      /End-Free
      *** End   add *** 02/20/17 **************************************
     C                   endif
      *
     C                   enddo
      *
     C                   exsr      term
      *
      *
      *=========================================================================
      *  Write EDI log record
      *=========================================================================
     C     $createWMLGP  begsr
      *
     C                   clear                   plg01
     C                   eval      lgco# = CMCO#
     C                   eval      lgfnd = CMFND
     C                   eval      lgfyr = CMFYR
     C                   eval      lgcase = CMCASE
     C                   eval      lgtset = 'A49'
     C                   eval      lgmtc = @@mntcde
     C                   eval      lgjst = @@state
     C                   eval      lgdue = xxdue
     C                   eval      lgsts = 'EC'
     C                   eval      lgstat = 'A'
     C                   eval      lgmtcd = xxdate
     C                   eval      lgiusr = q1user
     C                   eval      lgidt = xxdate
     C                   eval      lgitm = dstime
     C                   eval      lgipgm = q1pgm
     C                   write     plg01
      *
     C                   endsr
      *=========================================================================
      *  END OF PROGRAM
      *=========================================================================
     C     term          begsr
      *
     C                   eval      *inlr = *on
     C                   return
      *
     C                   endsr
