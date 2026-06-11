     H Copyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER) bnddir('WTBNDDIR')
     H*****************************************************************
     H/TITLE W0010R6502 - Policy Page 3c Information
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL ALLOW A USER TO ENTER & CHA- *
     H*                NGE RECORDS IN FILE WDF4P.                     *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W001065R - WORK WITH POLICY PAGE 3C INFORMATION            *
     H*                                                               *
     H*  CALLS TO:                                                    *
     H*    NONE                                                       *
     H*                                                               *
     H*  CREATED: 12/31/10  PROGRAMMER: C:255 PROJECT: WC1918         *
     H*****************************************************************
     H* REVISIONS:                                                    *
     H*                                                               *
     H* 10/01/20 Project WC2757                                       *
     h*          Blanket waiver was removed as a verbiage type, but   *
     h*          logic was left in place to code other types.         *
     h*                                                               *
     H* 01/25/11 Project WC1918 (Pirss# 25):                          *
     H*                        1) Moved 'PP3C' value to left adjusted.*
     H*                                                               *
     H*****************************************************************
     Fw0010f6502cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Fwdf4p     uf a e             disk
     F                                     rename(pf401:dbrcdf)
     Fwdf4l     if   e           k disk
     Fwmfnp     if   e             disk
    D*================================================================
    D*  /COPY required constants
    D*================================================================
      *** Begin Change *** 09/29/20 ***********************************
     D*optnm           c                   'Work With Policy Page 3C'
     D optnm           c                   'Verbiage Entry'
      *** End   Change *** 09/29/20 ***********************************
     D comapi          c                   'WA001R'
     D #ofscr          c                   1
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##co#           s              3  0
     D ##conm          s             40
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##fnd#          s              3  0
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D ##key           s                   like(f4key)
     D ##efdt          s                   like(f4efdt)
     D ##stat          s                   like(f4stat)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
    C*
     C/COPY SCOPYMT01C
    C*
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2                         WMFNP
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
     C     key1          klist
     C                   kfld                    ##co#
     C                   kfld                    ##fnd#
     C                   kfld                    ##key
     C                   kfld                    ##efdt
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ZRRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
     C*Validate effective date
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    datchk
     C                   movel     srdto         scefdt
     C*
     C  n90              if        srerr = *on
     C                   eval      *in90 = *on
     C                   eval      *in25 = *on
     C                   endif                                                  srerr=*on
     C*Chain file WDF4P to look for duplicate records
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         ##efdt
     C*** Beg Chg ***** 01/25/11 ******************************************
     C*****              move      'PP3C'        ##key
      *** Begin Change *** 10/01/20 ***************************************
     C***                movel(p)  'PP3C'        ##key
      /Free
       ##key = scVerbType;
       erVerbType = *Off;
      /End-Free
      *** End   Change *** 10/01/20 ***************************************
     C*** End Chg ***** 01/25/11 ******************************************
     C*
     C     key1          chain     pf401                              60
     C  n90              dow       *in60 = *off
     C                   if        f4stat = 'A'
     C                   eval      *in90 = *on
     C                   eval      *in26 = *on
     C                   leave
     C                   endif                                                  f4stat= 'A'
     c     key1          reade     pf401                                  60
     C                   enddo
      /Free
       //*** Begin Add *** 10/01/20 **************************************
       If scVerbType = *blank;
          erVerbType = *On;
          *in90 = *On;
       Endif;
       //*** End   Add *** 10/01/20 **************************************

      /End-Free
     C                   endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C                   endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
     C                   z-add     f4efdt        srdti
     C                   call      'SRCVTDAT'    dtmdyy
     C                   movel     srdto         scefdt
     C*
      *** Begin Change *** 10/01/20 ***********************************
     C                   eval      sctxt1 =f4txt1
     C                   eval      sctxt2 =f4txt2
      /Free
       scVerbType = F4key;

       Select;
       When F4key = 'PP3C';
          scTxt1 = F4txt1;
          scTxt2 = F4txt2;
          visPP3C = *On;
          visBlWvDsc = *Off;
       Other;
          scBlWvDsc = F4vDesc;
          visBlWvDsc = *On;
          visPP3C = *Off;
       Endsl;

       erVerbType = *Off;

      /End-Free
      *** End Change *** 10/01/20 *************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
      *** Begin Change *** 10/01/20 ***********************************
     C***                   eval      f4txt1 =sctxt1
     C***                   eval      f4txt2 =sctxt2
      /Free
       F4key =  scVerbType ;

       Select;
       When scVerbType = 'PP3C';
           F4txt1 = scTxt1;
           F4txt2 = scTxt2;
       Other;
           F4vDesc = scBlWVDsc;
       Endsl;

       erVerbType = *Off;
      /End-Free
      *** End Change *** 10/01/20 *************************************
     C*
     C                   z-add     scefdt        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f4efdt
     C*
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
     C                   eval      *inlr = *on
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
     C*
     C                   if        @@mode = 'E'
     C                   eval      @@mode = 'X'
     C                   endif
     C*  GET GROUP INFORMATION
     C     @@RRN2        CHAIN     wmfnp                              60
     C*
     C                   z-add     fnfnd         scfnd
     C                   z-add     fnfnd         ##fnd#
     C                   movel     fnname        scname
      /Free
       //*** Begin Add *** 10/01/20 ***********************************
       Select;
       When @@mode = 'D';
          disVerbTyp = *On;
       Other;
          disVerbTyp = *Off;
       Endsl;
       erVerbType = *Off;
       //*** End Add   *** 10/01/20 ***********************************
      /End-Free
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C     @@rrn         chain     dbrcdf                             99
B01  C                   if        *in99 = *off
     C*
     C                   if        @@mode = 'X'
     C                   eval      f4stat= 'D'
     C                   movel     q1user        f4user
     C                   movel     q1user        f4iusr
     C                   movel     q1pgm         f4pgm
     C                   movel     q1pgm         f4ipgm
     C                   TIME                    DSDTTM
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f4dt
     C                   movel     srdto         f4idt
     C                   movel     dstime        f4time
     C                   movel     dstime        f4itim
     C*
     C                   update    dbrcdf
E01  C                   endif                                                  @@mode='X'
     C*
     C                   exsr      pgmend
     C                   return
E01  C                   endif                                                  *in99 = *off
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
     C                   clear                   dbrcdf
     C*
     C                   z-add     fnco#         f4co#
     C                   z-add     scfnd         f4fnd
     C                   move      'A'           f4stat
     C*** Beg Chg ***** 01/25/11 ******************************************
     C********           move      'PP3C'        f4key
      *** Beg Change *** 10/01/20 ******************************************
     C***                   movel(p)  'PP3C'        f4key
     C*** End Chg ***** 01/25/11 ******************************************
     C***                   eval      f4desc ='Policy Page 3c'
      /Free
       F4key = scVerbType;

       Select;
       When scVerbType = 'PP3C';
          F4desc = 'Policy Page 3c';
       Other;
          F4desc = *blanks;
       Endsl;
      /End-Free
      *** End Change *** 10/01/20 ******************************************
     C*
     C                   exsr      mvscdb
     C                   movel     q1user        f4user
     C                   movel     q1user        f4iusr
     C                   movel     q1pgm         f4pgm
     C                   movel     q1pgm         f4ipgm
     C                   TIME                    DSDTTM
     C                   z-add     dsdate        srdti
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         f4dt
     C                   movel     srdto         f4idt
     C                   movel     dstime        f4time
     C                   movel     dstime        f4itim
     C*
     C                   write     dbrcdf
     C                   feod      wdf4p
     C                   endsr
