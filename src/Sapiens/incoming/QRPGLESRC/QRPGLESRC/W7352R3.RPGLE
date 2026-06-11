     H* Copyright('(c) 2022 Sapiens. All Rights Reserved.')
     H/TITLE W7352R3 - WCSTAT Resubmit Data
     H option(*nodebugio: *srcstmt) bnddir('STBNDDIR')
     H**************************************************************************************
     H*  Description:  This program submits the "resubmission" process.                    *
     H*                                                                                    *
     H*  Called from: W7350R3                                                              *
     H*  Calls to: W7352C3                                                                 *
     H*                                                                                    *
     H*                                                                                    *
     H*  Created: 08/19/05    Programmer C:15    Project:WC591                             *
     H**************************************************************************************
     H*  Revisions:                                                                        *
     H*                                                                                    *
     H* 03/11/22 C:340 Project: WC2843                                                     *
     H*                Start using new Logical - WMTML1                                    *
     H*                Use Carrier specific Data provider and FEIN                         *
     H*                                                                                    *
     H* 12/03/15 C:65  Project: 45WKP                                                      *
     H*                Add checkbox to allow selection of pre-2016 data for LCTA which     *
     H*                uses old Carrier #.                                                 *
     H*                                                                                    *
     H*  05/11/09  C:61  Project: 93WHW                                                    *
     H*                  Added experience rating selection for clt093
     H*                                                                                    *
     H* 11/13/07 C:15  Project: WC1272                                                     *
     H*                Default SCR02 values.                                               *
     H*                                                                                    *
     H**************************************************************************************
     H*  Indicators:                                                                       *
     H*                                                                                    *
     H*  60 - General chain                                                                *
     H*  90 - Error on screen input                                                        *
     H*                                                                                    *
     H**************************************************************************************
      /IF DEFINED (WEB)
     H DFTACTGRP(*NO)
      /ENDIF
     FW7352F3   CF   E             WORKSTN
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
     F*
     FSSYLF     IF   E           K DISK
     FSMCOL     IF   E           K DISK
     FSTSTL3    IF   E           K DISK
     F*** Begin add *** 11/13/07 **************************************************
     F* WCSTAT Transmittal Information Default
     F*** Beg   add *** 03/11/22 ***********************************************************
     F*** wmtml     if   e           k disk
     Fwmtml1    if   e           k disk
     FWMSRP3L1  if   e           k disk    PreFix(S2:2)
     F*** End   add *** 03/11/22 ***********************************************************
     F*** End add ***** 11/13/07 **************************************************

     D*  Date/time data structure
     D                 DS
     D dsdttm                        12  0
     D  dstime                        6  0 Overlay(dsdttm)
     D  dsdate                        6  0 Overlay(dsdttm:7)

     D                 DS
     D dummy                         40
     D  dum40                         1    Overlay(dummy:40)
     D                 DS
     D xxphn                         10  0
     D  scphna                        3  0 Overlay(xxphn)
     D  scphn1                        3  0 Overlay(xxphn:4)
     D  scphn2                        4  0 Overlay(xxphn:7)

     D                 DS
     D xxfax                         10  0
     D  scfaxa                        3  0 Overlay(xxfax)
     D  scfax1                        3  0 Overlay(xxfax:4)
     D  scfax2                        4  0 Overlay(xxfax:7)

     D*** Begin add *** 11/13/07 **************************************************
     d                 ds
     d sczip                          9  0
     d  sczip1                        5  0 overlay(sczip:1)
     d  sczip2                        4  0 overlay(sczip:6)
     D*** End add ***** 11/13/07 **************************************************

     D                 DS
     D xxadr                        101
     D  xxstrt                       60    Overlay(xxadr)
     D  xxcity                       30    Overlay(xxadr:61)
     D  xxst                          2    Overlay(xxadr:91)
     D  xxzip1                        5    Overlay(xxadr:93)
     D  xxzip2                        4    Overlay(xxadr:98)

     D*  Key list field definitions
     D kkkey           S                   Like(STKEY)
     D qqstcd          S                   Like(STNST)
     D*** Begin add *** 03/11/22 ***********************************************************
     D kkCar#          S                   Like(S2Carr)
     D kkCo#           S                   Like(S2Co#)
     D*** End   add *** 03/11/22 ***********************************************************

     DPSDS           ESDS                  Extname(SZQ1P)

     d/copy sprUsrPrms

     C*---------------
     C*  Key lists
     C*---------------
     C*
     C*  State code table file
     C*
     C     stky          klist
     C                   kfld                    kkkey

     C*** Begin add *** 11/13/07 **************************************************
     C* WCSTAT Transmittal Information Default
     C*
     C     tmky          klist
     C                   kfld                    coco#
     C                   kfld                    kkflag            1
     C*** End add ***** 11/13/07 **************************************************

     C*
     C     *entry        plist
     C                   parm      *inkc         @@kc              1
     C                   parm      *inkl         @@kl              1
     C                   parm                    @@mode            1
     C                   parm                    @@rrn             9 0

     C*
     C*  Transfer to tape program parameter list
     C     tttpl         plist
     C                   parm                    qqstcd
     C                   parm                    qqmail           40
     C                   parm                    qqcnam           25
     C                   parm                    qqphn            10
     C                   parm                    qqext             6
     C                   parm                    qqfax            10
     C                   parm                    qqadr           101
     C*** BEG ADD *** 05/11/09 ***************************************
     C                   parm                    qqer              1
     C*** END ADD *** 05/11/09 ***************************************
      *** begin add ***  12/03/15  *************************************************
     C                   PARM                    QQOldCar          1
      *** end add *****  12/03/15  *************************************************
      *** Beg   add ***  03/11/22  *************************************************
     C                   PARM                    qqDtaPvdr         1
     C                   PARM                    qqFein            9
      *** End   add ***  03/11/22  *************************************************

     C*  Date conversion parms
     C*
     C     datchk        plist
     C                   parm                    srdat_in          8 0
     C                   parm      *blanks       srfr_fmt          8
     C                   parm      *blanks       srto_fmt          8
     C                   parm                    srdat_out        10
     C                   parm                    srerror           1
     C*
     C     dtmdyy        plist
     C                   parm                    srdat_in
     C                   parm      '*CYMD'       srfr_fmt
     C                   parm      *blanks       srto_fmt
     C                   parm                    srdat_out
     C                   parm                    srerror
     C*
     C     dtmdy         plist
     C                   parm                    srdat_in
     C                   parm      '*CYMD'       srfr_fmt
     C                   parm      '*MDY'        srto_fmt
     C                   parm                    srdat_out
     C                   parm                    srerror

     C*** BEG ADD *** 05/11/09 ***************************************
     C                   call      'RTVCLTID'
     C                   parm                    cltid             3
      *
     C     cltid         comp      '093'                                  93
     C*** END ADD *** 05/11/09 ***************************************
      *** begin add ***  12/03/15  *************************************************
     C                   Eval      clt045 = *off
     C
     C                   If        cltID = '045'
     C                   Eval      clt045 = *on
     C                   Endif
      *** end add *****  12/03/15  *************************************************
     C*
     C                   time                    dsdttm
     C*** Begin add *** 11/13/07 **************************************************
     C                   move      'Y'           default02         1
     c                   z-add     0             sczip1
     c                   z-add     0             sczip2
     C*** End add ***** 11/13/07 **************************************************
     C*
     C*  Get company name
     C*
     C                   read      PCO01                                  60
     C                   if        *in60 = *off
     C                   eval      srname = %TRIML(CONAME)
     C                   call      'SRCENTER'
     C                   parm                    srname           80
     C                   parm      40            srlen             3 0
     C*
     C                   movel     srname        SCCONM
     C*
     C                   else
     C*
     C                   eval       SCCONM = ' '
     C                   endif                                                  *in60=*off
     C*
     C*** Beg   add *** 03/11/22 ***********************************************************

            KKCo# = GetUserInfo(q1user);
            SetLL *LoVal PSR01;
            ReadE KKCo# PSR01;
            Dow Not %Eof(WMSRP3L1);
               If S2User = Q1User and S2Time = Q1Time ;
                  kkCar# = S2Carr;
               Leave;
               EndIf;
               ReadE KKCo# PSR01;
            EndDo;

     C*** End   add *** 03/11/22 ***********************************************************
     C*  Load user name
     C                   eval       SCUSER = q1user
     C*
     C*
     C*  Display 'Transfer to tape' prompt
     C                   clear                   scst
     C                   dou       *in90 = *off
     C*
     C                   setoff                                           90
     C                   exfmt     WDW02
     C*
     C                   if        *INKL = *on
     C                   return
     C                   endif
     C*
     C                   clear                   qqstcd
     C                   if        SCST <> '  '
     C*
     C                   movel(p)  SCST          kkkey
     C     stky          chain     PST01                              88
     C*
     C  n88              move      stnst         qqstcd
     C   88              seton                                            90
     C                   endif
      *
     C*** BEG ADD *** 05/11/09 ***************************************
     C  n90              if        (SCER<>' ') AND (SCER<>'1') AND (SCER<>'3')
     C                   seton                                        75  90
     C                   endif
     C*** END ADD *** 05/11/09 ***************************************
     C*
     C                   enddo                                                  *in90=*off
     C*

     C*--------------------
     C*  Main processing
     C*--------------------
     c                   z-add     0             xxphn
     c                   z-add     0             xxfax
     C                   if        SCST <> '  '
     C*
     C                   dou       *in90 = *off
     C*
     C*** Begin add *** 11/13/07 **************************************************
     c                   if        default02 = 'Y'
     c                   exsr      srdef02
     c                   endif
     C*** End add ***** 11/13/07 **************************************************
     C*
     C                   setoff                                           90
     C                   exfmt     SCR01
     C*
     C*  Process command keys
     C                   exsr      Func_keys
     C*
     C  N90              if        (SCMAIL = *blanks)
     C                   seton                                        33  90
     C                   endif
     C*
     C  N90              if        (sccnam = *blanks)
     C                   seton                                        34  90
     C                   endif
     C*
     C  N90              if        (scphna = *zero)
     C                   seton                                        35  90
     C                   endif
     C*
     C  N90              if        (scphn1 = *zero)
     C                   seton                                        36  90
     C                   endif
     C*
     C  N90              if        (scphn2 = *zero)
     C                   seton                                        37  90
     C                   endif
     C*
     C  N90              if        (scext < *zero)
     C                   seton                                        38  90
     C                   endif
     C*
     C  N90              if        (scfaxa = *zero)
     C                   seton                                        39  90
     C                   endif
     C*
     C  N90              if        (scfax1 = *zero)
     C                   seton                                        40  90
     C                   endif
     C*
     C  N90              if        (scfax2 = *zero)
     C                   seton                                        41  90
     C                   endif
     C*
     C* STREET NAME CANNOT BE BLANK.
     C*
     C*
     C  N90              if        (scstrt = *blanks)
     C                   seton                                        42  90
     C                   endif
     C*
     C* CITY NAME CANNOT BE BLANK.
     C*
     C  N90              if        (sccity = *blanks)
     C                   seton                                        43  90
     C                   endif
     C*
     C* STATE MUST BE VALID.
     C*
     C                   movel(p)  SCST2         kkkey
     C  N90stky          chain     PST01                              44
     C   44              seton                                            90
     C*
     C* ZIP CODE 1 CANNOT BE LESS THAN OR EQUAL TO ZERO.
     C*
     C  N90              if        (sczip1 <= *zero)
     C                   seton                                        45  90
     C                   endif
     C*
     C* ZIP CODE 2 CANNOT BE LESS THAN ZERO.
     C*
     C  N90              if        (sczip2 < *zero)
     C                   seton                                        46  90
     C                   endif
     C*
      *** Beg   add ***  03/11/22  *************************************************

       // If Data Provider is Third Party, then FEIN is required

          If *In90 = *Off and scDtaPvdr = 'T' and scTFien = 0;
             *In90 = *On;
             *In47 = *On;
          EndIf;

      *** Beg   add ***  03/11/22  *************************************************
     C                   enddo                                                  *in90=*off
     C*
     C                   endif
     C*
     c                   movel(p)  scmail        qqmail
     c                   movel(p)  sccnam        qqcnam
     c                   move(p)   xxphn         qqphn
     c                   move(p)   scext         qqext
     c                   move(p)   xxfax         qqfax
     c                   move      *blanks       xxadr
     C                   MOVE      SCSTRT        XXSTRT
     C                   MOVE      SCCITY        XXCITY
     C                   MOVE      SCST2         XXST
     C                   MOVE      SCZIP1        XXZIP1
     C                   MOVE      SCZIP2        XXZIP2
     C                   MOVE(p)   XXADR         QQADR
     C*** BEG ADD *** 05/11/09 ***************************************
     C                   eval       qqer = SCER
     C*** END ADD *** 05/11/09 ***************************************
     C*
     C*    Check parms for last char blank (parms longer than 30 char)
     C                   move(p)   qqmail        dummy
     C                   if        dum40 = ' '
     C                   move      '¢'           qqmail
     C                   endif
     C*
      *** begin add ***  12/03/15  *************************************************
     C                   Move      scOldCar      QQOldCar
      *** end add *****  12/03/15  *************************************************
      *** Beg   add ***  03/11/22  *************************************************
                         qqDtaPvdr = scDtaPvdr ;
                         qqFein = %Char(scTFien);
      *** End   add ***  03/11/22  *************************************************
     C*
     C*  Call W7352C3 to submit transfer process to batch
     C*
     C                   call      'W7352C3'     tttpl
     C*
     C                   exsr      Pgmend
     C*
     C                   eval      *inlr = *on
     C                   Return

     C*============================================================================
     C*  Func_keys - Subroutine to process function keys pressed
     C*============================================================================
     C     Func_keys     begsr
     C*
     C                   if        *in11 = *on
     C*
     C                   if        *inkc = *off and *inkl = *off
     C                   else                                                   f3,f12 pressed
     C                   Return
     C                   endif                                                  *inkc=*off
     C*
     C                   endif                                                  *in11=*on
     C*
     C                   endsr                                                  Func_keys
     C*============================================================================
     C*  Pgmend - subroutine to perform some end of program processes
     C*============================================================================
     C     Pgmend        begsr
     C*
     C                   endsr                                                  Pgmend
     C*
     C*** Begin add *** 11/13/07 **************************************************
      *============================================================================
      *  SRDEF02 - Default SCR02 Values
      *============================================================================
     c     SRDEF02       begsr
     c                   eval      default02 = 'N'
     c*** Beg   Change - 03/11/22   ****************************************************************
     c***                eval      kkflag = 'S'
     c***  tmky          chain     ptm01

     c***                if        %found
     c***                eval      scmail = tmmail
     c***                eval      sccnam = tmname
     c***                eval      xxphn  = tmphn#
     c***                eval      scext  = tmext#
     c***                eval      xxfax  = tmfax#
     c***                eval      scstrt = tmadr1
     c***                eval      sccity = tmcty
     c***                eval      scst2  = tmst
     c***                eval      sczip  = tmzip

           Chain(n) (KKCo# : kkCar#) WMTML1;
           If Not %Found(WMTML1);
              Chain(n) (KKCo# : 0) WMTML1;
           EndIf;
           If %Found(WMTML1);
              scmail    = tmSmail ;
              sccnam    = tmSname ;
              xxphn     = tmSphn# ;
              scext     = tmSext# ;
              xxfax     = tmSfax# ;
              scstrt    = tmSadr1 ;
              sccity    = tmScty  ;
              scst2     = tmSst   ;
              sczip     = tmSzip  ;
              scDtaPvdr = tmSDType;
              scTFien   = tmSFein ;

     c*** Beg   Change - 03/11/22   ****************************************************************
     c                   endif

     c                   endsr                                                  Pgmend
     C*** End add ***** 11/13/07 **************************************************
     C*============================================================================
